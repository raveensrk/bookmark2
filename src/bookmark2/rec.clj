(ns bookmark2.rec
  "GNU librec, called through Java's FFM API: how data.rec is read and written.
  No binding library: the C functions are looked up and called directly.

  Every change parses the file's text, edits the database in memory, runs
  librec's integrity check and writes the database back to a string:

      text -> rec_parse_db -> rec_db_insert / rec_db_delete
           -> rec_int_check_db (refuse new errors) -> rec_write_db -> text

  The bytes are librec's own, the same file recins and recdel write, so a
  hand-formatted file is normalized on its first change (a blank line after a
  comment, for one).

  librec is GPL-3.0 and is loaded at run time, never bundled: from
  BOOKMARK2_LIBREC, else the usual install paths. Without it `available?` is
  false and the app cannot read data.rec.

  librec keeps global state and is not thread-safe, so every call holds `lock`.
  Each call has its own confined arena for the strings it passes in."
  (:require [bookmark2.log :as log]
            [clojure.string :as str])
  (:import (java.lang.foreign Arena FunctionDescriptor Linker Linker$Option MemoryLayout
                              MemorySegment SymbolLookup ValueLayout)
           (java.lang.invoke MethodHandle)))

;; The record set every call works on; bookmark2.db binds it to Link.
(def ^:dynamic *record-type* "Link")
(def candidates
  ["/opt/homebrew/lib/librec.dylib" "/usr/local/lib/librec.dylib"
   "/usr/lib/x86_64-linux-gnu/librec.so.1" "/usr/lib/aarch64-linux-gnu/librec.so.1"
   "/usr/lib/librec.so.1" "librec.so.1" "librec.dylib"])

;; values from rec.h
(def ^:private mset-record 1)
(def ^:private mset-field 1)
(def ^:private no-index -1)

(def ^:private A ValueLayout/ADDRESS)
(def ^:private I ValueLayout/JAVA_INT)
(def ^:private J ValueLayout/JAVA_LONG)
(def ^:private B ValueLayout/JAVA_BOOLEAN)
(def ^:private NULL MemorySegment/NULL)

(def ^:private signatures
  "C name -> [return & arguments]; nil return is void. size_t is JAVA_LONG."
  {"rec_init" [nil]
   "rec_parser_new_str" [A A A]
   "rec_parse_db" [B A A]
   "rec_parser_destroy" [nil A]
   "rec_writer_new_str" [A A A]
   "rec_write_db" [B A A]
   "rec_writer_destroy" [nil A]
   "rec_db_destroy" [nil A]
   "rec_db_get_rset_by_type" [A A A]
   "rec_rset_num_records" [J A]
   "rec_rset_mset" [A A]
   "rec_rset_descriptor" [A A]
   "rec_mset_get_at" [A A I J]
   "rec_mset_append" [A A I A I]
   "rec_record_new" [A]
   "rec_record_mset" [A A]
   "rec_record_num_fields" [J A]
   "rec_record_location" [J A]
   "rec_record_get_field_by_name" [A A A J]
   "rec_field_new" [A A A]
   "rec_field_name" [A A]
   "rec_field_value" [A A]
   "rec_db_insert" [B A A A A A J A A I]
   "rec_db_delete" [B A A A A A J I]
   "rec_buf_new" [A A A]
   "rec_buf_close" [nil A]
   "rec_int_check_db" [I A B B A]})

(def ^:private lock (Object.))

(defn- coerce [layout x]
  (condp = layout
    I (int x)
    J (long x)
    B (boolean x)
    x))

(defn- downcall [^Linker linker ^MemorySegment symbol [ret & args]]
  (let [layouts (into-array MemoryLayout args)
        desc (if ret (FunctionDescriptor/of ret layouts) (FunctionDescriptor/ofVoid layouts))
        ^MethodHandle mh (.downcallHandle linker symbol desc (make-array Linker$Option 0))]
    (fn [& xs] (.invokeWithArguments mh ^java.util.List (mapv coerce args xs)))))

(defn- open-library
  "The first librec that loads: BOOKMARK2_LIBREC, then the candidates."
  []
  (some (fn [name]
          (try
            [name (SymbolLookup/libraryLookup ^String name (Arena/global))]
            (catch Exception _ nil)))
        (cons (System/getenv "BOOKMARK2_LIBREC") candidates)))

(defonce ^:private api
  (delay
    (try
      (when-let [[path lib] (open-library)]
        (let [linker (Linker/nativeLinker)
              fns (into {} (for [[name sig] signatures]
                             [name (downcall linker (.orElseThrow (.find ^SymbolLookup lib name)) sig)]))
              free (downcall linker (.orElseThrow (.find (.defaultLookup linker) "free")) [nil A])]
          ((fns "rec_init"))
          (log/event :librec :path path)
          (assoc fns "free" free)))
      (catch Throwable e
        (log/event :librec-missing :message (str e))
        nil))))

(defn available? [] (some? @api))

(defn- c [name & args] (apply (@api name) args))

;; ---------------------------------------------------------------- helpers

(defn- cstr [^Arena arena s] (.allocateFrom arena ^String (str s)))

(defn- read-cstr [^MemorySegment p]
  (when-not (zero? (.address p))
    (.getString (.reinterpret p Long/MAX_VALUE) 0)))

(defn- take-cstr
  "Read a string librec malloc'ed into the pointer cell, then free it."
  [^MemorySegment cell]
  (let [p (.get cell ^java.lang.foreign.AddressLayout A 0)]
    (try (read-cstr p) (finally (when-not (zero? (.address p)) (c "free" p))))))

(defn- parse-db
  "A rec_db_t for the text; throws :rec-parse when librec cannot read it."
  [^Arena arena text source]
  (let [parser (c "rec_parser_new_str" (cstr arena text) (cstr arena source))
        cell (.allocate arena A)
        ok (c "rec_parse_db" parser cell)]
    (c "rec_parser_destroy" parser)
    (when-not ok
      (throw (ex-info (str source ": librec cannot parse it (run recfix on it for details).")
                      {:reason :rec-parse})))
    (.get cell ^java.lang.foreign.AddressLayout A 0)))

(defn- write-db [^Arena arena db]
  (let [str-cell (.allocate arena A)
        size-cell (.allocate arena J)
        writer (c "rec_writer_new_str" str-cell size-cell)]
    (c "rec_write_db" writer db)
    (c "rec_writer_destroy" writer)
    (take-cstr str-cell)))

(defn- integrity
  "[error-count messages] from rec_int_check_db."
  [^Arena arena db]
  (let [str-cell (.allocate arena A)
        size-cell (.allocate arena J)
        buf (c "rec_buf_new" str-cell size-cell)
        n (c "rec_int_check_db" db true false buf)]
    (c "rec_buf_close" buf)
    ;; a record not yet in the file has no location: "::  error: ..." -> "error: ..."
    [n (str/trim (str/replace (or (take-cstr str-cell) "") #"(?m)^:+\s*" ""))]))

(defn- with-db
  "(f arena db) under the lock, the db freed afterwards."
  [text source f]
  (when-not (available?)
    (throw (ex-info "librec is not installed, so .rec files are read-only here. Install recutils."
                    {:reason :no-librec})))
  (locking lock
    (with-open [arena (Arena/ofConfined)]
      (let [db (parse-db arena text source)]
        (try (f arena db) (finally (c "rec_db_destroy" db)))))))

(defn- fields-of [record]
  (let [mset (c "rec_record_mset" record)]
    (vec (for [j (range (c "rec_record_num_fields" record))
               :let [field (c "rec_mset_get_at" mset mset-field j)]]
           [(read-cstr (c "rec_field_name" field)) (read-cstr (c "rec_field_value" field))]))))

(defn- index-cell
  "Pairs of Min,Max ending in NOINDEX,NOINDEX: [i i -1 -1] selects record i."
  [^Arena arena i]
  (let [seg (.allocate arena (* 4 Long/BYTES))]
    (doseq [[k v] (map-indexed vector [i i no-index no-index])]
      (.setAtIndex seg J (long k) (long v)))
    seg))

(defn- new-record [^Arena arena fields]
  (let [record (c "rec_record_new")
        mset (c "rec_record_mset" record)]
    (doseq [[k v] fields]
      (c "rec_mset_append" mset mset-field (c "rec_field_new" (cstr arena k) (cstr arena v)) 0))
    record))

;; ---------------------------------------------------------------- reading

(defn read-text
  "{:doc :records [{:index :line :fields [[name value] ...]}]} for the
  *record-type* record set, or nil when the text has none."
  [text source]
  (with-db text source
    (fn [arena db]
      (let [rset (c "rec_db_get_rset_by_type" db (cstr arena *record-type*))]
        (when-not (zero? (.address ^MemorySegment rset))
          (let [descriptor (c "rec_rset_descriptor" rset)
                doc (when-not (zero? (.address ^MemorySegment descriptor))
                      (some-> (c "rec_record_get_field_by_name" descriptor (cstr arena "%doc") 0)
                              (#(when-not (zero? (.address ^MemorySegment %)) (read-cstr (c "rec_field_value" %))))))
                mset (c "rec_rset_mset" rset)]
            {:doc doc
             :records (vec (for [i (range (c "rec_rset_num_records" rset))
                                 :let [record (c "rec_mset_get_at" mset mset-record i)]]
                             {:index i
                              :line (c "rec_record_location" record)
                              :fields (fields-of record)}))}))))))

;; ---------------------------------------------------------------- changing

(defn- change
  "Run (mutate arena db) on the parsed text and return the written text.
  Refuses a change that leaves more integrity errors than the file had."
  [text source mutate]
  (with-db text source
    (fn [arena db]
      (let [[before _] (integrity arena db)]
        (when-not (mutate arena db)
          (throw (ex-info (str source ": librec refused the change.") {:reason :rec-refused})))
        (let [[after messages] (integrity arena db)]
          (when (> after before)
            (throw (ex-info (str "librec integrity check failed:\n" messages)
                            {:reason :rec-integrity :messages messages})))
          (write-db arena db))))))

(defn add-record
  "The text with a new *record-type* record appended."
  [text source fields]
  (change text source
          (fn [arena db]
            (c "rec_db_insert" db (cstr arena *record-type*) NULL NULL NULL 0 NULL (new-record arena fields) 0))))

(defn replace-record
  "The text with *record-type* record i replaced, in place."
  [text source i fields]
  (change text source
          (fn [arena db]
            (c "rec_db_insert" db (cstr arena *record-type*) (index-cell arena i) NULL NULL 0 NULL
               (new-record arena fields) 0))))

(defn remove-record
  "The text without *record-type* record i."
  [text source i]
  (change text source
          (fn [arena db]
            (c "rec_db_delete" db (cstr arena *record-type*) (index-cell arena i) NULL NULL 0 0))))
