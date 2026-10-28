(ns bookmark2.db
  "data.rec, the one database: a Link record per URL, organized by tags.

      %rec: Link
      %doc: every link, bookmarks and the reading list, organized by tags
      %key: Url

      Url: https://icestudio.io/
      Title: Icestudio                  <- optional; the window shows the url
      Tag: Hardware/FPGA                <- any number; / nests a tag in a tree
      Tag: To Read                      <- the reading list is two tags
      Note: visual editor               <- optional

  The file keeps records in the order they arrived, appended the way recins
  appends; the window shows the newest first. Every change is a plan,
  [[path old new]], written by store/commit! (refused when the file changed on
  disk) and undone by reversing it. The bytes are librec's (bookmark2.rec)."
  (:require [bookmark2.rec :as rec]
            [bookmark2.store :as store]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def record-type "Link")
(def to-read "To Read")
(def done "Read")

(def header
  "The descriptor a new data.rec starts with."
  (str "%rec: Link\n"
       "%doc: every link, bookmarks and the reading list, organized by tags (a / nests them)\n"
       "%key: Url\n"))

(defn config-file
  "$XDG_CONFIG_HOME/bookmark2/config.edn (default ~/.config/bookmark2/config.edn)."
  []
  (io/file (or (not-empty (System/getenv "XDG_CONFIG_HOME")) (str (store/home) "/.config"))
           "bookmark2" "config.edn"))

(defn config
  "The config map, {} when the file is missing or unreadable:

      {:data    \"~/notes/links.rec\"            ; the database
       :app-dir \"~/Applications\"}              ; where script/build_app installs"
  []
  (try
    (let [c (edn/read-string (slurp (config-file)))]
      (if (map? c) c {}))
    (catch Exception _ {})))

(defn default-path
  "BOOKMARK2_DATA, else :data in the config file, else
  $XDG_DATA_HOME/bookmark2/data.rec (default ~/.local/share/bookmark2/data.rec)."
  []
  (store/expand-home
   (or (not-empty (System/getenv "BOOKMARK2_DATA"))
       (not-empty (str (:data (config))))
       (str (or (not-empty (System/getenv "XDG_DATA_HOME")) (str (store/home) "/.local/share"))
            "/bookmark2/data.rec"))))

;; ---------------------------------------------------------------- tags

(defn clean-tag
  "One tag, tidied: \" Hardware / FPGA/ \" -> \"Hardware/FPGA\"; blank -> nil."
  [tag]
  (let [parts (->> (str/split (str tag) #"/") (map str/trim) (remove str/blank?))]
    (when (seq parts) (str/join "/" parts))))

(defn clean-tags [tags]
  (vec (distinct (keep clean-tag tags))))

(defn under?
  "Tag t is prefix p or below it: (under? \"Hardware/FPGA\" \"Hardware\") => true."
  [t p]
  (or (= t p) (str/starts-with? t (str p "/"))))

(defn ancestors-of
  "\"AI/Agents/Skills\" -> [\"AI\" \"AI/Agents\" \"AI/Agents/Skills\"]"
  [tag]
  (let [parts (str/split tag #"/")]
    (mapv #(str/join "/" (take % parts)) (range 1 (inc (count parts))))))

;; ---------------------------------------------------------------- reading

(defn parse
  "{:path :short :text :links} for the file's text, or nil when it has no
  Link set. Links are in file order, oldest first."
  [path text]
  (binding [rec/*record-type* record-type]
    (when-let [{:keys [records]} (rec/read-text text (store/short-path path))]
      (let [short (store/short-path path)
            values (fn [fields k] (keep (fn [[n v]] (when (= n k) v)) fields))]
        {:path (str path)
         :short short
         :text text
         :links (vec (for [{:keys [index line fields]} records
                           :let [url (first (values fields "Url"))]
                           :when url]
                       {:index index :line line :path (str path) :short short
                        :url url
                        :title (first (values fields "Title"))
                        :tags (vec (values fields "Tag"))
                        :note (first (values fields "Note"))}))}))))

(defn load-db
  "The database at path, or an empty one when the file does not exist yet."
  [path]
  (if (.exists (io/file (str path)))
    (or (parse path (store/read-text path))
        (store/fail :rec-parse (str (store/short-path path) " has no %rec: Link set.")))
    {:path (str path) :short (store/short-path path) :text "" :links []}))

(defn find-duplicate [db url & {:keys [except]}]
  (let [want (store/normalize url)]
    (first (filter #(and (= want (store/normalize (:url %))) (not= except (:index %)))
                   (:links db)))))

;; ---------------------------------------------------------------- planning

(defn- fields
  "A Link record's fields, in this order: Url Title Tag... Note."
  [{:keys [url title tags note]}]
  (vec (concat [["Url" url]]
               (when-not (str/blank? title) [["Title" (store/clean-title title)]])
               (map (fn [t] ["Tag" t]) (clean-tags tags))
               (when-not (str/blank? note) [["Note" (str/trim (str/replace note #"\s+" " "))]]))))

(defn- check-url [url]
  (when-not (re-matches #"https?://\S+" url)
    (store/fail :bad-url (str "Not an http(s) url: " (pr-str url)))))

(defn- change
  "[[path old new]] with (f text) run under the Link record type, starting
  from the header when the file is new."
  [db f]
  [[(:path db) (:text db)
    (binding [rec/*record-type* record-type]
      (f (if (str/blank? (:text db)) header (:text db))))]])

(defn plan-add
  "Add a link. No tags means To Read, unless as-is? (the merge keeps an
  untagged link untagged)."
  [db {:keys [url tags force? as-is?] :as link}]
  (let [url (str/trim (str url))
        _ (check-url url)
        tags (clean-tags tags)
        dup (find-duplicate db url)]
    (when (and dup (not force?))
      (store/fail :duplicate (str "Already saved: " (or (:title dup) (:url dup))
                                 (when (seq (:tags dup)) (str "  [" (str/join ", " (:tags dup)) "]")))
                 :entry dup))
    (change db #(rec/add-record % (:short db)
                                (fields (assoc link :url url :tags (if (or (seq tags) as-is?) tags [to-read])))))))

(defn plan-edit
  "Replace link's url, title, tags and note, in place."
  [db link {:keys [url] :as new}]
  (let [url (str/trim (str url))]
    (check-url url)
    (when-let [dup (find-duplicate db url :except (:index link))]
      (store/fail :duplicate (str "Already saved: " (or (:title dup) (:url dup))) :entry dup))
    (change db #(rec/replace-record % (:short db) (:index link) (fields (assoc new :url url))))))

(defn plan-tags
  "Give link these tags, in place."
  [db link tags]
  (change db #(rec/replace-record % (:short db) (:index link) (fields (assoc link :tags tags)))))

(defn toggle-read
  "The tags with To Read and Read swapped; neither -> To Read added."
  [tags]
  (cond
    (some #{to-read} tags) (mapv #(if (= % to-read) done %) tags)
    (some #{done} tags) (mapv #(if (= % done) to-read %) tags)
    :else (conj (vec tags) to-read)))

(defn plan-remove [db link]
  (change db #(rec/remove-record % (:short db) (:index link))))

(defn rename-in
  "tags with from (and everything below it) renamed to to."
  [tags from to]
  (clean-tags (map #(if (under? % from) (str to (subs % (count from))) %) tags)))

(defn plan-rename-tag
  "Rename tag from, and every tag below it, to to, on every link. One change.
  Renaming FPGA to Hardware/FPGA moves it into the Hardware branch."
  [db from to]
  (let [from (clean-tag from)
        to (clean-tag to)
        hit (filter (fn [l] (some #(under? % from) (:tags l))) (:links db))]
    (when (or (nil? from) (nil? to))
      (store/fail :bad-tag "A tag cannot be blank."))
    (when (empty? hit)
      (store/fail :no-tag (str "No link has the tag " from ".")))
    (change db (fn [text]
                 (reduce (fn [t l] (rec/replace-record t (:short db) (:index l)
                                                       (fields (update l :tags rename-in from to))))
                         text hit)))))
