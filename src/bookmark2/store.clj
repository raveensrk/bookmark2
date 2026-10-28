(ns bookmark2.store
  "Writing data.rec safely, and the small helpers around it.

      plan    [[path old-text new-text] ...]   pure data, made by bookmark2.db
      commit! checks every old-text against disk, then writes; nothing when a
              file changed since it was read
      undo    (commit! (reverse-changes plan))

  Writes go through a temp file and a rename, so a crash never leaves half a file."
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.io File)
           (java.nio.file CopyOption Files StandardCopyOption)
           (java.nio.charset StandardCharsets)))

(defn fail
  "Throw a refusal the window can act on: reason is a keyword, e.g. :duplicate."
  [reason msg & {:as data}]
  (throw (ex-info msg (merge {:reason reason} data))))

(defn home [] (System/getProperty "user.home"))

(defn expand-home
  "~/notes/links.rec -> /home/me/notes/links.rec; other paths unchanged."
  [p]
  (let [p (str p)]
    (if (or (= p "~") (str/starts-with? p "~/"))
      (str (home) (subs p 1))
      p)))

(defn short-path
  "/home/me/notes/links.rec -> ~/notes/links.rec"
  [path]
  (let [h (home)
        p (str path)]
    (if (str/starts-with? p (str h File/separator))
      (str "~" (subs p (count h)))
      p)))

(defn clean-title
  "A title on one line: whitespace runs become one space."
  [title]
  (str/trim (str/replace (str title) #"\s+" " ")))

(defn normalize
  "The duplicate key: no scheme, no leading www., no trailing slash, no #fragment."
  [url]
  (-> (str/trim (str url))
      (str/replace #"/+$" "")
      (str/replace #"^https?://" "")
      (str/replace #"(?i)^www\." "")
      (str/split #"#" 2)
      first))

(defn read-text [path]
  (String. (Files/readAllBytes (.toPath (io/file (str path)))) StandardCharsets/UTF_8))

(defn reverse-changes
  "The changes that undo these."
  [changes]
  (mapv (fn [[path old new]] [path new old]) (reverse changes)))

(defn- write-atomic
  "Write through a temp file in the same folder, then rename over the target.
  Missing folders are created first: a new user's first add makes
  ~/.local/share/bookmark2/."
  [path ^String text]
  (let [target (.toPath (io/file (str path)))
        tmp (.resolveSibling target (str "." (.getFileName target) ".bookmark2.tmp"))]
    (io/make-parents (io/file (str path)))
    (Files/write tmp (.getBytes text StandardCharsets/UTF_8) (make-array java.nio.file.OpenOption 0))
    (Files/move tmp target (into-array CopyOption [StandardCopyOption/REPLACE_EXISTING
                                                   StandardCopyOption/ATOMIC_MOVE]))))

(defn commit!
  "Write the planned changes. Every file is checked first: when one no longer
  holds the text the plan was made from, nothing is written. A plan made from
  \"\" may create its file."
  [changes]
  (doseq [[path old _] changes]
    (when-not (= old (if (.exists (io/file (str path))) (read-text path) ""))
      (fail :stale (str (short-path path) " changed on disk. Nothing was written; try again."))))
  (doseq [[path _ new] changes]
    (write-atomic path new))
  changes)
