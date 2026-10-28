(ns bookmark2.db-test
  "data.rec on librec: tags, add, edit, read toggle, rename a subtree, create
  the file. Adds are compared with recins. Skipped without librec."
  (:require [bookmark2.db :as db]
            [bookmark2.rec :as rec]
            [bookmark2.store :as store]
            [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]))

(use-fixtures :once (fn [f]
                      (if (rec/available?)
                        (f)
                        (println "SKIP bookmark2.db-test: librec not found"))))

(def sample
  (str db/header
       "\nUrl: https://icestudio.io/\nTitle: Icestudio\nTag: Hardware/FPGA\nTag: To Read\nNote: visual editor\n"
       "\nUrl: https://nohello.net/en/\nTag: Questioning\n"
       "\nUrl: https://riscv.org/\nTitle: RISC-V\nTag: Hardware/RISC-V\n"))

(defn- base [] (db/parse "/d/data.rec" sample))
(defn- after [changes] (db/parse "/d/data.rec" (nth (first changes) 2)))
(defn- reason [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(deftest tags
  (is (= "Hardware/FPGA" (db/clean-tag " Hardware / FPGA/ ")))
  (is (nil? (db/clean-tag " / ")))
  (is (= ["AI" "AI/Agents" "AI/Agents/Skills"] (db/ancestors-of "AI/Agents/Skills")))
  (is (db/under? "Hardware/FPGA" "Hardware"))
  (is (not (db/under? "Hardwarex" "Hardware"))))

(deftest reading
  (let [ls (:links (base))]
    (is (= [["https://icestudio.io/" "Icestudio" ["Hardware/FPGA" "To Read"] "visual editor"]
            ["https://nohello.net/en/" nil ["Questioning"] nil]]
           (mapv (juxt :url :title :tags :note) (take 2 ls))))))

(deftest add-matches-recins
  (when (zero? (:exit (sh "sh" "-c" "command -v recins")))
    (let [f (doto (java.io.File/createTempFile "data" ".rec") .deleteOnExit)
          _ (spit f sample)
          ours (nth (first (db/plan-add (base) {:url "https://x.dev/" :title "X" :tags ["A/B" "To Read"]})) 2)]
      (sh "recins" "-t" "Link" "-f" "Url" "-v" "https://x.dev/" "-f" "Title" "-v" "X"
          "-f" "Tag" "-v" "A/B" "-f" "Tag" "-v" "To Read" (str f))
      (is (= (slurp f) ours)))))

(deftest add-rules
  (testing "no tags means To Read"
    (is (= ["To Read"] (:tags (last (:links (after (db/plan-add (base) {:url "https://y.dev/"}))))))))
  (testing "duplicates refused"
    (is (= :duplicate (reason #(db/plan-add (base) {:url "http://www.icestudio.io" :tags ["X"]})))))
  (testing "bad url"
    (is (= :bad-url (reason #(db/plan-add (base) {:url "ftp://x"}))))))

(deftest read-toggle
  (is (= ["Hardware/FPGA" "Read"] (db/toggle-read ["Hardware/FPGA" "To Read"])))
  (is (= ["To Read"] (db/toggle-read ["Read"])))
  (is (= ["Questioning" "To Read"] (db/toggle-read ["Questioning"])))
  (let [ice (first (:links (base)))]
    (is (= ["Hardware/FPGA" "Read"]
           (:tags (first (:links (after (db/plan-tags (base) ice (db/toggle-read (:tags ice)))))))))))

(deftest edit-in-place
  (let [out (after (db/plan-edit (base) (second (:links (base)))
                                 {:url "https://nohello.net/en/" :title "no hello" :tags ["Writing/Questioning"] :note ""}))]
    (is (= [nil "no hello" ["Writing/Questioning"]] ((juxt :note :title :tags) (second (:links out)))))))

(deftest rename-subtree
  (let [out (after (db/plan-rename-tag (base) "Hardware" "Tech/Hardware"))]
    (is (= [["Tech/Hardware/FPGA" "To Read"] ["Questioning"] ["Tech/Hardware/RISC-V"]] (mapv :tags (:links out)))))
  (is (= :no-tag (reason #(db/plan-rename-tag (base) "Nope" "X")))))

(deftest create-the-file
  (let [path (str (java.io.File/createTempFile "data" ".rec"))
        _ (.delete (java.io.File. path))]
    (store/commit! (db/plan-add (db/load-db path) {:url "https://first.dev/" :title "First"}))
    (is (str/starts-with? (slurp path) db/header))
    (is (= [["https://first.dev/" ["To Read"]]] (mapv (juxt :url :tags) (:links (db/load-db path)))))
    (.delete (java.io.File. path))))

(deftest fresh-start-and-default
  ;; a pretend home; skipped when the environment names the places itself
  (if (some #(System/getenv %) ["BOOKMARK2_DATA" "XDG_CONFIG_HOME" "XDG_DATA_HOME"])
    (println "SKIP fresh-start-and-default: BOOKMARK2_DATA or XDG_* is set")
    (let [old-home (System/getProperty "user.home")
          home (str (java.nio.file.Files/createTempDirectory "home" (make-array java.nio.file.attribute.FileAttribute 0)))]
      (try
        (System/setProperty "user.home" home)
        (testing "nothing named, nothing at the fallback: the window must ask"
          (is (db/needs-choice?))
          (is (= (str home "/.local/share/bookmark2/data.rec") (db/default-path))))
        (testing "a new file has the header and no links, and becomes the default"
          (let [path (str home "/links/data.rec")]
            (db/create-file! path)
            (is (= db/header (slurp path)))
            (is (= [] (:links (db/load-db path))))
            (doto (db/config-file) io/make-parents (spit "{:app-dir \"~/Apps\"}"))
            (db/set-default! path)
            (is (= {:app-dir "~/Apps" :data "~/links/data.rec"} (db/config)))
            (is (= path (db/default-path)))
            (is (not (db/needs-choice?)))))
        (testing "create-file! leaves an existing file alone"
          (let [path (str home "/links/data.rec")]
            (spit path (str db/header "\nUrl: https://kept.dev/\n"))
            (db/create-file! path)
            (is (= ["https://kept.dev/"] (mapv :url (:links (db/load-db path)))))))
        (testing "a blank file opens empty, and the first add writes the header"
          (let [path (str home "/blank.rec")]
            (spit path "\n")
            (is (= [] (:links (db/load-db path))))
            (store/commit! (db/plan-add (db/load-db path) {:url "https://first.dev/" :title "First"}))
            (is (str/starts-with? (slurp path) db/header))))
        (finally (System/setProperty "user.home" old-home))))))
