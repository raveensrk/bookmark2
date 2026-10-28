(ns bookmark2.store-test
  "Writing safely: the stale-file refusal, creating a file, undo."
  (:require [bookmark2.store :as store]
            [clojure.test :refer [deftest is testing]]))

(deftest normalize
  (is (= (store/normalize "http://www.Example.com/a/") (store/normalize "https://Example.com/a#frag"))))

(deftest commit-and-undo
  (let [f (doto (java.io.File/createTempFile "store" ".rec") .deleteOnExit)
        path (str f)
        _ (spit f "one\n")
        cs [[path "one\n" "two\n"]]]
    (store/commit! cs)
    (is (= "two\n" (slurp f)))
    (store/commit! (store/reverse-changes cs))
    (is (= "one\n" (slurp f)))
    (testing "a file changed since the plan: nothing written"
      (spit f "edited elsewhere\n")
      (is (= :stale (try (store/commit! cs) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))
      (is (= "edited elsewhere\n" (slurp f))))
    (testing "a plan from \"\" creates the file"
      (.delete f)
      (store/commit! [[path "" "new\n"]])
      (is (= "new\n" (slurp f))))))

(deftest first-write-creates-missing-folders
  ;; a new user's first add: ~/.local/share/bookmark2/ does not exist yet
  (let [root (.toFile (java.nio.file.Files/createTempDirectory "store" (make-array java.nio.file.attribute.FileAttribute 0)))
        path (str root "/not/there/yet/data.rec")]
    (store/commit! [[path "" "first\n"]])
    (is (= "first\n" (slurp path)))))
