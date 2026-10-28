(ns bookmark2.ui-test
  "The list's pure parts: grouping by tag, scope, sorting, folds. No window."
  (:require [bookmark2.ui :as ui]
            [clojure.test :refer [deftest is testing]]))

(def links
  ;; newest first, as visible hands them over
  [{:url "https://d.dev" :title "delta" :tags ["Hardware/FPGA" "To Read"]}
   {:url "https://c.dev" :title "Charlie" :tags ["Hardware/RISC-V"]}
   {:url "https://b.dev" :title "bravo" :tags ["To Read"]}
   {:url "https://a.dev" :title "Alpha" :tags []}])

(defn- order [scope sort]
  (mapv (fn [[tag ls]] [tag (mapv :title ls)]) (#'ui/ordered-groups links scope sort)))

(deftest groups
  (testing "To Read first, then A-Z, untagged last; a link shows under each tag"
    (is (= [["To Read" ["delta" "bravo"]] ["Hardware/FPGA" ["delta"]] ["Hardware/RISC-V" ["Charlie"]]
            ["(untagged)" ["Alpha"]]]
           (order nil {:key nil}))))
  (testing "a branch scope lists only the tags inside it"
    (is (= [["Hardware/FPGA" ["delta"]] ["Hardware/RISC-V" ["Charlie"]]]
           (order "Hardware" {:key nil})))))

(deftest sorting
  (is (= ["bravo" "delta"] (second (first (order nil {:key :title})))))
  (is (= ["delta" "bravo"] (second (first (order nil {:key :title :desc? true})))))
  (testing "Tags descending flips the groups"
    (is (= ["(untagged)" "Hardware/RISC-V" "Hardware/FPGA" "To Read"] (mapv first (order nil {:key :tags :desc? true}))))))

(deftest folds
  (let [rs (#'ui/rows {:visible links :folded #{"To Read"} :query "" :sort {:key nil} :scope nil})]
    (is (= [[true "To Read"] [true "Hardware/FPGA"] [nil "delta"]]
           (mapv (juxt :header? #(or (:title %) (:key %))) (take 3 rs)))))
  (testing "searching ignores folds"
    (is (some #(= "bravo" (:title %))
              (#'ui/rows {:visible links :folded #{"To Read"} :query "x" :sort {:key nil} :scope nil})))))
