(ns bookmark2.ui-test
  "The window's pure parts: grouping, sorting, folds, urls, drops, keys. No window."
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

(deftest tag-completion
  (let [known ["Hardware" "Hardware/FPGA" "Hardware/RISC-V" "Read" "To Read" "Writing/Questioning"]]
    (testing "prefix first, then a part's prefix, then anywhere"
      (is (= ["Read" "To Read"] (#'ui/tag-completions known "rea" [])))
      (is (= ["Hardware/FPGA"] (#'ui/tag-completions known "fp" [])))
      (is (= ["Read" "Hardware/RISC-V" "Hardware" "Hardware/FPGA" "To Read" "Writing/Questioning"]
             (#'ui/tag-completions known "r" [])))
      (is (= ["Writing/Questioning"] (#'ui/tag-completions known "estion" []))))
    (testing "tags already in the field are left out; blank offers the rest"
      (is (= ["To Read"] (#'ui/tag-completions known "rea" ["read"])))
      (is (= 5 (count (#'ui/tag-completions known " " ["Read"]))))))
  (testing "the segment under the caret"
    (is (= [0 8] (#'ui/tag-segment "Hardware" 3)))
    (is (= [9 12] (#'ui/tag-segment "Hardware, FP" 12)))
    (is (= [0 0] (#'ui/tag-segment "" 0)))
    (is (= [9 12] (#'ui/tag-segment "Hardware, FP, Read" 11)))
    (is (= [9 9] (#'ui/tag-segment "Hardware," 9)))))

(deftest url-highlight
  (is (= ["https://" "www.x.org" "/a/b?q=1"] (ui/url-parts "https://www.x.org/a/b?q=1")))
  (is (= ["https://" "x.org" ""] (ui/url-parts "https://x.org")))
  (is (= ["" "not a url" ""] (ui/url-parts "not a url"))))

(deftest dropping
  (testing "a url out of dropped or pasted text"
    (is (= "https://a.dev/x" (ui/url-in "see https://a.dev/x now")))
    (is (nil? (ui/url-in "no link here"))))
  (testing "a tag dragged onto another moves under it; onto All, to the top"
    (is (= "Hardware/FPGA" (ui/moved-tag "FPGA" "Hardware")))
    (is (= "FPGA" (ui/moved-tag "Hardware/FPGA" nil)))
    (is (nil? (ui/moved-tag "Hardware/FPGA" "Hardware")) "already there")
    (is (nil? (ui/moved-tag "Hardware" "Hardware/FPGA")) "not into itself")
    (is (nil? (ui/moved-tag "Hardware" "Hardware")))))

(deftest key-bindings
  (testing "how a key reads"
    (is (= "j" (ui/key-text "typed j")))
    (is (= "Cmd+N" (ui/key-text "meta pressed N")))
    (is (= "Cmd+Shift+=" (ui/key-text "shift meta pressed EQUALS")))
    (is (= "Shift+F10" (ui/key-text "shift F10")))
    (is (= "Page Down" (ui/key-text "PAGE_DOWN")))
    (is (= "za" (ui/key-text "za"))))
  (testing "a menu shortcut needs a modifier or a function key"
    (is (ui/shortcut-ok? "meta pressed L"))
    (is (ui/shortcut-ok? "F2"))
    (is (not (ui/shortcut-ok? "typed l")))
    (is (not (ui/shortcut-ok? "ENTER"))))
  (testing "clashes: the same pane, or a menu shortcut anywhere"
    (let [entries [{:scope :list :id :next :keys ["DOWN" "typed j"]}
                   {:scope :tree :id :next :keys ["DOWN" "typed j"]}
                   {:scope :app :id :add :keys ["meta pressed N"]}]]
      (is (= :next (:id (ui/key-conflict entries :list :prev "typed j"))))
      (is (nil? (ui/key-conflict entries :list :next "typed j")) "its own key")
      (is (= :add (:id (ui/key-conflict entries :list :tag "meta N"))))
      (is (nil? (ui/key-conflict entries :search :open "typed j")) "another pane"))))

(deftest text-size
  (is (= 10 (ui/clamp-font-size 3)))
  (is (= 40 (ui/clamp-font-size 99)))
  (is (= 20 (ui/clamp-font-size 20))))
