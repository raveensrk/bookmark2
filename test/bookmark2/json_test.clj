(ns bookmark2.json-test
  (:require [bookmark2.json :as json]
            [clojure.test :refer [deftest is]]))

(deftest round-trip
  (let [v {"a" [1 2.5 "x\"y\n\u00e9" nil true false {}] "b" {"c" []}}]
    (is (= v (json/read-str (json/write-str v))))))

(deftest keywords-become-strings
  (is (= "{\"type\":\"choice\",\"n\":3}" (json/write-str {:type :choice :n 3}))))

(deftest unicode-escape
  (is (= "\u2014" (json/read-str "\"\\u2014\""))))

(deftest bad-input
  (is (thrown? clojure.lang.ExceptionInfo (json/read-str "{\"a\":}")))
  (is (thrown? clojure.lang.ExceptionInfo (json/read-str "[1,2] x"))))
