(ns bookmark2.test-main
  "clojure -M:test runs every test namespace; exit 1 on a failure."
  (:require [bookmark2.db-test]
            [bookmark2.json-test]
            [bookmark2.store-test]
            [bookmark2.ui-test]
            [clojure.test :as t]))

(defn -main [& _]
  (let [{:keys [fail error]} (t/run-tests 'bookmark2.db-test 'bookmark2.json-test 'bookmark2.store-test 'bookmark2.ui-test)]
    (shutdown-agents)
    (System/exit (if (zero? (+ fail error)) 0 1))))
