(ns bookmark2.json
  "Minimal JSON: enough for `list --json` and the
  log lines. Written here because FlatLaf is the app's only library.

    (write-str {:a [1 \"x\" nil]})  => \"{\\\"a\\\":[1,\\\"x\\\",null]}\"
    (read-str \"{\\\"a\\\":[1,2.5]}\") => {\"a\" [1 2.5]}

  Objects read back as maps with string keys.")

(defn- write-string [^StringBuilder sb ^String s]
  (.append sb \")
  (doseq [c s]
    (case c
      \" (.append sb "\\\"")
      \\ (.append sb "\\\\")
      \newline (.append sb "\\n")
      \return (.append sb "\\r")
      \tab (.append sb "\\t")
      (if (< (int c) 0x20)
        (.append sb (format "\\u%04x" (int c)))
        (.append sb c))))
  (.append sb \"))

(defn- write-value [^StringBuilder sb v]
  (cond
    (nil? v) (.append sb "null")
    (boolean? v) (.append sb (str v))
    (integer? v) (.append sb (str v))
    (number? v) (.append sb (str (double v)))
    (keyword? v) (write-string sb (name v))
    (string? v) (write-string sb v)

    (map? v)
    (do (.append sb \{)
        (doseq [[i [k x]] (map-indexed vector v)]
          (when (pos? i) (.append sb \,))
          (write-string sb (if (keyword? k) (name k) (str k)))
          (.append sb \:)
          (write-value sb x))
        (.append sb \}))

    (sequential? v)
    (do (.append sb \[)
        (doseq [[i x] (map-indexed vector v)]
          (when (pos? i) (.append sb \,))
          (write-value sb x))
        (.append sb \]))

    :else (write-string sb (str v))))

(defn write-str [v]
  (let [sb (StringBuilder.)]
    (write-value sb v)
    (str sb)))

(defn read-str
  "Parse one JSON value. Throws ex-info naming the offset on bad input."
  [^String s]
  (let [i (volatile! 0)
        n (.length s)]
    (letfn [(fail [msg] (throw (ex-info (str "json: " msg " at " @i) {:at @i})))
            (ch [] (if (< @i n) (.charAt s @i) (fail "unexpected end")))
            (skip-ws [] (while (and (< @i n) (Character/isWhitespace (.charAt s @i)))
                          (vswap! i inc)))
            (literal [^String word value]
              (when-not (.startsWith s word (int @i)) (fail (str "expected " word)))
              (vswap! i + (.length word))
              value)

            (read-string* []
              (vswap! i inc)
              (let [sb (StringBuilder.)]
                (loop []
                  (let [c (ch)]
                    (vswap! i inc)
                    (cond
                      (= c \") (str sb)

                      (= c \\)
                      (let [e (ch)]
                        (vswap! i inc)
                        (case e
                          \" (.append sb \")
                          \\ (.append sb \\)
                          \/ (.append sb \/)
                          \b (.append sb \backspace)
                          \f (.append sb \formfeed)
                          \n (.append sb \newline)
                          \r (.append sb \return)
                          \t (.append sb \tab)
                          \u (do (when (> (+ @i 4) n) (fail "short \\u escape"))
                                 (.append sb (char (Integer/parseInt (subs s @i (+ @i 4)) 16)))
                                 (vswap! i + 4))
                          (fail (str "bad escape " e)))
                        (recur))

                      :else (do (.append sb c) (recur)))))))

            (read-number []
              (let [start @i]
                (while (and (< @i n)
                            (let [c (.charAt s @i)]
                              (or (Character/isDigit c) (#{\- \+ \. \e \E} c))))
                  (vswap! i inc))
                (let [t (subs s start @i)]
                  (when (empty? t) (fail "expected a value"))
                  (if (re-find #"[.eE]" t) (Double/parseDouble t) (Long/parseLong t)))))

            (read-array []
              (vswap! i inc)
              (skip-ws)
              (if (= (ch) \])
                (do (vswap! i inc) [])
                (loop [out []]
                  (let [out (conj out (read-value))]
                    (skip-ws)
                    (let [c (ch)]
                      (vswap! i inc)
                      (case c
                        \, (recur out)
                        \] out
                        (fail "expected , or ]")))))))

            (read-object []
              (vswap! i inc)
              (skip-ws)
              (if (= (ch) \})
                (do (vswap! i inc) {})
                (loop [m {}]
                  (skip-ws)
                  (when-not (= (ch) \") (fail "expected a key"))
                  (let [k (read-string*)]
                    (skip-ws)
                    (when-not (= (ch) \:) (fail "expected :"))
                    (vswap! i inc)
                    (let [m (assoc m k (read-value))]
                      (skip-ws)
                      (let [c (ch)]
                        (vswap! i inc)
                        (case c
                          \, (recur m)
                          \} m
                          (fail "expected , or }"))))))))

            (read-value []
              (skip-ws)
              (case (ch)
                \{ (read-object)
                \[ (read-array)
                \" (read-string*)
                \t (literal "true" true)
                \f (literal "false" false)
                \n (literal "null" nil)
                (read-number)))]

      (let [v (read-value)]
        (skip-ws)
        (when (< @i n) (fail "trailing data"))
        v))))
