(ns bookmark2.log
  "Structured log: one JSON object per line, appended to

      $XDG_STATE_HOME/bookmark2/log.jsonl   (default ~/.local/state/bookmark2/log.jsonl)

  Each line carries ts and event, then the event's own fields:

      {\"ts\":\"2026-10-28T03:10:00Z\",\"event\":\"commit\",\"label\":\"Added x\",\"files\":[...]}

  Never log secrets."
  (:require [bookmark2.json :as json]
            [clojure.java.io :as io]))

(def ^:private lock (Object.))

(defn state-dir []
  (io/file (or (System/getenv "XDG_STATE_HOME")
               (str (System/getProperty "user.home") "/.local/state"))
           "bookmark2"))

(defn log-file []
  (io/file (state-dir) "log.jsonl"))

(defn event
  "Append one event. A log that cannot be written never breaks the app."
  [kind & {:as fields}]
  (let [line (json/write-str (merge {:ts (str (java.time.Instant/now)) :event (name kind)}
                                    fields))]
    (try
      (locking lock
        (io/make-parents (log-file))
        (spit (log-file) (str line "\n") :append true :encoding "UTF-8"))
      (catch Exception _ nil))))
