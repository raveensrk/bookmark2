(ns bookmark2.os
  "Hand a file to the desktop: the default text editor, the file manager.

    macOS     open -t FILE   (the default text editor)   open -R FILE
    Windows   Desktop edit   (the file's edit verb)      explorer /select,FILE
    Linux     xdg-open FILE  (default app for the type)  xdg-open FOLDER"
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.awt Desktop)))

(defn- os-name [] (str/lower-case (System/getProperty "os.name")))
(defn mac? [] (str/starts-with? (os-name) "mac"))
(defn windows? [] (str/starts-with? (os-name) "windows"))

(defn- run-cmd!
  "Run cmd and wait. With check?, a non-zero exit throws with its output."
  [cmd check?]
  (let [proc (-> (ProcessBuilder. ^java.util.List cmd) (.redirectErrorStream true) .start)
        out (slurp (.getInputStream proc))
        code (.waitFor proc)]
    (when (and check? (not (zero? code)))
      (throw (ex-info (str (first cmd) " failed: " (str/trim out)) {:reason :os :exit code})))))

(defn open-in-editor! [path]
  (cond
    (mac?) (run-cmd! ["open" "-t" (str path)] true)
    (windows?) (.edit (Desktop/getDesktop) (io/file (str path)))
    :else (run-cmd! ["xdg-open" (str path)] true)))

(defn reveal!
  "Show the file in Finder, Explorer or the file manager."
  [path]
  (cond
    (mac?) (run-cmd! ["open" "-R" (str path)] true)
    ;; explorer exits 1 even when it worked
    (windows?) (run-cmd! ["explorer" (str "/select," path)] false)
    :else (run-cmd! ["xdg-open" (str (.getParentFile (io/file (str path))))] true)))
