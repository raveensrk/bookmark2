(ns bookmark2.main
  "Entry point: parse the command line, then open the window, print or add.

  Help, `list`, `add` and `read` never load the window namespace, so they start
  fast and run without a display."
  (:gen-class)
  (:require [bookmark2.db :as db]
            [bookmark2.json :as json]
            [bookmark2.log :as log]
            [bookmark2.store :as store]
            [bookmark2.web :as web]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def exit-ok 0)
(def exit-refused 1)
(def exit-usage 2)

;; One table drives the short help, the long help and the parser.
(def options
  [{:long "--help" :short "-h" :desc "short help (this)"}
   {:long "--tag" :short "-t" :arg "TAG" :many? true :desc "add: tag the link (repeat for more); list: only this tag and below"}
   {:long "--json" :short "-j" :desc "list: JSON instead of lines"}
   {:long "--data" :short "-d" :arg "FILE" :desc "the database (default: BOOKMARK2_DATA, then :data in ~/.config/bookmark2/config.edn, then ~/.local/share/bookmark2/data.rec)"}
   {:long "--repl" :short "-r" :arg "PORT" :desc "socket REPL on 127.0.0.1:PORT, for debugging the window"}])

(def commands
  [["bookmark2" "open the window"]
   ["bookmark2 add URL [TITLE...] [-t TAG]..." "add a link (no -t: To Read; no title: fetched)"]
   ["bookmark2 list [-t TAG] [--json]" "print links, newest first"]
   ["bookmark2 read" "print the To Read links (list -t \"To Read\")"]
   ["bookmark2 help" "long help"]])

(defn- option-lines []
  (for [{:keys [long short arg desc]} options]
    (format "  %-22s %s" (str short ", " long (when arg (str " " arg))) desc)))

(defn short-help []
  (str/join "\n"
            (concat ["bookmark2 - your links in one file, data.rec, with a window and a command line."
                     ""
                     "Usage:"]
                    (for [[c d] commands] (format "  %-42s %s" c d))
                    ["" "Options:"]
                    (option-lines)
                    ["" "Example:" "  bookmark2 add https://icestudio.io -t Hardware/FPGA" ""])))

(defn long-help []
  (str (short-help)
       "
THE DATABASE
  One GNU recutils file, read and written through librec. A record per URL;
  Title and Note are optional, Tag repeats, and a / in a tag nests it:

    %rec: Link
    %key: Url

    Url: https://icestudio.io/
    Title: Icestudio
    Tag: Hardware/FPGA
    Tag: To Read
    Note: visual editor
    + each further line of a note starts with +

  The reading list is two tags, To Read and Read. Writes are the bytes recins
  would write, and every write checks the file still holds what was read; if
  another program changed it, nothing is written. recsel, recfix and a text
  editor all work on the file too.

THE WINDOW
  Left: every tag as a tree, with counts (a branch counts everything below it).
  Right: the links folded by tag, newest first; a link with two tags shows
  under both. Choosing a branch shows every link in and below it. The search box
  matches title, url, tags and note. Add, edit, delete, undo; r marks a link read
  or back to To Read; F2 renames a tag on every link, and a / in the new name
  moves it in the tree (FPGA -> Hardware/FPGA), with the tags below it.

KEYBOARD
  Everything works without a mouse. Tab cycles search, tags, links; each pane
  has single keys (j k g G e y a r h / ?); every action has a Cmd shortcut
  (Ctrl off macOS). ? or Help > Keyboard Shortcuts lists them all, and the
  status bar shows the keys for the focused pane.

FOLDING AND SORTING
  Space or za folds the tag under the cursor, zc / zo fold / unfold it, zM / zR
  fold / unfold all; a click on a header does the same. s cycles newest first,
  Title, URL, Tags; S flips the direction; a click on a column header sorts by
  it. To Read and Read always come first. Folds and the sort are remembered in
  folds.edn and view.edn next to the log; folds are ignored while you search.

THEMES
  View > Theme... (Cmd+T) lists every dark theme: FlatLaf's own and the
  IntelliJ theme pack's (Gruvbox, Nord, Dracula, One Dark, Solarized, Monokai
  Pro, Material, ...). Arrows preview each in the whole window, Enter keeps,
  Esc goes back. Cmd+] and Cmd+[ step to the next and previous theme. The
  choice is saved in view.edn. Tags are coloured by their top-level branch.

WHERE THE DATA LIVES
  --data FILE, else BOOKMARK2_DATA, else :data in ~/.config/bookmark2/config.edn,
  else ~/.local/share/bookmark2/data.rec (created on the first add).

  On a fresh start, with none of those set and no file at the last place, the
  window asks you to create a new data.rec or open an existing one, and makes
  it the default (:data in the config). File > Open Data File... shows another
  file in this window only; File > New Data File... creates one; File > Change
  Default Data File... picks the default. script/install --data FILE sets it
  too. The config is a plain EDN map:

    {:data \"~/notes/links.rec\" :app-dir \"~/Applications\"}

  examples/data.rec holds sample links: bookmark2 --data examples/data.rec

ENVIRONMENT
  BOOKMARK2_DATA     the database, like --data
  BOOKMARK2_LIBREC   path to librec (default: Homebrew, /usr/local, system paths)
  XDG_CONFIG_HOME    config location (default ~/.config)
  XDG_DATA_HOME      default database location (default ~/.local/share)
  XDG_STATE_HOME     log location (default ~/.local/state)

FILES
  reads, writes   the database (see WHERE THE DATA LIVES)
  reads, writes   ~/.config/bookmark2/config.edn (the window writes :data)
  writes          " (log/log-file) "   (one JSON object per line)
  writes          folds.edn and view.edn in the same folder

EXIT CODES
  0  ok, or the window closed
  1  add: refused, the link is already saved
  2  bad call
"))

(defn- fail-usage [msg]
  (binding [*out* *err*]
    (println (str "bookmark2: " msg))
    (println)
    (println (short-help)))
  (System/exit exit-usage))

(defn parse-args
  "{:command :window|:add|:list|:read|:help|:long-help, :words [...],
    :tag [...], :json bool, :data file, :repl port}"
  [args]
  (let [by-flag (into {} (mapcat (fn [o] [[(:long o) o] [(:short o) o]]) options))
        command-words {"help" :long-help "add" :add "list" :list "read" :read}]
    (loop [[a & more] args
           out {:command :window :words [] :tag []}]
      (cond
        (nil? a) out
        (and (= :window (:command out)) (command-words a)) (recur more (assoc out :command (command-words a)))
        (not (str/starts-with? a "-")) (recur more (update out :words conj a))

        :else
        (let [o (or (by-flag a) (fail-usage (str "unknown argument " a)))
              k (keyword (subs (:long o) 2))]
          (cond
            (= k :help) (recur more (assoc out :command :help))
            (:arg o) (do (when-not (first more) (fail-usage (str a " needs " (:arg o))))
                         (recur (rest more) (if (:many? o) (update out k conj (first more)) (assoc out k (first more)))))
            :else (recur more (assoc out k true))))))))

(defn- data-path
  "--data relative to where the launcher was called from (BOOKMARK2_CWD)."
  [data]
  (if (str/blank? data)
    (db/default-path)
    (let [f (io/file data)]
      (str (if (.isAbsolute f) f (io/file (or (System/getenv "BOOKMARK2_CWD") (System/getProperty "user.dir")) data))))))

(defn- print-links
  "list and read: links newest first, those under tag when given."
  [path tag as-json]
  (let [links (cond->> (reverse (:links (db/load-db path)))
                tag (filter (fn [l] (some #(db/under? % tag) (:tags l)))))]
    (if as-json
      (println (json/write-str (mapv #(select-keys % [:url :title :tags :note]) links)))
      (doseq [l links]
        (println (str (:url l) "\t" (or (:title l) "") "\t" (str/join ", " (:tags l))))))
    exit-ok))

(defn- add-command
  "bookmark2 add URL [TITLE...] [-t TAG]...: no tag means To Read; no title,
  the page's is fetched. Returns the exit code."
  [path [url & title-words] tags]
  (when-not url (fail-usage "add needs a URL"))
  (let [title (if (seq title-words) (str/join " " title-words) (web/fetch-title url))]
    (try
      (store/commit! (db/plan-add (db/load-db path) {:url url :title title :tags tags}))
      (log/event :add :url url :tags tags)
      (println (str "Added: " title "  [" (str/join ", " (if (seq tags) tags [db/to-read])) "]"))
      exit-ok
      (catch clojure.lang.ExceptionInfo e
        (binding [*out* *err*] (println (str "bookmark2: " (ex-message e))))
        (if (= :bad-url (:reason (ex-data e))) exit-usage exit-refused)))))

(defn -main [& args]
  (let [{:keys [command words tag json data repl]} (parse-args args)
        path (data-path data)
        done (fn [code] (shutdown-agents) (System/exit code))]
    (case command
      :help (println (short-help))
      :long-help (println (long-help))
      :list (done (print-links path (first tag) json))
      :read (done (print-links path db/to-read json))
      :add (done (add-command path words tag))
      :window
      (do
        (when (seq words) (fail-usage (str "unknown argument " (first words))))
        ;; macOS: name and dark title bar must be set before AWT starts
        (System/setProperty "apple.awt.application.name" "Bookmark2")
        (System/setProperty "apple.awt.application.appearance" "NSAppearanceNameDarkAqua")
        (System/setProperty "apple.laf.useScreenMenuBar" "true")
        (when repl
          (let [port (or (parse-long repl) (fail-usage (str "--repl needs a port number, not " repl)))]
            ((requiring-resolve 'clojure.core.server/start-server)
             {:name "bookmark2" :port port :address "127.0.0.1" :accept 'clojure.core.server/repl})
            (log/event :repl :port port)
            (binding [*out* *err*] (println (str "bookmark2: socket REPL on 127.0.0.1:" port)))))
        ;; a fresh start (no file named, none at the default place): the window asks
        ((requiring-resolve 'bookmark2.ui/start!)
         {:data-path (when-not (and (str/blank? data) (db/needs-choice?)) path)})))))
