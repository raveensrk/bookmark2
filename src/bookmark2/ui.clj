(ns bookmark2.ui
  "The window over data.rec: Swing widgets in FlatLaf's dark look.

    +---------------------------------------------------------------+
    | [Q search title, url, tag, note ...      ]   [Add Link] [Reload] |
    +------------------+--------------------------------------------+
    | All  301         | ▾ Hardware/FPGA  1                         |
    |  ▾ AI  19        |       Icestudio      https://icestudio.io/ |
    |     Agents 10    | ▾ To Read  182                             |
    |  To Read 182     |       Choose Boring Technology  ...        |
    +------------------+--------------------------------------------+
    | 301 links, 182 to read, 6 read        [busy]   keys for this pane |
    +---------------------------------------------------------------+

  Left: the tags as a tree (a / in a tag nests it). Right: the links folded by
  tag, newest first; a link with two tags shows under both. Choosing a branch
  shows every link in it and below it. Links can be selected several at once
  and dragged onto a tag; a tag dragged onto another moves under it.

  All state lives in the `state` atom and the widgets in `widgets`. Widget
  calls run on the Swing event thread; loading, page fetches and opening links
  run on futures and hand their result back through `on-edt`.

  Everything works from the keyboard: Tab cycles search -> tags -> links, each
  pane has single keys (`pane-keys`), every menu action has a shortcut
  (`app-actions`), and Settings > Keyboard changes any of them. The text
  size (default 20 pt) scales the whole window: FlatLaf derives every font,
  padding and row height from it.

  Debugging: `bookmark2 --repl 5555` opens a socket REPL on 127.0.0.1, where
  (bookmark2.ui/snapshot) shows what the window shows."
  (:require [bookmark2.anim :as anim]
            [bookmark2.db :as db]
            [bookmark2.log :as log]
            [bookmark2.os :as os]
            [bookmark2.store :as store]
            [bookmark2.web :as web]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (com.formdev.flatlaf FlatDarculaLaf FlatDarkLaf FlatLaf)
           (com.formdev.flatlaf.icons FlatSearchIcon)
           (com.formdev.flatlaf.intellijthemes FlatAllIJThemes)
           (com.formdev.flatlaf.themes FlatMacDarkLaf)
           (com.formdev.flatlaf.util ColorFunctions UIScale)
           (java.awt AWTEvent BorderLayout CardLayout Color Cursor Desktop Dialog$ModalityType Dimension
                     FlowLayout Font FontMetrics Graphics Graphics2D GraphicsEnvironment GridBagConstraints
                     GridBagLayout Insets KeyEventDispatcher KeyboardFocusManager Rectangle Toolkit Window)
           (java.awt.datatransfer DataFlavor StringSelection Transferable UnsupportedFlavorException)
           (java.awt.desktop PreferencesHandler QuitHandler)
           (java.awt.event ActionEvent ActionListener AWTEventListener FocusAdapter InputEvent KeyAdapter
                           KeyEvent MouseAdapter MouseEvent WindowAdapter WindowEvent)
           (javax.swing AbstractAction Action BorderFactory Box BoxLayout DefaultListModel DropMode JButton
                        JCheckBox JComponent JDialog JEditorPane JFileChooser JFrame JLabel JList JMenu JMenuBar
                        JMenuItem JOptionPane JPanel JPopupMenu JProgressBar JScrollPane JSeparator JSlider
                        JSplitPane JTable JTable$DropLocation JTextArea JTextField JTree JTree$DropLocation JWindow
                        KeyStroke ListSelectionModel MenuElement MenuSelectionManager SwingConstants
                        SwingUtilities Timer TransferHandler TransferHandler$TransferSupport UIManager)
           (javax.swing.event ChangeListener DocumentListener ListSelectionListener)
           (javax.swing.filechooser FileNameExtensionFilter)
           (javax.swing.plaf.basic BasicGraphicsUtils)
           (javax.swing.table AbstractTableModel TableCellRenderer)
           (javax.swing.text JTextComponent)
           (javax.swing.tree DefaultMutableTreeNode DefaultTreeCellRenderer DefaultTreeModel TreePath
                             TreeSelectionModel)))

(def app-name "Bookmark2")
(def window-size "At 13 pt; scaled with the text size." [1180 720])
(def tree-width 280)
(def untagged "(untagged)")
(def default-font-size "The text size of a fresh start and of View > Actual Size." 20)
(def font-size-range [10 40])
(def undo-hint (str (if (os/mac?) "Cmd" "Ctrl") "+Z or u undoes it."))

;; :db the loaded data.rec; :scope the tag chosen in the tree (nil = All);
;; :visible the links that pass the tree and the search; :rows what the list
;; shows, those links under tag headers; :folded the tags whose links are
;; hidden, saved in folds.edn; :sort the list order and :session the window's
;; place, file, tag and search, saved in view.edn; :theme :font-size :keys
;; :animations the settings, saved in config.edn
(defonce state (atom {:data-path nil :db nil :query "" :scope nil :visible [] :rows []
                      :folded #{} :sort {:key nil :desc? false} :theme nil :font-size default-font-size
                      :keys {} :animations true :session nil :undo [] :redo [] :status ""}))
(defonce widgets (atom {}))
;; the row under the pointer in the list and in the tree
(defonce ^:private hover (atom {:table nil :tree nil}))
(defonce ^:private jobs (atom 0))

(defn on-edt [f] (SwingUtilities/invokeLater f))

(defn- job!
  "Count a background job in (1) or out (-1). The status bar's busy bar
  shows once one has run 200 ms, so a quick job never flickers it."
  [delta]
  (let [n (swap! jobs + delta)]
    (if (pos? n)
      (when (= 1 n)
        (doto (Timer. 200 (reify ActionListener
                            (actionPerformed [_ _]
                              (when (pos? @jobs) (some-> ^JProgressBar (:busy @widgets) (.setVisible true))))))
          (.setRepeats false)
          .start))
      (some-> ^JProgressBar (:busy @widgets) (.setVisible false)))))

(defn- in-background
  "Run f off the event thread, then (done result) on it. Errors go to the
  status bar. Call it on the event thread."
  [f done]
  (job! 1)
  (future
    (try
      (let [result (f)]
        (on-edt #(do (job! -1) (done result))))
      (catch Throwable e
        (log/event :error :where "background" :message (str e))
        (on-edt #(do (job! -1)
                     (.setText ^JLabel (:status @widgets) (str "Error: " (ex-message e)))))))))

;; ---------------------------------------------------------------- views of state

(defn- matches?
  "Every word of the query appears in the title, url, tags or note."
  [query l]
  (let [hay (str/lower-case (str/join " " (concat [(:title l) (:url l) (:note l)] (:tags l))))]
    (every? #(str/includes? hay %) (str/split (str/lower-case (str/trim query)) #"\s+"))))

(defn- in-scope? [scope l]
  (cond
    (nil? scope) true
    (= scope untagged) (empty? (:tags l))
    :else (some #(db/under? % scope) (:tags l))))

(defn- visible
  "The links in the tree's scope that match the search, newest first."
  [{:keys [db query scope]}]
  (->> (reverse (:links db))
       (filter #(and (in-scope? scope %) (matches? query %)))
       vec))

(def sort-keys
  "What s cycles through; nil is file order. The list's columns, left to right."
  [nil :title :url :tags])

(def ^:private pinned
  "Tags whose groups come first, in this order, under every sort."
  {db/to-read 0 db/done 1})

(defn- group-tags
  "The tags a link is listed under: the ones inside the scope. Without a tag
  scope, a link with no tags is listed under untagged."
  [scope l]
  (let [all? (or (nil? scope) (= scope untagged))
        tags (if all? (:tags l) (filter #(db/under? % scope) (:tags l)))]
    (cond
      (seq tags) tags
      all? [untagged]
      :else [])))

(defn- ordered-groups
  "[tag links] pairs: To Read and Read first, then the tags A-Z, untagged last.
  :title / :url sort the links inside (desc? flips them); :tags sorts the
  groups (desc? flips them); nil keeps the links newest first."
  [visible scope {:keys [key desc?]}]
  (let [lower #(str/lower-case (str %))
        groups (reduce (fn [acc l] (reduce #(update %1 %2 (fnil conj []) l) acc (group-tags scope l)))
                       {} visible)
        rank (fn [t] [(get pinned t 2) (if (= t untagged) 1 0) (lower t)])
        tags (cond-> (sort-by rank (keys groups))
               (and (= key :tags) desc?) reverse)
        link-order (case key :url #(lower (:url %)) #(lower (or (:title %) (:url %))))]
    (for [t tags]
      [t (if (#{:title :url} key)
           (cond-> (sort-by link-order (groups t)) desc? reverse)
           (groups t))])))

(defn- rows
  "The links under one header per tag, folded tags without their links:

    {:header? true :key \"Hardware/FPGA\" :count 1 :folded? false}
      link (with :group \"Hardware/FPGA\")

  While searching, folds are ignored, so no match hides in a closed tag."
  [{:keys [visible folded query sort scope]}]
  (let [searching? (not (str/blank? query))]
    (into [] (mapcat (fn [[tag links]]
                       (let [closed? (and (not searching?) (contains? folded tag))]
                         (cons {:header? true :key tag :count (count links) :folded? closed?}
                               (when-not closed? (map #(assoc % :group tag) links))))))
          (ordered-groups visible scope sort))))

(def columns ["Title" "URL" "Tags"])

(defn- cell-text
  "The plain text of a cell, for accessibility; the renderer paints its own."
  [r col]
  (if (:header? r)
    (if (zero? col) (str (:key r) "  " (:count r)) "")
    (case (int col) 0 (or (:title r) (:url r)) 1 (:url r) (str/join ", " (:tags r)))))

(defn url-parts
  "https://www.x.org/a/b -> [\"https://\" \"www.x.org\" \"/a/b\"]. The host is
  what the eye looks for, so the list draws it brighter than the rest."
  [url]
  (if-let [[_ scheme host more] (re-matches #"(?s)([a-zA-Z][a-zA-Z0-9+.-]*://)([^/?#]*)(.*)" (str url))]
    [scheme host more]
    ["" (str url) ""]))

(defn- table ^JTable [] (:table @widgets))

(defn snapshot
  "What the window shows right now, for the socket REPL."
  []
  (let [s @state
        t (table)]
    {:status (some-> ^JLabel (:status @widgets) .getText)
     :data (:data-path s)
     :links (count (:links (:db s)))
     :query (:query s)
     :scope (:scope s)
     :visible (count (:visible s))
     :rows (count (:rows s))
     :folded (count (:folded s))
     :sort (:sort s)
     :font-size (:font-size s)
     :theme (:theme s)
     :first-rows (mapv #(if (:header? %)
                          (select-keys % [:header? :key :count :folded?])
                          (select-keys % [:title :url :tags]))
                       (take 5 (:rows s)))
     :selected-rows (some-> t .getSelectedRows vec)
     :undo (mapv :label (:undo s))
     :redo (mapv :label (:redo s))}))

(defn- status! [msg]
  (swap! state assoc :status msg)
  (.setText ^JLabel (:status @widgets) msg))

(defn- summary []
  (let [{:keys [db visible]} @state
        ls (:links db)
        n (fn [t] (count (filter #(some #{t} (:tags %)) ls)))]
    (str (count ls) " links, " (n db/to-read) " to read, " (n db/done) " read"
         (when (not= (count ls) (count visible)) (str ", " (count visible) " shown")))))

(defn- n-links [n] (str n (if (= 1 n) " link" " links")))

(defn- lead-index
  "The row the keyboard is on: the lead of the selection when it is selected."
  []
  (let [t (table)
        lead (.getLeadSelectionIndex (.getSelectionModel t))]
    (if (and (>= lead 0) (.isRowSelected t lead)) lead (.getSelectedRow t))))

(defn- selected-row
  "The row the keyboard is on: a link or a tag header."
  []
  (let [i (lead-index)]
    (when (>= i 0) (get (:rows @state) i))))

(defn- selected-rows []
  (let [rs (:rows @state)]
    (keep #(get rs %) (.getSelectedRows (table)))))

(defn- selected-links
  "Every selected link, once: a link under two tags is one link."
  []
  (let [seen (volatile! #{})]
    (filterv #(when-not (@seen (:index %)) (vswap! seen conj (:index %)))
             (remove :header? (selected-rows)))))

(defn- selected-entry
  "The selected link when exactly one is selected; nil on a header or with
  several, so one-link actions do nothing there."
  []
  (let [ls (selected-links)]
    (when (= 1 (count ls)) (first ls))))

(defn- row-id [r]
  (if (:header? r) [:header (:key r)] [(:group r) (:url r)]))

;; ---------------------------------------------------------------- rendering

(defrecord Item [name n tag]
  Object
  (toString [_] name))

(defn- tree-root
  "All, then every tag as a tree (counts include the branch below), then
  untagged when some link has no tag."
  [links]
  (let [count-under (fn [p] (count (filter (fn [l] (some #(db/under? % p) (:tags l))) links)))
        tags (distinct (mapcat :tags links))
        nodes (sort-by (fn [t] [(get pinned t 2) (str/lower-case t)]) (distinct (mapcat db/ancestors-of tags)))
        root (DefaultMutableTreeNode. (->Item "All" (count links) nil))
        by-tag (atom {})]
    ;; parents sort before their children, so each parent exists when its child arrives
    (doseq [t nodes]
      (let [parent-tag (str/join "/" (butlast (str/split t #"/")))
            parent (if (str/blank? parent-tag) root (get @by-tag parent-tag root))
            node (DefaultMutableTreeNode. (->Item (last (str/split t #"/")) (count-under t) t))]
        (.add ^DefaultMutableTreeNode parent node)
        (swap! by-tag assoc t node)))
    (let [n (count (filter #(empty? (:tags %)) links))]
      (when (pos? n)
        (.add root (DefaultMutableTreeNode. (->Item untagged n untagged)))))
    root))

(defn- glide-to-row! [i]
  (let [t (table)]
    (anim/glide! t (.getCellRect t (int i) 0 true))))

(defn- render-table!
  "Rebuild the rows and keep the selection: on target (a row-id) when given,
  else on the rows selected before, the lead one in view, else on the header
  of the lead link's tag when that tag was just folded, else the first row."
  [& [target]]
  (let [before (selected-rows)
        lead (selected-row)
        _ (swap! state #(let [s (assoc % :visible (visible %))] (assoc s :rows (rows s))))
        t (table)
        index (into {} (map-indexed (fn [i r] [(row-id r) i]) (:rows @state)))]
    (.fireTableDataChanged ^AbstractTableModel (.getModel t))
    (when (pos? (.getRowCount t))
      (let [kept (when-not target (keep #(index (row-id %)) before))
            i (or (some-> target index)
                  (some-> lead row-id index)
                  (some->> lead :group (vector :header) index)
                  (first kept)
                  0)]
        (doseq [k kept] (.addRowSelectionInterval t k k))
        (.addRowSelectionInterval t i i)
        (glide-to-row! i)))))

(defn- select-scope-in-tree!
  "Select the tree node for the current scope, or All."
  []
  (let [^JTree tree (:tree @widgets)
        root ^DefaultMutableTreeNode (.getRoot (.getModel tree))
        scope (:scope @state)
        match (->> (enumeration-seq (.breadthFirstEnumeration root))
                   (filter #(= scope (:tag (.getUserObject ^DefaultMutableTreeNode %))))
                   first)
        node (or match root)]
    (.setSelectionPath tree (TreePath. ^"[Ljava.lang.Object;" (.getPath ^DefaultMutableTreeNode node)))))

(defn- db-changed!
  "Rebuild the tree and the list after data.rec was (re)loaded."
  []
  (let [^JTree tree (:tree @widgets)
        links (:links (:db @state))
        tags (set (mapcat #(db/ancestors-of %) (mapcat :tags links)))]
    (swap! state (fn [s] (if (or (nil? (:scope s)) (= untagged (:scope s)) (tags (:scope s))) s (assoc s :scope nil))))
    (.setModel tree (DefaultTreeModel. (tree-root links)))
    (.expandRow tree 0)
    (select-scope-in-tree!)
    (render-table!)))

;; ---------------------------------------------------------------- changes

(defn- reload-db [s] (assoc s :db (db/load-db (:data-path s))))

(defn- apply-changes!
  "Write the changes, re-read data.rec, push them on the undo stack."
  [label changes]
  (store/commit! changes)
  (swap! state #(-> % reload-db (update :undo conj {:label label :changes changes}) (assoc :redo [])))
  (log/event :commit :label label)
  (db-changed!)
  (status! (str label ". " undo-hint)))

(defn- report-error!
  "A refusal: a stale file reloads, the rest is shown."
  [^Exception e]
  (log/event :refused :reason (some-> (ex-data e) :reason name) :message (ex-message e))
  (when (= :stale (:reason (ex-data e)))
    (swap! state reload-db)
    (db-changed!))
  (JOptionPane/showMessageDialog (:frame @widgets) (ex-message e) app-name JOptionPane/WARNING_MESSAGE)
  (status! (ex-message e)))

(defn- guarded
  "f as an action body: refusals become a dialog, anything else is logged too."
  [f]
  (fn []
    (try
      (f)
      (catch clojure.lang.ExceptionInfo e (report-error! e))
      (catch Exception e
        (log/event :error :message (str e))
        (report-error! e)))))

(defn undo! []
  (let [{:keys [label changes] :as step} (peek (:undo @state))]
    (if-not changes
      (status! "Nothing to undo.")
      (do (store/commit! (store/reverse-changes changes))
          (swap! state #(-> % reload-db (update :undo pop) (update :redo conj step)))
          (log/event :undo :label label)
          (db-changed!)
          (status! (str "Undid: " label))))))

(defn redo! []
  (let [{:keys [label changes] :as step} (peek (:redo @state))]
    (if-not changes
      (status! "Nothing to redo.")
      (do (store/commit! changes)
          (swap! state #(-> % reload-db (update :redo pop) (update :undo conj step)))
          (log/event :redo :label label)
          (db-changed!)
          (status! (str "Redid: " label))))))

(defn reload! []
  (status! (str "Reading " (store/short-path (:data-path @state)) " ..."))
  (in-background
   #(let [t0 (System/nanoTime)
          d (db/load-db (:data-path @state))]
      (log/event :load :links (count (:links d)) :ms (quot (- (System/nanoTime) t0) 1000000))
      d)
   (fn [d]
     (swap! state assoc :db d)
     (db-changed!)
     (status! (summary)))))

(defn- link-name [l] (if (str/blank? (:title l)) (:url l) (:title l)))

(defn- links-name
  "One link by its name, several by their count."
  [ls]
  (if (next ls) (n-links (count ls)) (link-name (first ls))))

(defn- open-selected!
  "Open every selected link in the browser; more than five asks first."
  []
  (let [ls (selected-links)]
    (when (and (seq ls)
               (or (<= (count ls) 5)
                   (= JOptionPane/OK_OPTION
                      (JOptionPane/showConfirmDialog (:frame @widgets) (str "Open " (count ls) " links in the browser?")
                                                     app-name JOptionPane/OK_CANCEL_OPTION))))
      (doseq [l ls] (log/event :open :url (:url l)))
      (in-background #(doseq [l ls] (web/open-url! (:url l)))
                     (fn [_] (status! (str "Opened " (if (next ls) (n-links (count ls)) (:url (first ls))))))))))

(defn- copy-selected! []
  (when-let [ls (not-empty (selected-links))]
    (.setContents (.getSystemClipboard (Toolkit/getDefaultToolkit))
                  (StringSelection. (str/join "\n" (map :url ls))) nil)
    (status! (str "Copied " (if (next ls) (str (count ls) " URLs") (:url (first ls)))))))

(defn- delete-selected! []
  (when-let [ls (not-empty (selected-links))]
    (apply-changes! (str "Deleted " (links-name ls)) (db/plan-remove (:db @state) ls))))

(defn- tag-links!
  "Add tag to every link in ls, one change (To Read and Read replace each other)."
  [ls tag]
  (when (and (seq ls) tag)
    (apply-changes! (str "Tagged " (links-name ls) " " tag)
                    (db/plan-retag (:db @state) (for [l ls] [l (db/with-tag (:tags l) tag)])))))

(defn- open-file! []
  (let [path (:data-path @state)]
    (in-background #(os/open-in-editor! path)
                   (fn [_] (status! (str "Opened " (store/short-path path) " in the text editor."))))))

(defn- reveal-file! []
  (let [path (:data-path @state)]
    (in-background #(os/reveal! path) (fn [_] (status! (str "Revealed " (store/short-path path)))))))

;; ---------------------------------------------------------------- data files

(defn- show-path!
  "The window title names the file shown, and says when it is not the default."
  []
  (let [path (:data-path @state)]
    (.setTitle ^JFrame (:frame @widgets)
               (str app-name " - " (store/short-path path)
                    (when (not= path (db/default-path)) "  (not the default)")))))

(defn- pick-file
  "A file chooser for a database: :open an existing one, :new a file to
  create (.rec is added when missing). Starts at start. A File, or nil."
  [mode ^java.io.File start]
  (let [ch (doto (JFileChooser.)
             (.setDialogTitle (if (= mode :new) "New data file" "Open data file"))
             (.setFileFilter (FileNameExtensionFilter. "recutils files (*.rec)" (into-array String ["rec"])))
             (.setSelectedFile start))
        frame (:frame @widgets)]
    (when (= JFileChooser/APPROVE_OPTION
             (if (= mode :new) (.showSaveDialog ch frame) (.showOpenDialog ch frame)))
      (let [f (.getSelectedFile ch)]
        (if (and (= mode :new) (not (str/ends-with? (.getName f) ".rec")))
          (io/file (str f ".rec"))
          f)))))

(defn- use-file!
  "Show the database at path: create it first when create?, make it the
  default when default?. A file that is no database is refused before
  anything changes."
  [path {:keys [create? default?]}]
  (let [path (str path)
        existed? (.exists (io/file path))]
    (when create? (db/create-file! path))
    (let [d (db/load-db path)]
      (when default? (db/set-default! path))
      (swap! state assoc :data-path path :db d :scope nil :undo [] :redo [])
      (log/event :data :path path :created (and create? (not existed?)) :default (boolean default?))
      (show-path!)
      (db-changed!)
      (status! (str (cond (and create? (not existed?)) "Created " :else "Opened ") (store/short-path path)
                    (cond
                      (and default? (System/getenv "BOOKMARK2_DATA"))
                      ". It is the default in the config, but BOOKMARK2_DATA wins while it is set."
                      default? ", now the default."
                      (not= path (db/default-path)) " for this window; the default is unchanged."
                      :else "."))))))

(defn new-data-file! []
  (when-let [f (pick-file :new (io/file (store/home) "data.rec"))]
    (use-file! f {:create? true})))

(defn open-data-file! []
  (when-let [f (pick-file :open (io/file (:data-path @state)))]
    (use-file! f {})))

(defn change-default! []
  (when-let [f (pick-file :open (io/file (:data-path @state)))]
    (use-file! f {:default? true})))

(defn- first-start!
  "No database named and none at the default place: ask for a new file or an
  existing one, and make it the default. Quit leaves without writing."
  []
  (let [options (into-array String ["Create New..." "Open Existing..." "Quit"])]
    (loop []
      (let [choice (JOptionPane/showOptionDialog
                    (:frame @widgets)
                    (str "<html>Bookmark2 keeps your links in one recutils file, data.rec.<br><br>"
                         "Create a new one, or open one you already have?<br>"
                         "The file becomes the default; File > Change Default Data File... changes it.</html>")
                    app-name JOptionPane/YES_NO_CANCEL_OPTION JOptionPane/QUESTION_MESSAGE nil
                    options (aget options 0))
            f (case choice
                0 (pick-file :new (io/file (store/home) "data.rec"))
                1 (pick-file :open (io/file (store/home)))
                (System/exit 0))
            done? (when f
                    (try (use-file! f {:create? (= choice 0) :default? true}) true
                         (catch clojure.lang.ExceptionInfo e
                           (JOptionPane/showMessageDialog (:frame @widgets) (ex-message e) app-name
                                                          JOptionPane/WARNING_MESSAGE)
                           false)))]
        (when-not done? (recur))))))

;; ---------------------------------------------------------------- moving around

(defn- select-row!
  "Select row i of the list alone, clamped, and bring it into view: a long
  jump glides there."
  [i]
  (let [t (table)
        n (.getRowCount t)]
    (when (pos? n)
      (let [i (max 0 (min (dec n) i))]
        (.setRowSelectionInterval t i i)
        (glide-to-row! i)))))

(defn- move-row! [delta]
  (select-row! (+ delta (max 0 (lead-index)))))

(defn- extend-row!
  "J / K: grow or shrink the selection from its anchor, one row."
  [delta]
  (let [t (table)
        n (.getRowCount t)
        i (max 0 (min (dec n) (+ delta (max 0 (.getLeadSelectionIndex (.getSelectionModel t))))))]
    (when (pos? n) (.changeSelection t i 0 false true))))

(defn- page!
  "Page Up / Down: a screenful of rows, gliding."
  [delta]
  (let [t (table)]
    (move-row! (* delta (max 1 (quot (.height (.getVisibleRect t)) (max 1 (.getRowHeight t))))))))

(defn- move-tree! [delta]
  (let [^JTree tree (:tree @widgets)
        row (max 0 (min (dec (.getRowCount tree)) (+ delta (max 0 (.getLeadSelectionRow tree)))))]
    (.setSelectionRow tree row)
    (.scrollRowToVisible tree row)))

(defn- focus-search! []
  (doto ^JTextField (:search @widgets) .requestFocusInWindow .selectAll))

(defn- focus-tree! []
  (.requestFocusInWindow ^JTree (:tree @widgets)))

(defn- focus-list! []
  (when (neg? (.getSelectedRow (table)))
    (select-row! 0))
  (.requestFocusInWindow (table)))

(defn- search-escape!
  "Esc in search: clear it; when already empty, go to the list."
  []
  (let [^JTextField search (:search @widgets)]
    (if (str/blank? (.getText search))
      (focus-list!)
      (.setText search ""))))

(defn- list-escape!
  "Esc in the list: with several rows selected keep only the lead one, else
  go to the search."
  []
  (if (next (.getSelectedRows (table)))
    (select-row! (lead-index))
    (focus-search!)))

;; ---------------------------------------------------------------- folding

(defn- folds-file [] (io/file (log/state-dir) "folds.edn"))

(defn- load-folds []
  (try
    (set (filter string? (edn/read-string (slurp (folds-file)))))
    (catch Exception _ #{})))

(defn- save-folds! []
  (try
    (io/make-parents (folds-file))
    (spit (folds-file) (pr-str (vec (sort (:folded @state)))))
    (catch Exception e (log/event :error :where "save-folds" :message (str e)))))

(defn- current-group
  "The tag of the selected header, or of the group the selected link is in."
  []
  (when-let [r (selected-row)]
    (if (:header? r) (:key r) (:group r))))

(defn- set-folds!
  "Replace the folded set, save it, redraw with the selection on target."
  [folded target]
  (swap! state assoc :folded folded)
  (save-folds!)
  (render-table! target))

(defn- fold!
  "Fold (true), unfold (false) or toggle (:toggle) the current tag. The
  selection lands on its header."
  [how]
  (when-let [k (current-group)]
    (let [folded (:folded @state)
          close? (if (= how :toggle) (not (contains? folded k)) how)]
      (when-not (str/blank? (:query @state))
        (status! "Folds are ignored while searching; clear the search to see them."))
      (set-folds! (if close? (conj folded k) (disj folded k)) [:header k]))))

(defn- fold-all! []
  (set-folds! (into (:folded @state) (keep #(when (:header? %) (:key %)) (:rows @state))) nil))

(defn- unfold-all! []
  (set-folds! #{} nil))

;; ---------------------------------------------------------------- sorting and the session

(defn- view-file [] (io/file (log/state-dir) "view.edn"))

(defn- load-view []
  (try
    (let [v (edn/read-string (slurp (view-file)))]
      (if (some #{(get-in v [:sort :key])} sort-keys) v (dissoc v :sort)))
    (catch Exception _ {})))

(defn- save-view!
  "view.edn: the sort, the window as it was at the last quit, and that the
  first-run defaults were applied."
  []
  (try
    (io/make-parents (view-file))
    (spit (view-file) (pr-str (cond-> {:sort (:sort @state) :tag-defaults true}
                                (:session @state) (assoc :session (:session @state)))))
    (catch Exception e (log/event :error :where "save-view" :message (str e)))))

(defn- save-setting!
  "One setting into config.edn; nil takes it out, back to the default."
  [k v]
  (try
    (db/update-config! #(if (nil? v) (dissoc % k) (assoc % k v)))
    (catch Exception e (log/event :error :where "settings" :message (str e)))))

(defn- sort-label [{:keys [key desc?]}]
  (if key
    (str "Sorted by " (str/capitalize (name key)) (if desc? " ▼" " ▲"))
    "Newest first"))

(defn- show-sort-in-header!
  "The sorted column's name gets ▲ or ▼."
  []
  (let [t (table)
        {:keys [key desc?]} (:sort @state)]
    (doseq [[i col-name] (map-indexed vector columns)]
      (.setHeaderValue (.getColumn (.getColumnModel t) (int i))
                       (str col-name (when (= key (get sort-keys (inc i))) (if desc? "  ▼" "  ▲")))))
    (.repaint (.getTableHeader t))))

(defn- set-sort!
  "Apply and save a sort, keeping the selected row selected."
  [sort]
  (swap! state assoc :sort sort)
  (save-view!)
  (show-sort-in-header!)
  (render-table!)
  (status! (sort-label sort)))

(defn- cycle-sort!
  "s: newest first -> Title -> URL -> Tags -> newest first, ascending."
  []
  (let [k (:key (:sort @state))
        next-key (get sort-keys (mod (inc (.indexOf ^java.util.List sort-keys k)) (count sort-keys)))]
    (set-sort! {:key next-key :desc? false})))

(defn- flip-sort!
  "S: ascending <-> descending. In newest-first order it starts Title descending."
  []
  (let [{:keys [key desc?]} (:sort @state)]
    (set-sort! (if key {:key key :desc? (not desc?)} {:key :title :desc? true}))))

(defn- sort-by-column!
  "A click on column i: sort by it, or flip it when it is already the sort."
  [i]
  (let [k (get sort-keys (inc i))
        {:keys [key desc?]} (:sort @state)]
    (set-sort! {:key k :desc? (and (= k key) (not desc?))})))

(defn- list-enter!
  "Enter: open the selected links; on a folded header unfold it, on an open
  one go to its first link."
  []
  (let [r (selected-row)]
    (cond
      (not (:header? r)) (open-selected!)
      (:folded? r) (fold! false)
      :else (move-row! 1))))

(defn- list-left!
  "h or Left: from a link to its tag header; from a header to the tags."
  []
  (let [r (selected-row)]
    (if (or (nil? r) (:header? r))
      (focus-tree!)
      (render-table! [:header (:group r)]))))

(defn- toggle-read!
  "r: To Read <-> Read on the selected link (no reading tag: To Read is
  added), and the cursor stays on its row, so the next link to triage is
  under it. With several selected: all Read, or all back to To Read when
  every one is read already."
  []
  (when-let [ls (not-empty (selected-links))]
    (let [row (.getSelectedRow (table))
          target (when (next ls) (if (every? #(some #{db/done} (:tags %)) ls) db/to-read db/done))
          pairs (for [l ls] [l (if target (db/with-tag (:tags l) target) (db/toggle-read (:tags l)))])]
      (apply-changes! (str (if (some #{db/done} (second (first pairs))) "Read: " "To read: ") (links-name ls))
                      (db/plan-retag (:db @state) pairs))
      (when-not (next ls) (select-row! row)))))

;; ---------------------------------------------------------------- forms

(defn- gbc [x y weight & {:keys [width anchor fill] :or {width 1 anchor GridBagConstraints/WEST
                                                         fill GridBagConstraints/HORIZONTAL}}]
  (let [c (GridBagConstraints.)
        pad (UIScale/scale (int 4))]
    (set! (.gridx c) x)
    (set! (.gridy c) y)
    (set! (.gridwidth c) width)
    (set! (.weightx c) (double weight))
    (set! (.weighty c) (if (= fill GridBagConstraints/BOTH) 1.0 0.0))
    (set! (.fill c) (int fill))
    (set! (.anchor c) (int anchor))
    (set! (.insets c) (Insets. pad pad pad pad))
    c))

(defn- clipboard-url []
  (try
    (let [s (str/trim (str (.getData (.getSystemClipboard (Toolkit/getDefaultToolkit)) DataFlavor/stringFlavor)))]
      (when (re-matches #"https?://\S+" s) s))
    (catch Exception _ nil)))

(defn- parse-tags
  "\"AI/Agents, To Read\" -> [\"AI/Agents\" \"To Read\"]"
  [s]
  (db/clean-tags (str/split (str s) #",")))

(defn- tag-segment
  "[start end] of the comma-separated tag the caret is in."
  [text caret]
  [(if (pos? caret) (inc (or (str/last-index-of text "," (dec caret)) -1)) 0)
   (or (str/index-of text "," caret) (count text))])

(def max-completions 12)

(defn- tag-completions
  "Known tags for what is typed, those already chosen left out: the ones that
  start with it first, then those with a part that starts with it (fpga ->
  Hardware/FPGA), then the rest that contain it. Blank typed -> all of them."
  [known typed chosen]
  (let [q (str/lower-case (str/trim typed))
        chosen (set (map str/lower-case chosen))
        rank (fn [t]
               (let [l (str/lower-case t)]
                 (cond (str/starts-with? l q) 0
                       (some #(str/starts-with? % q) (str/split l #"/")) 1
                       (str/includes? l q) 2)))]
    (->> known
         (remove #(chosen (str/lower-case %)))
         (keep #(when-let [r (rank %)] [r %]))
         (sort-by first)
         (map second)
         (take max-completions)
         vec)))

(defn- complete-tags!
  "Suggest known tags under field as it is typed in. Down opens the list or
  moves down, Up moves up, Enter takes the chosen tag, Esc closes the list;
  with the list closed, Enter and Esc reach the dialog. multi?: the field is
  comma separated, and taking a tag at the end adds \", \" for the next."
  [^JTextField field multi?]
  (let [known (db/known-tags (:links (:db @state)))
        model (DefaultListModel.)
        lst (doto (JList. model)
              (.setFocusable false)
              (.setSelectionMode ListSelectionModel/SINGLE_SELECTION))
        scroll (doto (JScrollPane. lst) (.setBorder (UIManager/getBorder "PopupMenu.border")))
        popup (atom nil)
        quiet? (atom false)
        open? #(some-> ^JWindow @popup .isVisible)
        hide! #(some-> ^JWindow @popup (.setVisible false))
        segment (fn [] (let [t (.getText field)] [t (tag-segment t (.getCaretPosition field))]))
        refresh! (fn [all?]
                   (let [[t [a b]] (segment)
                         typed (subs t a b)
                         chosen (if multi? (parse-tags (str (subs t 0 a) "," (subs t b))) [])
                         items (when (or all? (not (str/blank? typed))) (tag-completions known typed chosen))]
                     (if (empty? items)
                       (hide!)
                       (let [^JWindow w (or @popup
                                            (reset! popup (doto (JWindow. (SwingUtilities/getWindowAncestor field))
                                                            (.setFocusableWindowState false)
                                                            (.add scroll))))]
                         (.clear model)
                         (doseq [i items] (.addElement model i))
                         (.setSelectedIndex lst 0)
                         (.setVisibleRowCount lst (min 8 (count items)))
                         (.setPreferredSize scroll nil)
                         (.setPreferredSize scroll (Dimension. (.getWidth field) (.height (.getPreferredSize scroll))))
                         (.pack w)
                         (let [p (.getLocationOnScreen field)]
                           (.setLocation w (.x p) (+ (.y p) (.getHeight field))))
                         (when-not (.isVisible w) (anim/fade-in! w #(.setVisible w true)))))))
        accept! (fn []
                  (when-let [v (.getSelectedValue lst)]
                    (let [[t [a b]] (segment)
                          before (str (subs t 0 a) (when (pos? a) " ") v (when (and multi? (= b (count t))) ", "))]
                      (reset! quiet? true)
                      (.setText field (str before (subs t b)))
                      (reset! quiet? false)
                      (.setCaretPosition field (count before))
                      (hide!))))
        move! (fn [d]
                (if (open?)
                  (let [i (max 0 (min (dec (.getSize model)) (+ d (.getSelectedIndex lst))))]
                    (.setSelectedIndex lst i)
                    (.ensureIndexIsVisible lst i))
                  (when (pos? d) (refresh! true))))
        ;; enabled only while the list is open, so otherwise the key goes on to the dialog
        bind! (fn [ks name f always?]
                (.put (.getInputMap field JComponent/WHEN_FOCUSED) (KeyStroke/getKeyStroke ^String ks) name)
                (.put (.getActionMap field) name (proxy [AbstractAction] []
                                                   (isEnabled [] (boolean (or always? (open?))))
                                                   (actionPerformed [_] (f)))))]
    (bind! "DOWN" "tags-down" #(move! 1) true)
    (bind! "UP" "tags-up" #(move! -1) false)
    (bind! "ENTER" "tags-take" accept! false)
    (bind! "ESCAPE" "tags-close" hide! false)
    (.addDocumentListener (.getDocument field)
                          (reify DocumentListener
                            (insertUpdate [_ _] (when-not @quiet? (on-edt #(refresh! false))))
                            (removeUpdate [_ _] (when-not @quiet? (on-edt #(refresh! false))))
                            (changedUpdate [_ _])))
    (.addFocusListener field (proxy [FocusAdapter] [] (focusLost [_] (hide!))))
    (.setCursor lst (Cursor/getPredefinedCursor Cursor/HAND_CURSOR))
    (.addMouseListener lst (proxy [MouseAdapter] []
                             (mouseClicked [e]
                               (.setSelectedIndex lst (.locationToIndex lst (.getPoint ^MouseEvent e)))
                               (accept!))))
    field))

(def ok-key "OK from anywhere in a form, the multi-line note included."
  (if (os/mac?) "meta ENTER" "ctrl ENTER"))
(def ok-key-name (if (os/mac?) "Cmd+Enter" "Ctrl+Enter"))

(defn- show-form
  "A modal OK/Cancel dialog around panel, focus in first. True on OK."
  [heading ^JPanel panel ^JComponent first]
  (let [pane (JOptionPane. panel JOptionPane/PLAIN_MESSAGE JOptionPane/OK_CANCEL_OPTION)
        dialog (.createDialog pane (:frame @widgets) heading)]
    (.put (.getInputMap pane JComponent/WHEN_ANCESTOR_OF_FOCUSED_COMPONENT) (KeyStroke/getKeyStroke ^String ok-key) "ok")
    (.put (.getActionMap pane) "ok" (proxy [AbstractAction] []
                                      (actionPerformed [_] (.setValue pane (int JOptionPane/OK_OPTION)))))
    (.addWindowListener dialog (proxy [WindowAdapter] []
                                 (windowOpened [_] (.requestFocusInWindow first))))
    (.setVisible dialog true)
    (= JOptionPane/OK_OPTION (.getValue pane))))

(defn- link-form
  "URL, Title, Tags (comma separated, / nests) and Note. A blank title is
  fetched in the background when fetch? is set, with a spinner in the title
  field meanwhile. {:url :title :tags :note} or nil."
  [{:keys [heading url title tags note fetch?]}]
  (let [url-field (JTextField. (str url) 48)
        title-field (JTextField. (str title) 48)
        tags-field (complete-tags! (JTextField. (str/join ", " tags) 48) true)
        note-field (doto (JTextArea. (str note) 4 48)
                     (.setLineWrap true)
                     (.setWrapStyleWord true)
                     ;; plain text in a plain font
                     (.putClientProperty "FlatLaf.styleClass" "monospaced")
                     ;; Tab leaves the note, Enter starts a new line
                     (.setFocusTraversalKeys KeyboardFocusManager/FORWARD_TRAVERSAL_KEYS nil)
                     (.setFocusTraversalKeys KeyboardFocusManager/BACKWARD_TRAVERSAL_KEYS nil))
        fetch-button (doto (JButton. "Fetch title")
                       (.setMnemonic KeyEvent/VK_E)
                       (.setCursor (Cursor/getPredefinedCursor Cursor/HAND_CURSOR)))
        note-label (JLabel. " ")
        spin (anim/spinner)
        panel (JPanel. (GridBagLayout.))
        fetch! (fn []
                 (let [u (str/trim (.getText url-field))]
                   (when (re-matches #"https?://\S+" u)
                     (.setText note-label "Fetching title ...")
                     (.putClientProperty title-field "JTextField.trailingComponent" spin)
                     (in-background #(web/fetch-title u)
                                    (fn [t]
                                      (when (str/blank? (.getText title-field)) (.setText title-field t))
                                      (.putClientProperty title-field "JTextField.trailingComponent" nil)
                                      (.setText note-label " "))))))]
    (.addActionListener fetch-button (reify ActionListener (actionPerformed [_ _] (fetch!))))
    (.putClientProperty url-field "JTextField.placeholderText" "https://")
    (.putClientProperty title-field "JTextField.placeholderText" "optional; Fetch title reads the page's")
    (.putClientProperty tags-field "JTextField.placeholderText" "comma separated; a / nests: Hardware/FPGA, To Read; Down lists tags")
    (.putClientProperty note-field "JTextField.placeholderText" (str "optional; Enter starts a line, " ok-key-name " is OK"))
    ;; mnemonics: Alt+letter (Ctrl+Option on macOS) jumps to a field or presses a button
    (doseq [[row label mnemonic comp extra] [[0 "URL" KeyEvent/VK_U url-field nil]
                                             [1 "Title" KeyEvent/VK_T title-field fetch-button]
                                             [2 "Tags" KeyEvent/VK_G tags-field nil]
                                             [3 "Note" KeyEvent/VK_N note-field nil]]]
      (.add panel (doto (JLabel. ^String label)
                    (.setDisplayedMnemonic (int mnemonic))
                    (.setLabelFor comp))
            (gbc 0 row 0 :anchor GridBagConstraints/NORTHWEST))
      (.add panel (if (= comp note-field) (JScrollPane. ^JComponent comp) ^JComponent comp) (gbc 1 row 1))
      (when extra (.add panel ^JComponent extra (gbc 2 row 0))))
    (.add panel note-label (gbc 0 4 1 :width 3))
    (when (and fetch? (str/blank? title)) (fetch!))
    (when (show-form heading panel url-field)
      {:url (str/trim (.getText url-field))
       :title (.getText title-field)
       :tags (parse-tags (.getText tags-field))
       :note (.getText note-field)})))

(defn- run-form
  "Show the form until its values plan cleanly or the user cancels: a refusal
  is shown and the form comes back with what was typed."
  [form-opts plan label]
  (loop [values form-opts]
    (when-let [v (link-form values)]
      (let [result (try {:changes (plan v)}
                        (catch clojure.lang.ExceptionInfo e {:error e}))]
        (if-let [e (:error result)]
          (do (JOptionPane/showMessageDialog (:frame @widgets) (ex-message e) app-name JOptionPane/WARNING_MESSAGE)
              (recur (merge values v)))
          (apply-changes! (label v) (:changes result)))))))

(defn- scope-tag
  "The tree's tag, when a real tag is chosen."
  []
  (let [s (:scope @state)]
    (when-not (or (nil? s) (= s untagged)) s)))

(defn add!
  "Add a link: the clipboard's url and the tree's tag, unless a drop or a
  paste names them. No tags means To Read."
  ([] (add! {}))
  ([{:keys [url tags]}]
   (run-form {:heading "Add link" :url (or url (clipboard-url)) :tags (or tags (keep identity [(scope-tag)]))
              :fetch? true}
             #(db/plan-add (:db @state) %)
             #(str "Added " (link-name %)))))

(defn edit! []
  (if-let [l (selected-entry)]
    (run-form {:heading "Edit link" :url (:url l) :title (:title l) :tags (:tags l) :note (:note l)}
              #(db/plan-edit (:db @state) l %)
              #(str "Saved " (link-name %)))
    (when (next (selected-links))
      (status! "Edit works on one link; t adds a tag to every selected one."))))

(defn- ask-tag
  "A one-field form with tag completion: the tidied tag typed, or nil."
  [heading prompt start]
  (let [field (complete-tags! (JTextField. (str start) 40) false)
        panel (doto (JPanel. (GridBagLayout.))
                (.add (JLabel. ^String prompt) (gbc 0 0 1))
                (.add field (gbc 0 1 1)))]
    (when (show-form heading panel field)
      (db/clean-tag (.getText field)))))

(defn add-tag!
  "t: add one tag to every selected link."
  []
  (if-let [ls (not-empty (selected-links))]
    (when-let [tag (ask-tag "Add tag" (str "Add a tag to " (links-name ls) ":") "")]
      (tag-links! ls tag))
    (status! "Select links first; t adds a tag to every selected one.")))

(defn- tag-in-focus
  "The tag Rename acts on: the tree's when the tree has focus, else the list's
  header or link group."
  []
  (if (.isFocusOwner ^JTree (:tree @widgets))
    (scope-tag)
    (let [g (current-group)] (when-not (= g untagged) g))))

(defn- rename-to!
  "Rename tag from (and the tags below it) to to on every link. The tree
  follows: a scope at or below from moves with it."
  [verb from to]
  (when (and to (not= to from))
    (let [scope (:scope @state)]
      (when (and scope (db/under? scope from))
        (swap! state assoc :scope (str to (subs scope (count from))))))
    (apply-changes! (str verb " " from " to " to) (db/plan-rename-tag (:db @state) from to))))

(defn rename-tag!
  "F2: rename a tag on every link. A / moves it: FPGA -> Hardware/FPGA. Tags
  below it move along."
  []
  (if-let [from (tag-in-focus)]
    (rename-to! "Renamed" from (ask-tag "Rename tag" (str "Rename \"" from "\" and every tag below it to:") from))
    (status! "Choose a tag in the tree, or a tag header in the list, to rename it.")))

;; ---------------------------------------------------------------- themes and text size

(def row-height "At 13 pt; scaled with the text size." 26)
(def tree-row-height 24)

(defn themes
  "Every dark theme as [name class]: FlatLaf's own first, then the IntelliJ
  theme pack's (Gruvbox, Nord, Dracula, One Dark, Solarized, Monokai, ...)."
  []
  (vec (concat [["FlatLaf Dark" (.getName (if (os/mac?) FlatMacDarkLaf FlatDarkLaf))]
                ["Darcula" (.getName FlatDarculaLaf)]]
               (for [i FlatAllIJThemes/INFOS :when (.isDark i)]
                 [(.getName i) (.getClassName i)]))))

(defn- current-theme [] (or (:theme @state) (second (first (themes)))))

(defn- theme-name [cls] (or (some (fn [[n c]] (when (= c cls) n)) (themes)) cls))

(defn- style-widgets!
  "What a theme or text size switch does not redo by itself: row heights."
  []
  (let [{:keys [^JTable table ^JTree tree]} @widgets]
    (when table (.setRowHeight table (UIScale/scale (int row-height))))
    (when tree (.setRowHeight tree (UIScale/scale (int tree-row-height))))))

(defn- set-theme!
  "Switch every window to the look and feel class cls, cross-fading, and save it."
  [cls]
  (anim/cross-fade! (:frame @widgets)
                    (fn []
                      (UIManager/setLookAndFeel ^String cls)
                      (FlatLaf/updateUI)
                      (anim/fade-popups!)
                      (style-widgets!)))
  (swap! state assoc :theme cls)
  (save-setting! :theme cls)
  (status! (str "Theme: " (theme-name cls))))

(defn- cycle-theme!
  "Cmd+] / Cmd+[: the next or previous theme, saved."
  [delta]
  (let [ts (themes)
        i (or (first (keep-indexed #(when (= (second %2) (current-theme)) %1) ts)) 0)]
    (set-theme! (second (nth ts (mod (+ i delta) (count ts)))))))

(defn clamp-font-size [n]
  (let [[lo hi] font-size-range] (max lo (min hi (int n)))))

(defn- put-font-size!
  "The look's default font at n points. Every widget's font derives from it,
  and FlatLaf scales paddings and row heights with it. The value outlives
  theme switches."
  [n]
  (UIManager/put "defaultFont" (.deriveFont (UIManager/getFont "defaultFont") (float n))))

(defn- set-font-size!
  "Text size n for the whole window, now. It is saved as the default at quit."
  [n]
  (let [n (clamp-font-size n)]
    (swap! state assoc :font-size n)
    (put-font-size! n)
    (FlatLaf/updateUI)
    (style-widgets!)
    (status! (str "Text size " n " pt"))))

(defn- change-font-size! [delta]
  (set-font-size! (+ delta (:font-size @state))))

(defn- set-animations!
  "Fades and glides on or off: ours, and FlatLaf's smooth scrolling."
  [on]
  (reset! anim/on? (boolean on))
  (System/setProperty "flatlaf.animation" (str (boolean on)))
  (UIManager/put "ScrollPane.smoothScrolling" (boolean on))
  (swap! state assoc :animations (boolean on))
  (save-setting! :animations (when-not on false)))

;; ---------------------------------------------------------------- colours and painting

(defn- accent []
  (or (UIManager/getColor "Component.accentColor") (UIManager/getColor "Component.focusColor")
      (Color. 0x4c 0x8d 0xff)))

(defn- dim [] (or (UIManager/getColor "Label.disabledForeground") Color/GRAY))

(defn- band
  "A header's background: the table's, a little lighter."
  [^Color b]
  (Color. (min 255 (+ 14 (.getRed b))) (min 255 (+ 14 (.getGreen b))) (min 255 (+ 18 (.getBlue b)))))

(defn- hover-color
  "A row under the pointer: its background moved 7% toward the text colour."
  [^Color bg ^Color fg]
  (ColorFunctions/mix fg bg (float 0.07)))

(defn- hex [^Color c] (format "#%02x%02x%02x" (.getRed c) (.getGreen c) (.getBlue c)))

(defn- esc-html [s]
  (-> (str s) (str/replace "&" "&amp;") (str/replace "<" "&lt;") (str/replace ">" "&gt;")))

(defn- tag-color
  "To Read in the accent, Read dimmed; every other tag in its top-level
  branch's colour, a hue from the branch name, the same in every theme."
  [tag]
  (cond
    (= tag db/to-read) (accent)
    (= tag db/done) (dim)
    :else (let [top (first (str/split tag #"/"))]
            (Color/getHSBColor (float (/ (mod (Math/abs (long (.hashCode ^String top))) 360) 360.0))
                               (float 0.45) (float 0.95)))))

(defn- fit
  "s, cut with … to fit room pixels."
  [^FontMetrics fm ^String s room]
  (if (<= (.stringWidth fm s) room)
    s
    (let [e "…"
          ew (.stringWidth fm e)]
      (loop [lo 0 hi (count s)]
        (if (>= lo hi)
          (str (subs s 0 lo) e)
          (let [mid (quot (+ lo hi 1) 2)]
            (if (<= (+ ew (.stringWidth fm (subs s 0 mid))) room)
              (recur mid hi)
              (recur lo (dec mid)))))))))

(defn- cell-runs
  "What one cell of the list paints: {:pad :gap :runs}, runs of [text colour
  font]. Headers bold in the accent with a dim count; titles indented under
  them; urls monospaced, the host bright; tags in their branch colours. A
  selected row draws everything in the selection colour."
  [^JTable t r col selected?]
  (let [font (.getFont t)
        fg (if selected? (.getSelectionForeground t) (.getForeground t))
        quiet (if selected? fg (dim))
        pad (UIScale/scale (int 6))
        gap (UIScale/scale (int 14))]
    (cond
      (:header? r)
      {:pad pad :gap gap
       :runs (when (zero? col)
               [[(str (if (:folded? r) "▸ " "▾ ") (:key r)) (if selected? fg (accent)) (.deriveFont font Font/BOLD)]
                [(str (:count r)) quiet font]])}

      (zero? col) {:pad (UIScale/scale (int 28)) :gap 0 :runs [[(or (:title r) (:url r)) fg font]]}

      (= 1 col)
      (let [mono (.deriveFont ^Font (UIManager/getFont "monospaced.font") (float (max 8 (- (.getSize font) 2))))
            [scheme host more] (url-parts (:url r))]
        {:pad pad :gap 0 :runs [[scheme quiet mono] [host fg mono] [more quiet mono]]})

      :else {:pad pad :gap gap :runs (vec (for [tag (:tags r)] [tag (if selected? fg (tag-color tag)) font]))})))

(defn- cell-renderer
  "Every cell of the list, painted as runs of text: no HTML to parse while
  scrolling, and each run keeps its own colour and font."
  []
  (let [cell (atom nil)]
    (proxy [JLabel TableCellRenderer] []
      (getTableCellRendererComponent [^JTable t _ selected? _ row col]
        (let [r (get (:rows @state) row)
              base (if (:header? r) (band (.getBackground t)) (.getBackground t))]
          (reset! cell (assoc (cell-runs t r col selected?)
                              :bg (cond selected? (.getSelectionBackground t)
                                        (= row (:table @hover)) (hover-color base (.getForeground t))
                                        :else base)))
          this))
      (paintComponent [^Graphics g]
        (let [{:keys [bg pad gap runs]} @cell
              g2 ^Graphics2D g
              w (.getWidth ^JLabel this)
              h (.getHeight ^JLabel this)]
          (.setColor g2 bg)
          (.fillRect g2 0 0 w h)
          (loop [x pad
                 [[text color font] & more] runs]
            (when (and text (< x (- w pad)))
              (let [fm (.getFontMetrics ^JLabel this font)
                    s (fit fm text (- w pad x))
                    y (+ (quot (- h (.getHeight fm)) 2) (.getAscent fm))]
                (.setFont g2 font)
                (.setColor g2 color)
                (BasicGraphicsUtils/drawString ^JLabel this g2 ^String s (float x) (float y))
                (when (= s text)
                  (recur (+ x (.stringWidth fm s) gap) more))))))))))

(defn- tree-renderer
  "A tag and its count, the count dimmed."
  []
  (proxy [DefaultTreeCellRenderer] []
    (getTreeCellRendererComponent [tree value sel? expanded? leaf? row focus?]
      (let [^JLabel c (proxy-super getTreeCellRendererComponent tree value sel? expanded? leaf? row focus?)
            item (.getUserObject ^DefaultMutableTreeNode value)]
        (when (instance? Item item)
          (let [{:keys [name n]} item]
            (.setText c (cond
                          (nil? n) name
                          sel? (str name "  " n)
                          :else (str "<html>" (esc-html name) "&nbsp;&nbsp;<span style='color:" (hex (dim)) "'>"
                                     n "</span></html>")))))
        c))))

(defn- hover!
  "The pointer is over row (nil: none) of c: repaint the old and new row."
  [k ^JComponent c row bounds]
  (let [old (get @hover k)]
    (when (not= old row)
      (swap! hover assoc k row)
      (doseq [r [old row] :when r
              :let [^Rectangle b (bounds r)] :when b]
        (.repaint c 0 (.y b) (.getWidth c) (.height b))))))

;; ---------------------------------------------------------------- drag and drop, cut, copy, paste

(def ^:private links-flavor
  (DataFlavor. (str DataFlavor/javaJVMLocalObjectMimeType ";class=java.util.List") "bookmark2 links"))
(def ^:private tag-flavor
  (DataFlavor. (str DataFlavor/javaJVMLocalObjectMimeType ";class=java.lang.String") "bookmark2 tag"))

(defn- transferable
  "data under our own flavor, and text for every other program."
  [^DataFlavor flavor data ^String text]
  (reify Transferable
    (getTransferDataFlavors [_] (into-array DataFlavor [flavor DataFlavor/stringFlavor]))
    (isDataFlavorSupported [_ f] (or (= f flavor) (= f DataFlavor/stringFlavor)))
    (getTransferData [_ f]
      (cond (= f flavor) data
            (= f DataFlavor/stringFlavor) text
            :else (throw (UnsupportedFlavorException. f))))))

(defn url-in
  "The first http(s) url in text: a link dropped from a browser, a paste."
  [s]
  (re-find #"https?://[^\s<>\"']+" (str s)))

(defn moved-tag
  "Where dragging tag from onto tag onto puts it: under onto, keeping its last
  part; onto nil (All) makes it top level. nil when that changes nothing or
  would put the tag inside itself."
  [from onto]
  (let [to (str (when onto (str onto "/")) (last (str/split from #"/")))]
    (when-not (or (= to from) (and onto (db/under? onto from)))
      to)))

(defn- support-data [^TransferHandler$TransferSupport s ^DataFlavor f]
  (try (.getTransferData (.getTransferable s) f) (catch Exception _ nil)))

(defn- links-at-urls
  "The links of the database at these urls, for a drop: the dragged maps may
  be older than the file."
  [urls]
  (let [want (set urls)] (filterv #(want (:url %)) (:links (:db @state)))))

(defn- table-transfer
  "The list: links drag out as their urls; links dropped on a tag header get
  that tag; a url dropped or pasted anywhere is added, with the tag under it."
  []
  (let [target (fn [^TransferHandler$TransferSupport s]
                 (when (.isDrop s) (get (:rows @state) (.getRow ^JTable$DropLocation (.getDropLocation s)))))
        header (fn [s] (let [r (target s)] (when (and (:header? r) (not= untagged (:key r))) (:key r))))
        can? (fn [^TransferHandler$TransferSupport s]
               (if (.isDataFlavorSupported s links-flavor)
                 (some? (header s))
                 (.isDataFlavorSupported s DataFlavor/stringFlavor)))]
    (proxy [TransferHandler] []
      (getSourceActions [_] TransferHandler/COPY)
      (createTransferable [_]
        (when-let [ls (not-empty (selected-links))]
          (transferable links-flavor (mapv :url ls) (str/join "\n" (map :url ls)))))
      (canImport
        ([s] (boolean (can? s)))
        ([_ _] false))
      (importData
        ([^TransferHandler$TransferSupport s]
         (boolean
          (when (can? s)
            (if (.isDataFlavorSupported s links-flavor)
              (let [urls (support-data s links-flavor) tag (header s)]
                (on-edt (guarded #(tag-links! (links-at-urls urls) tag)))
                true)
              (when-let [u (url-in (support-data s DataFlavor/stringFlavor))]
                (let [r (target s)
                      g (or (:key r) (:group r))
                      tag (if (and g (not= g untagged)) g (scope-tag))]
                  (on-edt (guarded #(add! {:url u :tags (keep identity [tag])})))
                  true))))))
        ([_ _] false)))))

(defn- tree-transfer
  "The tags: a tag drags onto another (it moves under it) or onto All (to the
  top); links dropped on a tag get it; a url dropped on a tag is added with it."
  []
  (let [onto (fn [^TransferHandler$TransferSupport s]
               (when (.isDrop s)
                 (when-let [p (.getPath ^JTree$DropLocation (.getDropLocation s))]
                   {:tag (:tag (.getUserObject ^DefaultMutableTreeNode (.getLastPathComponent p)))})))
        real? #(and % (not= % untagged))
        can? (fn [^TransferHandler$TransferSupport s]
               (let [o (onto s)]
                 (cond
                   (.isDataFlavorSupported s tag-flavor)
                   (when-let [from (support-data s tag-flavor)]
                     (and o (or (nil? (:tag o)) (real? (:tag o))) (moved-tag from (:tag o))))
                   (.isDataFlavorSupported s links-flavor) (real? (:tag o))
                   :else (.isDataFlavorSupported s DataFlavor/stringFlavor))))]
    (proxy [TransferHandler] []
      (getSourceActions [_] TransferHandler/MOVE)
      (createTransferable [_]
        (when-let [t (scope-tag)]
          (when-not (#{db/to-read db/done} t) (transferable tag-flavor t t))))
      (canImport
        ([s] (boolean (can? s)))
        ([_ _] false))
      (importData
        ([^TransferHandler$TransferSupport s]
         (boolean
          (when (can? s)
            (let [tag (:tag (onto s))]
              (cond
                (.isDataFlavorSupported s tag-flavor)
                (let [from (support-data s tag-flavor)]
                  (on-edt (guarded #(rename-to! "Moved" from (moved-tag from tag))))
                  true)
                (.isDataFlavorSupported s links-flavor)
                (let [urls (support-data s links-flavor)]
                  (on-edt (guarded #(tag-links! (links-at-urls urls) tag)))
                  true)
                :else
                (when-let [u (url-in (support-data s DataFlavor/stringFlavor))]
                  (let [tag (if (.isDrop s) (when (real? tag) tag) (scope-tag))]
                    (on-edt (guarded #(add! {:url u :tags (keep identity [tag])}))))
                  true))))))
        ([_ _] false)))))

(defn- focus-owner ^JComponent []
  (let [c (.getFocusOwner (KeyboardFocusManager/getCurrentKeyboardFocusManager))]
    (when (instance? JComponent c) c)))

(defn- run-action!
  "The component's own action by name, as its key would run it."
  [^JComponent c action-name]
  (when-let [a (some-> c .getActionMap (.get action-name))]
    (.actionPerformed ^Action a (ActionEvent. c ActionEvent/ACTION_PERFORMED (str action-name)))))

(defn- clipboard-action!
  "Edit > Cut, Copy, Paste: the focused widget's own, so a text field cuts
  and pastes text, the list copies links' urls and pastes a url as a new
  link, the tree copies a tag."
  [action-name]
  (some-> (focus-owner) (run-action! action-name)))

(defn- select-all! []
  (when-let [c (focus-owner)]
    (run-action! c (if (instance? JTextComponent c) "select-all" "selectAll"))))

(defn- text-menu
  "Cut, Copy, Paste and Select All for a text field, each enabled only when
  it can act."
  [^JTextComponent t]
  (let [sel? (some? (.getSelectedText t))
        edit? (and (.isEditable t) (.isEnabled t))
        item (fn [^String label action-name enabled?]
               (doto (JMenuItem. label)
                 (.setEnabled (boolean enabled?))
                 (.addActionListener (reify ActionListener (actionPerformed [_ _] (run-action! t action-name))))))]
    (doto (JPopupMenu.)
      (.add (item "Cut" "cut" (and sel? edit?)))
      (.add (item "Copy" "copy" sel?))
      (.add (item "Paste" "paste" edit?))
      (.addSeparator)
      (.add (item "Select All" "select-all" (pos? (.getLength (.getDocument t))))))))

(defn- show-text-menu!
  "Shift+F10 in the search box: its Cut, Copy, Paste menu under the caret."
  []
  (let [^JTextField t (:search @widgets)
        r (or (.modelToView2D t (.getCaretPosition t)) (Rectangle.))]
    (.show (text-menu t) t (int (.getX r)) (int (+ (.getY r) (.getHeight r))))))

(defonce ^:private text-menus
  ;; a right-click on any text field without a menu of its own, in any dialog too
  (delay (.addAWTEventListener (Toolkit/getDefaultToolkit)
                               (reify AWTEventListener
                                 (eventDispatched [_ e]
                                   (let [^MouseEvent e e
                                         c (.getComponent e)]
                                     (when (and (.isPopupTrigger e) (instance? JTextComponent c)
                                                (nil? (.getComponentPopupMenu ^JComponent c)))
                                       (.requestFocusInWindow ^JComponent c)
                                       (.show (text-menu c) c (.getX e) (.getY e))
                                       (.consume e)))))
                               AWTEvent/MOUSE_EVENT_MASK)))

;; ---------------------------------------------------------------- window, session, restart

(defn- on-screen?
  "Some 100 x 100 of the rect is on a screen there now."
  [[x y w h]]
  (let [r (Rectangle. (int x) (int y) (int w) (int h))]
    (some #(let [i (.intersection r (.getBounds (.getDefaultConfiguration ^java.awt.GraphicsDevice %)))]
             (and (>= (.width i) 100) (>= (.height i) 100)))
          (.getScreenDevices (GraphicsEnvironment/getLocalGraphicsEnvironment)))))

(defn- session-now
  "The window as it is: its file, tag and search, place and divider."
  []
  (let [{:keys [^JFrame frame ^JSplitPane split]} @widgets
        b (.getBounds frame)]
    {:data (some-> (:data-path @state) store/short-path)
     :scope (:scope @state)
     :query (:query @state)
     :bounds [(.x b) (.y b) (.width b) (.height b)]
     :maximized (= JFrame/MAXIMIZED_BOTH (bit-and (.getExtendedState frame) JFrame/MAXIMIZED_BOTH))
     :divider (.getDividerLocation split)}))

(defn- save-on-quit!
  "At every way out: the window into view.edn, and the text size in use
  becomes the default in config.edn."
  []
  (try
    (when (:frame @widgets) (swap! state assoc :session (session-now)))
    (save-view!)
    (let [n (:font-size @state)]
      (when (not= n (:font-size (db/config))) (save-setting! :font-size n)))
    (log/event :quit)
    (catch Exception e (log/event :error :where "quit" :message (str e)))))

(defn- restore-window!
  "Put the window where it was at the last quit, or a default size scaled
  with the text, centred."
  []
  (let [{:keys [^JFrame frame ^JSplitPane split]} @widgets
        {:keys [bounds maximized divider]} (:session @state)]
    (if (and (vector? bounds) (= 4 (count bounds)) (every? int? bounds) (on-screen? bounds))
      (let [[x y w h] bounds] (.setBounds frame (int x) (int y) (int w) (int h)))
      (let [usable (.getMaximumWindowBounds (GraphicsEnvironment/getLocalGraphicsEnvironment))
            [w h] (map #(UIScale/scale (int %)) window-size)]
        (.setSize frame (Dimension. (min w (.width usable)) (min h (.height usable))))
        (.setLocationRelativeTo frame nil)))
    (.setDividerLocation split (int (if (int? divider) divider (UIScale/scale (int tree-width)))))
    (when (true? maximized) (.setExtendedState frame JFrame/MAXIMIZED_BOTH))))

(defn- close-window!
  "Cmd+W, File > Quit: close the window, which quits; the window is saved first."
  []
  (let [^JFrame f (:frame @widgets)]
    (.dispatchEvent f (WindowEvent. f WindowEvent/WINDOW_CLOSING))))

(defn relaunch-command
  "This process's own command line, to start it again: java and its
  arguments from bin/bookmark2, or the app's launcher."
  []
  (let [info (.info (java.lang.ProcessHandle/current))]
    (when-let [cmd (.orElse (.command info) nil)]
      (into [cmd] (.orElse (.arguments info) (make-array String 0))))))

(defn restart!
  "File > Restart: save as at a quit, start this same command again (new code
  and all), and leave. The window comes back as it was."
  []
  (if-let [cmd (relaunch-command)]
    (do (save-on-quit!)
        (log/event :restart)
        (-> (ProcessBuilder. ^java.util.List cmd) .inheritIO .start)
        (System/exit 0))
    (status! "Cannot restart: this process's command line is unknown.")))

(defn- minimize! []
  (let [^JFrame f (:frame @widgets)] (.setExtendedState f (bit-or (.getExtendedState f) JFrame/ICONIFIED))))

(defn- zoom! []
  (let [^JFrame f (:frame @widgets)]
    (.setExtendedState f (if (= JFrame/MAXIMIZED_BOTH (bit-and (.getExtendedState f) JFrame/MAXIMIZED_BOTH))
                           JFrame/NORMAL
                           JFrame/MAXIMIZED_BOTH))))

(declare show-shortcuts! show-context-menu! show-settings!)

(defn choose-theme!
  "Theme...: Settings, on the theme list. Arrows preview each theme in the
  whole window, and the one left chosen stays."
  []
  (show-settings! :appearance))

;; ---------------------------------------------------------------- keys

(defn- app-actions
  "Every menu action: [id label default-keys f]. Keys are KeyStroke strings,
  menu meaning Cmd on macOS and Ctrl elsewhere. The first key is the menu
  accelerator; others work anywhere in the window too. Settings > Keyboard
  changes them."
  []
  [[:add "Add Link..." ["menu N"] add!]
   [:reload "Reload" ["menu R"] reload!]
   [:new-data "New Data File..." ["menu alt N"] new-data-file!]
   [:open-data "Open Data File..." ["menu alt O"] open-data-file!]
   [:default-data "Change Default Data File..." ["menu shift D"] change-default!]
   [:open-file "Open data.rec in Text Editor" ["menu shift O"] open-file!]
   [:reveal (if (os/mac?) "Reveal data.rec in Finder" "Show data.rec in Folder") ["menu shift R"] reveal-file!]
   [:close "Close Window" ["menu W"] close-window!]
   [:restart "Restart" ["menu alt R"] restart!]
   [:quit "Quit" ["menu Q"] close-window!]
   [:undo "Undo" ["menu Z"] undo!]
   [:redo "Redo" ["menu shift Z"] redo!]
   [:cut "Cut" ["menu X"] #(clipboard-action! "cut")]
   [:copy-text "Copy" ["menu C"] #(clipboard-action! "copy")]
   [:paste "Paste" ["menu V"] #(clipboard-action! "paste")]
   [:select-all "Select All" ["menu A"] select-all!]
   [:edit "Edit..." ["menu E"] edit!]
   [:tag "Add Tag..." [] add-tag!]
   [:delete "Delete" ["menu BACK_SPACE"] delete-selected!]
   [:copy "Copy URL" ["menu shift C"] copy-selected!]
   [:toggle-read "Mark Read / To Read" ["menu D"] toggle-read!]
   [:rename "Rename Tag..." ["F2"] rename-tag!]
   [:settings "Settings..." ["menu COMMA"] show-settings!]
   [:find "Search" ["menu F"] focus-search!]
   [:tags "Go to Tags" ["menu 1"] focus-tree!]
   [:links "Go to Links" ["menu 2"] focus-list!]
   [:open "Open in Browser" ["menu O"] open-selected!]
   [:menu "Show Context Menu" ["menu J"] show-context-menu!]
   [:bigger "Bigger Text" ["menu EQUALS" "menu shift EQUALS"] #(change-font-size! 1)]
   [:smaller "Smaller Text" ["menu MINUS"] #(change-font-size! -1)]
   [:actual "Actual Size" ["menu 0"] #(set-font-size! default-font-size)]
   [:theme "Theme..." ["menu T"] choose-theme!]
   [:next-theme "Next Theme" ["menu CLOSE_BRACKET"] #(cycle-theme! 1)]
   [:prev-theme "Previous Theme" ["menu OPEN_BRACKET"] #(cycle-theme! -1)]
   [:minimize "Minimize" ["menu M"] minimize!]
   [:zoom "Zoom" [] zoom!]
   [:help "Keyboard Shortcuts" ["menu SLASH"] show-shortcuts!]])

(defn- available?
  "macOS puts Settings and Quit in its own app menu, with Cmd+, and Cmd+Q;
  the Window menu (Minimize, Zoom) is macOS only."
  [id]
  (if (os/mac?) (not (#{:quit :settings} id)) (not (#{:minimize :zoom} id))))

(defn- menus []
  (->> [["File" KeyEvent/VK_F [:add :reload :sep :new-data :open-data :default-data :sep :open-file :reveal
                               :sep :close :restart :quit]]
        ["Edit" KeyEvent/VK_E [:undo :redo :sep :cut :copy-text :paste :select-all :sep :edit :tag :delete :copy
                               :toggle-read :sep :rename :settings]]
        ["View" KeyEvent/VK_V [:find :tags :links :sep :open :menu :sep :bigger :smaller :actual :sep
                               :theme :next-theme :prev-theme]]
        ["Window" KeyEvent/VK_W [:minimize :zoom]]
        ["Help" KeyEvent/VK_H [:help]]]
       (map (fn [[title mnemonic ks]] [title mnemonic (filterv #(or (= % :sep) (available? %)) ks)]))
       (filter (fn [[_ _ ks]] (some #(not= % :sep) ks)))))

(defn- pane-keys
  "Single keys inside one pane: [id description default-keys f], keys as
  KeyStroke strings. An entry without id and f is fixed: Swing or the z
  listener does it. This table drives the bindings, the help, the status
  bar's hints and Settings > Keyboard."
  []
  {:search [[:to-list "Go to the links" ["DOWN"] focus-list!]
            [:open "Open the selected link" ["ENTER"] open-selected!]
            [:escape "Clear the search; when empty, go to the links" ["ESCAPE"] search-escape!]
            [:menu "Cut, copy, paste menu" ["shift F10" "CONTEXT_MENU"] show-text-menu!]
            [nil "Next, previous pane" ["TAB" "shift TAB"] nil]]
   :tree [[:prev "Previous tag" ["UP" "typed k"] #(move-tree! -1)]
          [:next "Next tag" ["DOWN" "typed j"] #(move-tree! 1)]
          [nil "Collapse, expand a branch" ["LEFT" "RIGHT"] nil]
          [:links "Go to its links" ["ENTER" "typed l"] focus-list!]
          [:add "Add a link with this tag" ["typed a"] add!]
          [:undo "Undo" ["typed u"] undo!]
          [:rename "Rename this tag (a / moves it in the tree)" ["F2"] rename-tag!]
          [:search "Search" ["typed /" "ESCAPE"] focus-search!]
          [:menu "Context menu" ["shift F10" "CONTEXT_MENU"] #(show-context-menu! :tree)]
          [:help "Keyboard shortcuts" ["typed ?"] show-shortcuts!]]
   :list [[:prev "Previous row (links and tag headers)" ["UP" "typed k"] #(move-row! -1)]
          [:next "Next row" ["DOWN" "typed j"] #(move-row! 1)]
          [:extend-up "Grow the selection up" ["typed K"] #(extend-row! -1)]
          [:extend-down "Grow the selection down" ["typed J"] #(extend-row! 1)]
          [nil "Grow the selection up, down" ["shift UP" "shift DOWN"] nil]
          [:page-up "A page up" ["PAGE_UP"] #(page! -1)]
          [:page-down "A page down" ["PAGE_DOWN"] #(page! 1)]
          [:first "First row" ["HOME" "typed g"] #(select-row! 0)]
          [:last "Last row" ["END" "typed G"] #(select-row! Integer/MAX_VALUE)]
          [:open "Open the selected links; on a folded header, unfold it" ["ENTER"] list-enter!]
          [:read "Mark read, or back to To Read" ["typed r"] toggle-read!]
          [:fold "Fold or unfold this tag" ["SPACE"] #(fold! :toggle)]
          [nil "Folds: toggle, fold, unfold this tag; fold all, unfold all" ["za" "zc" "zo" "zM" "zR"] nil]
          [:sort "Sort: newest first, Title, URL, Tags in turn" ["typed s"] cycle-sort!]
          [:flip "Flip the sort between ascending and descending" ["typed S"] flip-sort!]
          [:edit "Edit the link: url, title, tags, note" ["typed e"] edit!]
          [:tag "Add a tag to the selected links" ["typed t"] add-tag!]
          [:copy "Copy the selected links' URLs" ["typed y"] copy-selected!]
          [:delete "Delete the selected links (from every tag)" ["DELETE" "BACK_SPACE"] delete-selected!]
          [:undo "Undo" ["typed u"] undo!]
          [:add "Add a link" ["typed a"] add!]
          [:rename "Rename this tag (on a header or a link in it)" ["F2"] rename-tag!]
          [:left "To the link's tag header; from a header, to the tags" ["LEFT" "typed h"] list-left!]
          [:search "Search" ["typed /"] focus-search!]
          [:escape "With several selected, keep one; else search" ["ESCAPE"] list-escape!]
          [:menu "Context menu" ["shift F10" "CONTEXT_MENU"] #(show-context-menu! :list)]
          [:help "Keyboard shortcuts" ["typed ?"] show-shortcuts!]]})

(def ^:private panes [[:search "Search box"] [:tree "Tags (left)"] [:list "Links (right)"]])

(defn- resolve-key [s] (str/replace (str s) #"\bmenu\b" (if (os/mac?) "meta" "ctrl")))

(defn- stroke ^KeyStroke [s] (KeyStroke/getKeyStroke ^String (resolve-key s)))

(def ^:private key-names
  {"ESCAPE" "Esc" "BACK_SPACE" "Backspace" "PAGE_UP" "Page Up" "PAGE_DOWN" "Page Down"
   "CONTEXT_MENU" "Menu key" "TAB" "Tab" "ENTER" "Enter" "DELETE" "Delete" "HOME" "Home" "END" "End"
   "UP" "Up" "DOWN" "Down" "LEFT" "Left" "RIGHT" "Right" "SPACE" "Space" "EQUALS" "=" "MINUS" "-"
   "COMMA" "," "PERIOD" "." "SLASH" "/" "BACK_SLASH" "\\" "OPEN_BRACKET" "[" "CLOSE_BRACKET" "]"
   "SEMICOLON" ";" "QUOTE" "'" "BACK_QUOTE" "`"})

(defn key-text
  "How a KeyStroke string reads: \"typed j\" -> j, \"meta pressed N\" ->
  Cmd+N, \"shift F10\" -> Shift+F10, a z sequence (\"za\") as it is."
  [s]
  (let [s (resolve-key s)]
    (if-let [[_ c] (re-matches #"typed (.)" s)]
      c
      (let [words (str/split (str/trim s) #"\s+")
            k (last words)
            mods (set (butlast words))]
        (if (and (empty? mods) (nil? (KeyStroke/getKeyStroke ^String s)))
          s
          (str/join "+" (concat (for [[m label] [["meta" "Cmd"] ["ctrl" "Ctrl"] ["alt" (if (os/mac?) "Option" "Alt")]
                                                 ["shift" "Shift"]]
                                      :when (mods m)]
                                  label)
                                [(get key-names k (if (re-matches #"[A-Z0-9]|F\d+" k)
                                                    k
                                                    (str/capitalize (str/replace k "_" " "))))])))))))

(defn- key-id [scope id] (str (name scope) "/" (name id)))

(defn- keys-of
  "The keys of action id in scope: the user's from Settings, else the defaults."
  [scope id defaults]
  (get (:keys @state) (key-id scope id) (mapv resolve-key defaults)))

(defn shortcut-ok?
  "A menu shortcut works everywhere, so it needs Cmd, Ctrl or Alt, or is a
  function key: a bare letter there would eat typing in the search box."
  [s]
  (when-let [ks (stroke s)]
    (or (pos? (bit-and (.getModifiers ks) (bit-or InputEvent/META_DOWN_MASK InputEvent/CTRL_DOWN_MASK
                                                  InputEvent/ALT_DOWN_MASK)))
        (<= KeyEvent/VK_F1 (.getKeyCode ks) KeyEvent/VK_F12))))

(defn key-conflict
  "The entry other than scope/id that already uses key k where they would
  clash: in the same pane, or anywhere when either is a menu shortcut.
  entries: maps with :scope :id :keys."
  [entries scope id k]
  (let [ks (stroke k)]
    (first (filter (fn [e] (and (not (and (= scope (:scope e)) (= id (:id e))))
                                (or (= scope (:scope e)) (= :app scope) (= :app (:scope e)))
                                (some #(= ks (stroke %)) (:keys e))))
                   entries))))

(defn- key-entries
  "Every action whose keys Settings can change: {:scope :where :id :label :defaults :keys}."
  []
  (vec (concat
        (for [[id label ks f] (app-actions) :when (and f (available? id))]
          {:scope :app :where "Anywhere" :id id :label label :defaults (mapv resolve-key ks) :keys (keys-of :app id ks)})
        (for [[pane where] panes
              [id label ks f] (get (pane-keys) pane) :when (and id f)]
          {:scope pane :where where :id id :label label :defaults (mapv resolve-key ks) :keys (keys-of pane id ks)}))))

(defn- action [label f]
  (let [body (guarded f)]
    (proxy [AbstractAction] [label]
      (actionPerformed [_] (body)))))

(defn- bind-key!
  "Bind ks on c. A two-key sequence like \"za\" is no KeyStroke: the z-prefix
  listener handles those, so they are skipped here."
  [^JComponent c ^String ks f & [condition]]
  (when-let [s (stroke ks)]
    (let [id (str "bookmark2 " s)]
      (.put (.getInputMap c (int (or condition JComponent/WHEN_FOCUSED))) s id)
      (.put (.getActionMap c) id (proxy [AbstractAction] [] (actionPerformed [_] ((guarded f))))))))

(defn- unbind! [^JComponent c condition]
  (let [im (.getInputMap c (int condition))]
    (doseq [k (or (.keys im) []) :when (str/starts-with? (str (.get im k)) "bookmark2 ")]
      (.remove im k))))

(def ^:private hint-actions
  {:search [[:to-list "links"] [:open "open"] [:escape "clear"]]
   :tree [[:next "move"] [:links "links"] [:add "add here"] [:rename "rename"] [:search "search"] [:menu "menu"]
          [:help "keys"]]
   :list [[:open "open"] [:read "read"] [:fold "fold"] [:sort "sort"] [:edit "edit"] [:tag "tag"] [:delete "delete"]
          [:rename "rename"] [:search "search"] [:help "keys"]]})

(defn- hint
  "The status bar's keys for a pane, from the keys in use: the shortest name
  of each action's keys."
  [pane]
  (let [defaults (into {} (for [[id _ ks _] (get (pane-keys) pane) :when id] [id ks]))
        help (first (keys-of :app :help (some (fn [[id _ ks]] (when (= id :help) ks)) (app-actions))))]
    (str/join "   " (concat (for [[id word] (hint-actions pane)
                                  :let [ks (keys-of pane id (defaults id))] :when (seq ks)]
                              (str (first (sort-by count (map key-text ks))) " " word))
                            (when (= pane :search)
                              (cons "Tab next pane" (when help [(str (key-text help) " keys")])))))))

(defn- show-hint! [pane]
  (swap! widgets assoc :pane pane)
  (some-> ^JLabel (:hint @widgets) (.setText (hint pane))))

(defn- apply-keys!
  "Bind every pane's single keys and every menu shortcut from the defaults
  and Settings > Keyboard; earlier bindings go first, so this rebinds."
  []
  (let [{:keys [^JFrame frame actions buttons] :as ws} @widgets
        root (.getRootPane frame)]
    (doseq [[k pane] [[:search :search] [:tree :tree] [:table :list]]
            :let [^JComponent c (ws k)]]
      (unbind! c JComponent/WHEN_FOCUSED)
      (doseq [[id _ ks f] (get (pane-keys) pane) :when f, k (keys-of pane id ks)]
        (bind-key! c k f)))
    (unbind! root JComponent/WHEN_IN_FOCUSED_WINDOW)
    (doseq [[id label ks f] (app-actions) :when (available? id)
            :let [^Action a (actions id)
                  [k & more] (keys-of :app id ks)]]
      (.putValue a Action/ACCELERATOR_KEY (some-> k stroke))
      (doseq [k more] (bind-key! root k f JComponent/WHEN_IN_FOCUSED_WINDOW)))
    (doseq [[id ^JButton b] buttons
            :let [[k] (keys-of :app id (some (fn [[i _ ks]] (when (= i id) ks)) (app-actions)))]]
      (.setToolTipText b (str (.getValue ^Action (actions id) Action/NAME) (when k (str "  " (key-text k))))))
    (show-hint! (or (:pane @widgets) :search))))

(defn- shortcuts-html []
  (let [row (fn [k d] (str "<tr><td><b>" (esc-html k) "</b>&nbsp;&nbsp;&nbsp;</td><td>" (esc-html d) "</td></tr>"))
        section (fn [title rows] (str "<tr><td colspan=2><br><b><u>" title "</u></b></td></tr>" (str/join rows)))
        menu-bar (if (os/mac?) "Ctrl+F2 (macOS: Keyboard Navigation on)" "F10, or Alt+the underlined letter")
        keys-text #(str/join " or " (map key-text %))]
    (str "<html><body style='padding:6px'><table>"
         (section "Anywhere"
                  (concat (for [[id label ks f] (app-actions) :when (and f (available? id))
                                :let [kk (keys-of :app id ks)] :when (seq kk)]
                            (row (keys-text kk) label))
                          (when (os/mac?) [(row "Cmd+," "Settings...") (row "Cmd+Q" "Quit")])))
         (str/join (for [[pane title] panes]
                     (section title (for [[id d ks _] (get (pane-keys) pane)]
                                      (row (keys-text (if id (keys-of pane id ks) ks)) d)))))
         (section "Mouse"
                  [(row "Click, Shift+click, Cmd+click" "Select one link, a run of them, or add one")
                   (row "Double-click" "Open the link")
                   (row "Click a tag header" "Fold or unfold it")
                   (row "Drag links onto a tag" "Add that tag to them, in the tree or on a header")
                   (row "Drag a tag onto another" "Move it under that tag; onto All, to the top")
                   (row "Drop a URL" "From a browser: add it, with the tag it lands on")
                   (row "Drag links out" "Their URLs, as text")
                   (row "Right-click" "The context menu; in a text field, cut, copy, paste")])
         (section "Dialogs"
                  [(row "Tab / Shift+Tab" "Next, previous field")
                   (row "Enter" "OK; in the note, a new line")
                   (row ok-key-name "OK, from any field")
                   (row "Esc" "Cancel")
                   (row "Down / Up" "In a tag field: list known tags, move in the list; Enter takes one, Esc closes it")
                   (row "Alt+letter" "The underlined field or button (Ctrl+Option on macOS)")])
         (section "Menus"
                  [(row "Menu bar" menu-bar)
                   (row "Arrows, Enter, Esc" "Move, choose, close")])
         "</table><br>Data: " (esc-html (:data-path @state))
         "<br>Settings: " (esc-html (db/config-file))
         "<br>Log: " (esc-html (log/log-file)) "</body></html>")))

(defn show-shortcuts! []
  (let [view (doto (JEditorPane. "text/html" (shortcuts-html))
               (.setEditable false)
               ;; the window's own font and text size, not HTML's
               (.putClientProperty JEditorPane/HONOR_DISPLAY_PROPERTIES true)
               (.setCaretPosition 0))
        scroll (doto (JScrollPane. view) (.setPreferredSize (Dimension. (UIScale/scale (int 720)) (UIScale/scale (int 560)))))
        pane (JOptionPane. scroll JOptionPane/PLAIN_MESSAGE)
        dialog (.createDialog pane (:frame @widgets) "Keyboard Shortcuts")]
    ;; focus the text so arrows and Page Up/Down scroll it; Esc or Enter closes
    (.addWindowListener dialog (proxy [WindowAdapter] [] (windowOpened [_] (.requestFocusInWindow view))))
    (bind-key! view "ESCAPE" #(.dispose dialog))
    (bind-key! view "ENTER" #(.dispose dialog))
    (.setVisible dialog true)))

;; ---------------------------------------------------------------- context menus

(defn- popup-for
  "The context menu for :tree or :list, from what is selected there."
  [pane]
  (let [actions (:actions @widgets)
        items (case pane
                :list (cond
                        (selected-entry) [:open :copy :toggle-read :edit :tag :delete :sep :rename]
                        (seq (selected-links)) [:open :copy :toggle-read :tag :delete]
                        (selected-row) [:rename :add]
                        :else [:add])
                :tree (if (scope-tag) [:add :rename :sep :open-file :reveal] [:add :sep :open-file :reveal]))
        menu (JPopupMenu.)]
    (doseq [k items]
      (if (= k :sep) (.addSeparator menu) (.add menu ^Action (actions k))))
    menu))

(defn- show-popup!
  "Show menu at x y of c. From the keyboard, the first item is selected so
  arrows and Enter work at once."
  [^JPopupMenu menu ^JComponent c x y keyboard?]
  (when (pos? (.getComponentCount menu))
    (.show menu c (int x) (int y))
    (when keyboard?
      (.setSelectedPath (MenuSelectionManager/defaultManager)
                        (into-array MenuElement [menu (.getComponent menu 0)])))))

(defn show-context-menu!
  "The focused pane's context menu, under its selected row."
  ([] (show-context-menu! (if (.isFocusOwner ^JTree (:tree @widgets)) :tree :list)))
  ([pane]
   (let [{:keys [^JTree tree ^JTable table]} @widgets]
     (if (= pane :tree)
       (let [r (or (.getRowBounds tree (.getLeadSelectionRow tree)) (Rectangle.))]
         (show-popup! (popup-for :tree) tree (+ (.x r) 24) (+ (.y r) (.height r)) true))
       (let [r (.getCellRect table (max 0 (lead-index)) 0 true)]
         (show-popup! (popup-for :list) table (+ (.x r) 40) (+ (.y r) (.height r)) true))))))

(def ^:private hand (Cursor/getPredefinedCursor Cursor/HAND_CURSOR))

(defn- mouse-handler
  "Mouse on a pane: a right-click selects what is under the pointer (keeping
  a selection it is part of), then shows its menu. In the list, a plain click
  on a tag header folds it, a double click opens a link. The row under the
  pointer is lit, and the pointer is a hand over what one click acts on."
  [pane]
  (let [show (fn [^MouseEvent e]
               (when (.isPopupTrigger e)
                 (let [{:keys [^JTree tree ^JTable table]} @widgets]
                   (if (= pane :tree)
                     (when-let [p (.getPathForLocation tree (.getX e) (.getY e))]
                       (.setSelectionPath tree p))
                     (let [row (.rowAtPoint table (.getPoint e))]
                       (when (and (>= row 0) (not (.isRowSelected table row)))
                         (.setRowSelectionInterval table row row))))
                   (show-popup! (popup-for pane) (.getComponent e) (.getX e) (.getY e) false))))
        moved (fn [^MouseEvent e]
                (let [{:keys [^JTree tree ^JTable table]} @widgets]
                  (if (= pane :tree)
                    (let [r (.getClosestRowForLocation tree (.getX e) (.getY e))
                          b (when (>= r 0) (.getRowBounds tree r))
                          r (when (and b (<= (.y b) (.getY e) (+ (.y b) (.height b)))) r)]
                      (hover! :tree tree r #(.getRowBounds tree (int %)))
                      (.setCursor tree (if r hand (Cursor/getDefaultCursor))))
                    (let [r (.rowAtPoint table (.getPoint e))
                          r (when (>= r 0) r)]
                      (hover! :table table r #(.getCellRect table (int %) 0 true))
                      (.setCursor table (if (:header? (get (:rows @state) r)) hand (Cursor/getDefaultCursor)))))))]
    (proxy [MouseAdapter] []
      (mousePressed [e] (show e))
      (mouseReleased [e] (show e))
      (mouseMoved [e] (moved e))
      (mouseDragged [e] (moved e))
      (mouseExited [_]
        (let [{:keys [^JTree tree ^JTable table]} @widgets]
          (if (= pane :tree)
            (hover! :tree tree nil #(.getRowBounds tree (int %)))
            (hover! :table table nil #(.getCellRect table (int %) 0 true)))))
      (mouseClicked [^MouseEvent e]
        (when (and (= pane :list) (SwingUtilities/isLeftMouseButton e))
          (cond
            (and (= 1 (.getClickCount e)) (zero? (bit-and (.getModifiersEx e) (bit-or InputEvent/SHIFT_DOWN_MASK InputEvent/META_DOWN_MASK InputEvent/CTRL_DOWN_MASK)))
                 (:header? (selected-row)))
            (fold! :toggle)
            (and (= 2 (.getClickCount e)) (not (:header? (selected-row)))) (open-selected!)))))))

;; ---------------------------------------------------------------- settings

(defn- heading [^String s]
  (doto (JLabel. s) (.putClientProperty "FlatLaf.styleClass" "h2")))

(defn- remark [^String s]
  (doto (JLabel. s)
    (.putClientProperty "FlatLaf.styleClass" "small")
    (.putClientProperty "FlatLaf.style" "foreground: $Label.disabledForeground")))

(defn- path-field [s]
  (doto (JTextField. (str s) 40)
    (.setEditable false)
    (.putClientProperty "FlatLaf.styleClass" "monospaced")))

(defn- on-change [f] (reify ChangeListener (stateChanged [_ e] (f e))))
(defn- on-action [f] (reify ActionListener (actionPerformed [_ e] (f e))))

(defn- appearance-page
  "Theme (arrows preview it in every window), text size, and motion."
  []
  (let [ts (themes)
        lst (doto (JList. ^"[Ljava.lang.Object;" (into-array Object (map first ts)))
              (.setSelectionMode ListSelectionModel/SINGLE_SELECTION)
              (.setVisibleRowCount 10))
        start (or (first (keep-indexed #(when (= (second %2) (current-theme)) %1) ts)) 0)
        [lo hi] font-size-range
        slider (doto (JSlider. (int lo) (int hi) (int (:font-size @state)))
                 (.setMajorTickSpacing 10)
                 (.setMinorTickSpacing 1)
                 (.setPaintTicks true)
                 (.setPaintLabels true))
        size-label (JLabel. (str (:font-size @state) " pt"))
        motion (JCheckBox. "Fades and glides" (boolean (:animations @state)))
        mods (if (os/mac?) "Cmd" "Ctrl")
        panel (JPanel. (GridBagLayout.))]
    (.setSelectedIndex lst start)
    (.ensureIndexIsVisible lst start)
    (.addListSelectionListener lst (reify ListSelectionListener
                                     (valueChanged [_ e]
                                       (let [i (.getSelectedIndex lst)]
                                         (when (and (not (.getValueIsAdjusting e)) (>= i 0))
                                           ;; switch after this event: a theme switch rebuilds the list's
                                           ;; own UI, which is still handling the selection now; then give
                                           ;; focus back, since the rebuild moves it
                                           (on-edt (fn []
                                                     (let [cls (second (nth ts i))]
                                                       (when (not= cls (current-theme))
                                                         ((guarded #(set-theme! cls)))))
                                                     (.requestFocusInWindow lst))))))))
    (.addChangeListener slider (on-change (fn [_]
                                            (.setText size-label (str (.getValue slider) " pt"))
                                            (when-not (.getValueIsAdjusting slider)
                                              (let [n (.getValue slider)]
                                                (when (not= n (:font-size @state))
                                                  (on-edt (fn []
                                                            ((guarded #(set-font-size! n)))
                                                            (.requestFocusInWindow slider)))))))))
    (.addActionListener motion (on-action (fn [_] (set-animations! (.isSelected motion)))))
    (doto panel
      (.add (heading "Appearance") (gbc 0 0 1 :width 3))
      (.add (JLabel. "Theme") (gbc 0 1 0 :anchor GridBagConstraints/NORTHWEST))
      (.add (JScrollPane. lst) (gbc 1 1 1 :width 2 :fill GridBagConstraints/BOTH))
      (.add (remark "Dark themes only. Arrows preview each one in the whole window.") (gbc 1 2 1 :width 2))
      (.add (JLabel. "Text size") (gbc 0 3 0))
      (.add slider (gbc 1 3 1))
      (.add size-label (gbc 2 3 0))
      (.add (remark (str mods "+= bigger, " mods "+- smaller, " mods "+0 back to " default-font-size
                         " pt. The size in use at quit is the next start's."))
            (gbc 1 4 1 :width 2))
      (.add (JLabel. "Motion") (gbc 0 5 0))
      (.add motion (gbc 1 5 1 :width 2))
      (.add (remark "Theme cross-fades, gliding jumps, fading menus, smooth scrolling.") (gbc 1 6 1 :width 2)))
    {:panel panel :focus lst}))

(defn- capture-key
  "A small dialog that waits for one key: its KeyStroke string, or nil on
  Esc. In a pane (pane? true) a plain character is a typed key, as j is."
  [^Window owner ^String label pane?]
  (let [d (JDialog. owner "Press a key" Dialog$ModalityType/APPLICATION_MODAL)
        result (atom nil)
        modifier-keys #{KeyEvent/VK_SHIFT KeyEvent/VK_CONTROL KeyEvent/VK_ALT KeyEvent/VK_META KeyEvent/VK_ALT_GRAPH}
        disp (reify KeyEventDispatcher
               (dispatchKeyEvent [_ e]
                 (boolean
                  (when (.isFocused d)
                    (when (= KeyEvent/KEY_PRESSED (.getID e))
                      (let [code (.getKeyCode e)
                            mods (.getModifiersEx e)
                            ch (.getKeyChar e)
                            mod? (pos? (bit-and mods (bit-or InputEvent/META_DOWN_MASK InputEvent/CTRL_DOWN_MASK
                                                             InputEvent/ALT_DOWN_MASK)))]
                        (cond
                          (modifier-keys code) nil
                          (and (= code KeyEvent/VK_ESCAPE) (zero? mods)) (.dispose d)
                          :else (do (reset! result
                                            (if (and pane? (not mod?) (not= ch KeyEvent/CHAR_UNDEFINED)
                                                     (< 32 (int ch) 127))
                                              (str "typed " ch)
                                              (str (KeyStroke/getKeyStroke (int code) (int mods)))))
                                    (.dispose d)))))
                    true))))
        kfm (KeyboardFocusManager/getCurrentKeyboardFocusManager)]
    (doto (.getContentPane d)
      (.setLayout (BorderLayout. 0 (UIScale/scale (int 8))))
      (.add (doto (JLabel. (str "Press the new key for: " label)) (.putClientProperty "FlatLaf.styleClass" "h4"))
            BorderLayout/CENTER)
      (.add (remark "Esc cancels.") BorderLayout/SOUTH))
    (.setBorder ^JComponent (.getContentPane d) (BorderFactory/createEmptyBorder 16 20 16 20))
    (.pack d)
    (.setLocationRelativeTo d owner)
    (.addKeyEventDispatcher kfm disp)
    (try (.setVisible d true) (finally (.removeKeyEventDispatcher kfm disp)))
    @result))

(defn- set-keys!
  "Give entry these keys, save the difference from the defaults, rebind."
  [{:keys [scope id defaults]} ks]
  (swap! state update :keys #(if (= (vec ks) defaults) (dissoc % (key-id scope id)) (assoc % (key-id scope id) (vec ks))))
  (save-setting! :keys (not-empty (:keys @state)))
  (apply-keys!))

(defn- keyboard-page
  "Every action and its keys, one key a row; change, add, remove, reset."
  [^Window owner]
  (let [rows (atom [])
        reload-rows! #(reset! rows (vec (mapcat (fn [e] (if (seq (:keys e))
                                                           (for [k (:keys e)] {:entry e :key k})
                                                           [{:entry e :key nil}]))
                                                (key-entries))))
        model (proxy [AbstractTableModel] []
                (getRowCount [] (count @rows))
                (getColumnCount [] 3)
                (getColumnName [c] (["Where" "Action" "Key"] c))
                (getValueAt [r c] (let [{:keys [entry key]} (get @rows r)]
                                    (case (int c) 0 (:where entry) 1 (:label entry) (if key (key-text key) "-"))))
                (isCellEditable [_ _] false))
        t (doto (JTable. ^AbstractTableModel model)
            (.setSelectionMode ListSelectionModel/SINGLE_SELECTION)
            (.setFillsViewportHeight true))
        refresh! (fn [entry k]
                   (let [i (.getSelectedRow t)]
                     (reload-rows!)
                     (.fireTableDataChanged ^AbstractTableModel model)
                     (let [j (or (first (keep-indexed (fn [j r] (when (and (= (:id entry) (:id (:entry r)))
                                                                            (= (:scope entry) (:scope (:entry r)))
                                                                            (or (nil? k) (= k (:key r))))
                                                                   j))
                                                      @rows))
                                 (max 0 i))]
                       (when (pos? (.getRowCount t))
                         (.setRowSelectionInterval t j j)
                         (.scrollRectToVisible t (.getCellRect t j 0 true))))))
        current (fn [] (get @rows (.getSelectedRow t)))
        assign! (fn [add?]
                  (when-let [{:keys [entry key]} (current)]
                    (when-let [k (capture-key owner (:label entry) (not= :app (:scope entry)))]
                      (if (and (= :app (:scope entry)) (not (shortcut-ok? k)))
                        (JOptionPane/showMessageDialog owner "A menu shortcut works everywhere, so it needs Cmd, Ctrl or Alt, or a function key."
                                                       app-name JOptionPane/WARNING_MESSAGE)
                        (let [other (key-conflict (key-entries) (:scope entry) (:id entry) k)]
                          (when (or (nil? other)
                                    (= JOptionPane/YES_OPTION
                                       (JOptionPane/showConfirmDialog owner (str (key-text k) " is already \"" (:label other) "\" ("
                                                                                 (:where other) "). Use it here instead?")
                                                                      app-name JOptionPane/YES_NO_OPTION)))
                            (when other
                              (set-keys! other (vec (remove #(= (stroke k) (stroke %)) (:keys other)))))
                            (set-keys! entry (cond add? (conj (:keys entry) k)
                                                   key (mapv #(if (= % key) k %) (:keys entry))
                                                   :else [k]))
                            (refresh! entry k)))))))
        button (fn [^String label f]
                 (doto (JButton. label)
                   (.setCursor hand)
                   (.addActionListener (on-action (fn [_] ((guarded f)))))))
        buttons (doto (JPanel. (FlowLayout. FlowLayout/LEFT 0 0))
                  (.add (button "Change Key..." #(assign! false)))
                  (.add (Box/createHorizontalStrut (UIScale/scale (int 8))))
                  (.add (button "Add Key..." #(assign! true)))
                  (.add (Box/createHorizontalStrut (UIScale/scale (int 8))))
                  (.add (button "Remove Key" #(when-let [{:keys [entry key]} (current)]
                                                (when key
                                                  (set-keys! entry (vec (remove #{key} (:keys entry))))
                                                  (refresh! entry nil)))))
                  (.add (Box/createHorizontalStrut (UIScale/scale (int 8))))
                  (.add (button "Reset" #(when-let [{:keys [entry]} (current)]
                                           (set-keys! entry (:defaults entry))
                                           (refresh! entry nil))))
                  (.add (Box/createHorizontalStrut (UIScale/scale (int 8))))
                  (.add (button "Reset All" #(do (swap! state assoc :keys {})
                                                 (save-setting! :keys nil)
                                                 (apply-keys!)
                                                 (refresh! nil nil)))))
        panel (JPanel. (GridBagLayout.))]
    (reload-rows!)
    (.fireTableDataChanged ^AbstractTableModel model)
    (when (pos? (.getRowCount t)) (.setRowSelectionInterval t 0 0))
    (doseq [[i w] (map-indexed vector [130 380 170])]
      (.setPreferredWidth (.getColumn (.getColumnModel t) (int i)) (int (UIScale/scale (int w)))))
    (bind-key! t "ENTER" #(assign! false))
    (bind-key! t "typed a" #(assign! true))
    (.addMouseListener t (proxy [MouseAdapter] []
                           (mouseClicked [^MouseEvent e] (when (= 2 (.getClickCount e)) ((guarded #(assign! false)))))))
    (doto panel
      (.add (heading "Keyboard") (gbc 0 0 1))
      (.add (remark "Enter or a double click changes the key, a adds one. Menu shortcuts need Cmd, Ctrl or Alt.")
            (gbc 0 1 1))
      (.add (JScrollPane. t) (gbc 0 2 1 :fill GridBagConstraints/BOTH))
      (.add buttons (gbc 0 3 1)))
    {:panel panel :focus t}))

(defn- data-page
  "Where the links and the settings live, and the default file."
  []
  (let [default-field (path-field (store/short-path (db/default-path)))
        window-field (path-field (store/short-path (:data-path @state)))
        change (doto (JButton. "Change Default...") (.setCursor hand))
        panel (JPanel. (GridBagLayout.))]
    (.addActionListener change (on-action (fn [_]
                                            ((guarded change-default!))
                                            (.setText default-field (store/short-path (db/default-path)))
                                            (.setText window-field (store/short-path (:data-path @state))))))
    (doto panel
      (.add (heading "Data") (gbc 0 0 1 :width 3))
      (.add (JLabel. "Default file") (gbc 0 1 0))
      (.add default-field (gbc 1 1 1))
      (.add change (gbc 2 1 0))
      (.add (remark "Where bookmark2 and the command line look first. File > Open Data File... shows another file here only.")
            (gbc 1 2 1 :width 2))
      (.add (JLabel. "This window") (gbc 0 3 0))
      (.add window-field (gbc 1 3 1 :width 2))
      (.add (JLabel. "Settings") (gbc 0 4 0))
      (.add (path-field (store/short-path (db/config-file))) (gbc 1 4 1 :width 2))
      (.add (JLabel. "Window state") (gbc 0 5 0))
      (.add (path-field (store/short-path (log/state-dir))) (gbc 1 5 1 :width 2))
      (.add (remark "view.edn (sort, window), folds.edn, log.jsonl") (gbc 1 6 1 :width 2))
      (.add (Box/createVerticalGlue) (gbc 0 7 1 :width 3 :fill GridBagConstraints/BOTH)))
    {:panel panel :focus change}))

(defn show-settings!
  "Settings: categories in a side bar on the left, the chosen one's settings
  on the right. Every change applies at once and is saved; Esc or Close leaves."
  ([] (show-settings! nil))
  ([page]
   (let [^JFrame frame (:frame @widgets)
         d (JDialog. frame "Settings" true)
         pages [[:appearance "Appearance" (appearance-page)]
                [:keyboard "Keyboard" (keyboard-page d)]
                [:data "Data" (data-page)]]
         layout (CardLayout.)
         cards (JPanel. layout)
         side (doto (JList. ^"[Ljava.lang.Object;" (into-array Object (map second pages)))
                (.setSelectionMode ListSelectionModel/SINGLE_SELECTION)
                (.setFixedCellHeight (UIScale/scale (int 34)))
                (.setFixedCellWidth (UIScale/scale (int 170)))
                (.setCursor hand)
                (.setBorder (BorderFactory/createEmptyBorder 8 8 8 8)))
         close (doto (JButton. "Close") (.setCursor hand))
         start (max 0 (.indexOf ^java.util.List (mapv first pages) page))]
     (doseq [[k _ {:keys [^JPanel panel]}] pages]
       (.setBorder panel (BorderFactory/createEmptyBorder 12 16 12 16))
       (.add cards (doto (JScrollPane. panel) (.setBorder (BorderFactory/createEmptyBorder))) (name k)))
     (.addListSelectionListener side (reify ListSelectionListener
                                       (valueChanged [_ _]
                                         (let [i (.getSelectedIndex side)]
                                           (when (>= i 0) (.show layout cards (name (first (nth pages i)))))))))
     (.setSelectedIndex side start)
     (.addActionListener close (on-action (fn [_] (.dispose d))))
     (doto (.getContentPane d)
       (.setLayout (BorderLayout.))
       (.add (doto (JPanel. (BorderLayout.))
               (.add side BorderLayout/CENTER)
               (.add (JSeparator. SwingConstants/VERTICAL) BorderLayout/EAST))
             BorderLayout/WEST)
       (.add cards BorderLayout/CENTER)
       (.add (doto (JPanel. (BorderLayout.))
               (.add (JSeparator.) BorderLayout/NORTH)
               (.add (doto (JPanel. (FlowLayout. FlowLayout/RIGHT)) (.add close)) BorderLayout/CENTER))
             BorderLayout/SOUTH))
     (bind-key! (.getRootPane d) "ESCAPE" #(.dispose d) JComponent/WHEN_IN_FOCUSED_WINDOW)
     (.setDefaultButton (.getRootPane d) close)
     (.setSize d (UIScale/scale (int 980)) (UIScale/scale (int 640)))
     (.setLocationRelativeTo d frame)
     (.addWindowListener d (proxy [WindowAdapter] []
                             (windowOpened [_]
                               (.requestFocusInWindow ^JComponent (if page (:focus (nth (nth pages start) 2)) side)))))
     (.setVisible d true))))

;; ---------------------------------------------------------------- building

(defn- setup-look!
  "The saved theme, or FlatLaf Dark, at the saved text size. UIManager/put
  values outlive theme switches."
  []
  (UIManager/put "Table.showHorizontalLines" true)
  (UIManager/put "Tree.showDefaultIcons" true)
  (UIManager/put "ScrollPane.smoothScrolling" (boolean (:animations @state)))
  (try
    (UIManager/setLookAndFeel ^String (current-theme))
    (catch Exception e
      (log/event :error :where "theme" :message (str e))
      (if (os/mac?) (FlatMacDarkLaf/setup) (FlatDarkLaf/setup))))
  (put-font-size! (:font-size @state))
  (anim/fade-popups!))

(defn- build! []
  (let [frame (JFrame. app-name)
        search (JTextField.)
        model (proxy [AbstractTableModel] []
                (getRowCount [] (count (:rows @state)))
                (getColumnCount [] (count columns))
                (getColumnName [c] (nth columns c))
                (getValueAt [r c] (cell-text (nth (:rows @state) r) c))
                (isCellEditable [_ _] false))
        table (doto (JTable. ^AbstractTableModel model)
                (.setFillsViewportHeight true)
                (.setSelectionMode ListSelectionModel/MULTIPLE_INTERVAL_SELECTION)
                (.setDragEnabled true)
                (.setDropMode DropMode/ON)
                (.setTransferHandler (table-transfer)))
        tree (doto (proxy [JTree] [(DefaultTreeModel. (DefaultMutableTreeNode. (->Item "Loading ..." nil nil)))]
                     ;; the row under the pointer, lit across the whole width
                     (paintComponent [^Graphics g]
                       (proxy-super paintComponent g)
                       (when-let [r (:tree @hover)]
                         (let [^JTree t this]
                           (when-let [b (.getRowBounds t (int r))]
                             (when-not (.isRowSelected t (int r))
                               (let [fg (.getForeground t)
                                     g2 (.create g)]
                                 (.setColor g2 (Color. (.getRed fg) (.getGreen fg) (.getBlue fg) 20))
                                 (.fillRect g2 0 (.y b) (.getWidth t) (.height b))
                                 (.dispose g2))))))))
               (.setRootVisible true)
               (.setCellRenderer (tree-renderer))
               (.setDragEnabled true)
               (.setDropMode DropMode/ON)
               (.setTransferHandler (tree-transfer)))
        status (doto (JLabel. " ") (.setBorder (BorderFactory/createEmptyBorder 4 10 4 10)))
        busy (doto (JProgressBar.)
               (.setIndeterminate true)
               (.setVisible false)
               (.setPreferredSize (Dimension. (UIScale/scale (int 120)) (UIScale/scale (int 6))))
               (.setMaximumSize (Dimension. (UIScale/scale (int 120)) (UIScale/scale (int 6)))))
        hint (doto (JLabel. " ")
               (.setBorder (BorderFactory/createEmptyBorder 4 10 4 10))
               (.putClientProperty "FlatLaf.styleClass" "small")
               (.putClientProperty "FlatLaf.style" "foreground: $Label.disabledForeground"))
        actions (into {} (for [[id label _ f] (app-actions)] [id (action label f)]))
        split (doto (JSplitPane. JSplitPane/HORIZONTAL_SPLIT (JScrollPane. tree) (JScrollPane. table))
                (.setFocusable false)
                (.setContinuousLayout true))
        buttons (into {} (for [k [:add :reload]]
                           [k (doto (JButton. ^Action (actions k))
                                (.setFocusable false)
                                (.setCursor hand))]))]
    (reset! widgets {:frame frame :search search :table table :tree tree :status status :hint hint :busy busy
                     :split split :actions actions :buttons buttons})

    ;; search: filters on every keystroke
    (.putClientProperty search "JTextField.placeholderText" "Search title, URL, tag, note")
    (.putClientProperty search "JTextField.showClearButton" true)
    (.putClientProperty search "JTextField.leadingIcon" (FlatSearchIcon.))
    (.addDocumentListener (.getDocument search)
                          (let [changed (fn [] (swap! state assoc :query (.getText search)) (render-table!) (status! (summary)))]
                            (reify DocumentListener
                              (insertUpdate [_ _] (changed))
                              (removeUpdate [_ _] (changed))
                              (changedUpdate [_ _] (changed)))))

    ;; list: Tab leaves the table instead of moving between cells
    (.setFocusTraversalKeys table KeyboardFocusManager/FORWARD_TRAVERSAL_KEYS nil)
    (.setFocusTraversalKeys table KeyboardFocusManager/BACKWARD_TRAVERSAL_KEYS nil)
    (let [m (mouse-handler :list)]
      (.addMouseListener table m)
      (.addMouseMotionListener table m))

    ;; a click on a column header sorts by it; columns keep their places so the
    ;; click maps to the right sort key
    (let [header (.getTableHeader table)]
      (.setReorderingAllowed header false)
      (.setCursor header hand)
      (.addMouseListener header (proxy [MouseAdapter] []
                                  (mouseClicked [^MouseEvent e]
                                    (let [i (.columnAtPoint header (.getPoint e))]
                                      (when (>= i 0) ((guarded #(sort-by-column! i)))))))))

    ;; Vim folds: z then a (toggle), c (fold), o (unfold), M (fold all), R (unfold all).
    ;; A listener runs before key bindings, so after z it takes the next key
    ;; and a never reaches add.
    (let [pending? (atom false)
          after-z {\a #(fold! :toggle) \c #(fold! true) \o #(fold! false) \M fold-all! \R unfold-all!}]
      (.addKeyListener table (proxy [KeyAdapter] []
                               (keyTyped [^KeyEvent e]
                                 (let [ch (.getKeyChar e)]
                                   (cond
                                     @pending? (do (reset! pending? false)
                                                   (when-let [f (after-z ch)]
                                                     ((guarded f))
                                                     (.consume e)))
                                     (= ch \z) (do (reset! pending? true) (.consume e))))))))

    (.setDefaultRenderer table Object (cell-renderer))
    (doseq [[i w] (map-indexed vector [420 400 260])]
      (.setPreferredWidth (.getColumn (.getColumnModel table) (int i)) (UIScale/scale (int w))))

    ;; tree: no type-to-select, so its single keys are free; a tag narrows the list
    (doseq [l (.getKeyListeners tree)] (.removeKeyListener tree l))
    (let [m (mouse-handler :tree)]
      (.addMouseListener tree m)
      (.addMouseMotionListener tree m))
    (.setSelectionMode (.getSelectionModel tree) TreeSelectionModel/SINGLE_TREE_SELECTION)
    (.addTreeSelectionListener tree (reify javax.swing.event.TreeSelectionListener
                                      (valueChanged [_ e]
                                        (when-let [node ^DefaultMutableTreeNode (some-> (.getNewLeadSelectionPath e) .getLastPathComponent)]
                                          (let [{:keys [tag]} (.getUserObject ^DefaultMutableTreeNode node)]
                                            (swap! state assoc :scope tag)
                                            (render-table!)
                                            (status! (if tag (str tag ": " (count (:visible @state)) " links") (summary))))))))

    ;; the hint at the bottom right follows the focused pane
    (.addPropertyChangeListener (KeyboardFocusManager/getCurrentKeyboardFocusManager) "permanentFocusOwner"
                                (reify java.beans.PropertyChangeListener
                                  (propertyChange [_ e]
                                    (when-let [pane ({search :search tree :tree table :list} (.getNewValue e))]
                                      (show-hint! pane)))))

    (.setJMenuBar frame (let [bar (JMenuBar.)]
                          (doseq [[title mnemonic ks] (menus)]
                            (let [m (doto (JMenu. ^String title) (.setMnemonic (int mnemonic)))]
                              (doseq [k ks] (if (= k :sep) (.addSeparator m) (.add m (JMenuItem. ^Action (actions k)))))
                              (.add bar m)))
                          bar))

    ;; Tab cycles search -> tags -> links; the buttons have shortcuts instead.
    ;; Separators part the search bar, the panes and the status bar.
    (let [pad (UIScale/scale (int 8))
          button-row (doto (JPanel. (FlowLayout. FlowLayout/RIGHT pad 0))
                       (.add ^JButton (buttons :add))
                       (.add ^JButton (buttons :reload)))
          top (doto (JPanel. (BorderLayout. pad 0))
                (.setBorder (BorderFactory/createEmptyBorder pad pad pad pad))
                (.add search BorderLayout/CENTER)
                (.add button-row BorderLayout/EAST))
          east (doto (JPanel.)
                 (as-> p (.setLayout p (BoxLayout. p BoxLayout/X_AXIS)))
                 (.add busy)
                 (.add hint))
          bottom (doto (JPanel. (BorderLayout.))
                   (.add (JSeparator.) BorderLayout/NORTH)
                   (.add status BorderLayout/CENTER)
                   (.add east BorderLayout/EAST))]
      ;; the add button stands out: it is the window's main action
      (.putClientProperty ^JButton (buttons :add) "FlatLaf.style"
                          "background: $Button.default.background; foreground: $Button.default.foreground; borderColor: $Button.default.borderColor")
      (doto (.getContentPane frame)
        (.setLayout (BorderLayout.))
        (.add (doto (JPanel. (BorderLayout.))
                (.add top BorderLayout/CENTER)
                (.add (JSeparator.) BorderLayout/SOUTH))
              BorderLayout/NORTH)
        (.add split BorderLayout/CENTER)
        (.add bottom BorderLayout/SOUTH)))

    (.addWindowListener frame (proxy [WindowAdapter] [] (windowClosing [_] (save-on-quit!))))
    (when (os/mac?)
      (let [d (Desktop/getDesktop)]
        (.setPreferencesHandler d (reify PreferencesHandler
                                    (handlePreferences [_ _] (on-edt (guarded show-settings!)))))
        (.setQuitHandler d (reify QuitHandler
                             (handleQuitRequestWith [_ _ response]
                               (save-on-quit!)
                               (.performQuit response))))))
    @text-menus
    (apply-keys!)
    (.setDefaultCloseOperation frame JFrame/EXIT_ON_CLOSE)
    (restore-window!)
    (doto frame
      (.setVisible true)
      (.toFront))
    (.requestFocusInWindow search)))

(defn start!
  "Open the window on the event thread and load data.rec. The window comes
  back as it was at the last quit, its file too unless one was named
  (explicit?). No data-path and nothing to come back to: a fresh start, which
  asks for a file first."
  [{:keys [data-path explicit?]}]
  (let [view (load-view)
        config (db/config)
        first-run? (not (:tag-defaults view))
        session (when (map? (:session view)) (:session view))
        restored (when-not explicit?
                   (some-> (:data session) store/expand-home (#(when (.exists (io/file %)) %))))
        path (or restored data-path)]
    ;; Read starts folded; after that the folds are yours
    (swap! state assoc :data-path path
           :folded (cond-> (load-folds) first-run? (conj db/done))
           :sort (or (:sort view) {:key nil :desc? false})
           :theme (or (:theme config) (:theme view))
           :font-size (clamp-font-size (if (number? (:font-size config)) (:font-size config) default-font-size))
           :keys (if (map? (:keys config)) (:keys config) {})
           :animations (not (false? (:animations config)))
           :session session
           :scope (when (or restored (not explicit?)) (:scope session)))
    (reset! anim/on? (:animations @state))
    (System/setProperty "flatlaf.animation" (str (:animations @state)))
    (when first-run?
      (save-folds!)
      (save-view!))
    (on-edt (fn []
              (setup-look!)
              (build!)
              (style-widgets!)
              (show-sort-in-header!)
              (log/event :start :data path)
              (when-let [q (not-empty (str (:query session)))]
                (when (or restored (not explicit?)) (.setText ^JTextField (:search @widgets) q)))
              (if path
                (do (show-path!) (reload!))
                ;; after the window is up, so the question has a parent to sit on
                (on-edt (guarded first-start!)))))))
