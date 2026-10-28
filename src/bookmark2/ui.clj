(ns bookmark2.ui
  "The window over data.rec: Swing widgets in FlatLaf's dark look.

    +---------------------------------------------------------------+
    | [ search title, url, tag, note ...       ]   [Add]  [Reload]   |
    +------------------+--------------------------------------------+
    | All  (301)       | ▾ Hardware/FPGA  (1)                       |
    |  ▾ AI  (19)     |       Icestudio      https://icestudio.io/ |
    |     Agents (10)  | ▾ To Read  (182)                           |
    |  To Read (182)   |       Choose Boring Technology  ...        |
    +------------------+--------------------------------------------+
    | 301 links, 182 to read, 6 read            keys for this pane   |
    +---------------------------------------------------------------+

  Left: the tags as a tree (a / in a tag nests it). Right: the links folded by
  tag, newest first; a link with two tags shows under both. Choosing a branch
  shows every link in it and below it.

  All state lives in the `state` atom and the widgets in `widgets`. Widget
  calls run on the Swing event thread; loading, page fetches and opening links
  run on futures and hand their result back through `on-edt`.

  Everything works from the keyboard: Tab cycles search -> tags -> links, each
  pane has single keys (`pane-keys`), every action has a menu shortcut, and
  Shift+F10 / the Menu key / Cmd+J open the context menu.

  Debugging: `bookmark2 --repl 5555` opens a socket REPL on 127.0.0.1, where
  (bookmark2.ui/snapshot) shows what the window shows."
  (:require [bookmark2.db :as db]
            [bookmark2.log :as log]
            [bookmark2.os :as os]
            [bookmark2.store :as store]
            [bookmark2.web :as web]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (com.formdev.flatlaf FlatDarculaLaf FlatDarkLaf FlatLaf)
           (com.formdev.flatlaf.intellijthemes FlatAllIJThemes)
           (com.formdev.flatlaf.themes FlatMacDarkLaf)
           (java.awt BorderLayout Color Dimension Font GridBagConstraints GridBagLayout Insets
                     KeyboardFocusManager Rectangle Toolkit)
           (java.awt.datatransfer DataFlavor StringSelection)
           (java.awt.event KeyAdapter KeyEvent MouseAdapter WindowAdapter)
           (javax.swing AbstractAction Action BorderFactory JButton JComponent
                        JEditorPane JFrame JLabel JList JMenu JMenuBar JMenuItem JOptionPane JPanel
                        JPopupMenu JScrollPane JSplitPane JTable JTextField JToolBar JTree KeyStroke
                        ListSelectionModel MenuElement MenuSelectionManager SwingUtilities UIManager)
           (javax.swing.event DocumentListener)
           (javax.swing.table AbstractTableModel DefaultTableCellRenderer)
           (javax.swing.tree DefaultMutableTreeNode DefaultTreeModel TreePath TreeSelectionModel)))

(def app-name "Bookmark2")
(def window-size [1180 720])
(def tree-width 280)
(def untagged "(untagged)")
(def undo-hint (str (if (os/mac?) "Cmd" "Ctrl") "+Z or u undoes it."))

;; :db the loaded data.rec; :scope the tag chosen in the tree (nil = All);
;; :visible the links that pass the tree and the search; :rows what the list
;; shows, those links under tag headers; :folded the tags whose links are
;; hidden, saved in folds.edn; :sort the list order and :theme the look and
;; feel class, saved in view.edn
(defonce state (atom {:data-path nil :db nil :query "" :scope nil :visible [] :rows []
                      :folded #{} :sort {:key nil :desc? false} :theme nil :undo [] :status ""}))
(defonce widgets (atom {}))

(defn on-edt [f] (SwingUtilities/invokeLater f))

(defn- in-background
  "Run f off the event thread, then (done result) on it. Errors go to the status bar."
  [f done]
  (future
    (try
      (let [result (f)]
        (on-edt #(done result)))
      (catch Throwable e
        (log/event :error :where "background" :message (str e))
        (on-edt #(.setText ^JLabel (:status @widgets) (str "Error: " (ex-message e))))))))

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

(def columns
  "[name, cell for a link, cell for a header]. Links are indented under their header."
  [["Title" #(str "      " (or (:title %) (:url %)))
    #(str (if (:folded? %) "▸ " "▾ ") (:key %) "  (" (:count %) ")")]
   ["URL" :url (constantly "")]
   ["Tags" #(str/join ", " (:tags %)) (constantly "")]])

(defn snapshot
  "What the window shows right now, for the socket REPL."
  []
  (let [s @state
        ^JTable table (:table @widgets)]
    {:status (some-> ^JLabel (:status @widgets) .getText)
     :data (:data-path s)
     :links (count (:links (:db s)))
     :query (:query s)
     :scope (:scope s)
     :visible (count (:visible s))
     :rows (count (:rows s))
     :folded (count (:folded s))
     :sort (:sort s)
     :first-rows (mapv #(if (:header? %)
                          (select-keys % [:header? :key :count :folded?])
                          (select-keys % [:title :url :tags]))
                       (take 5 (:rows s)))
     :selected-row (some-> table .getSelectedRow)
     :undo (mapv :label (:undo s))}))

(defn- status! [msg]
  (swap! state assoc :status msg)
  (.setText ^JLabel (:status @widgets) msg))

(defn- summary []
  (let [{:keys [db visible]} @state
        ls (:links db)
        n (fn [t] (count (filter #(some #{t} (:tags %)) ls)))]
    (str (count ls) " links, " (n db/to-read) " to read, " (n db/done) " read"
         (when (not= (count ls) (count visible)) (str ", " (count visible) " shown")))))

(defn- selected-row
  "The selected row of the list: a link or a tag header."
  []
  (let [row (.getSelectedRow ^JTable (:table @widgets))]
    (when (>= row 0)
      (get (:rows @state) row))))

(defn- selected-entry
  "The selected link; nil on a header, so link actions do nothing there."
  []
  (let [r (selected-row)]
    (when-not (:header? r) r)))

(defn- row-id [r]
  (if (:header? r) [:header (:key r)] [(:group r) (:url r)]))

;; ---------------------------------------------------------------- rendering

(defrecord Item [label tag]
  Object
  (toString [_] label))

(defn- tree-root
  "All, then every tag as a tree (counts include the branch below), then
  untagged when some link has no tag."
  [links]
  (let [count-under (fn [p] (count (filter (fn [l] (some #(db/under? % p) (:tags l))) links)))
        tags (distinct (mapcat :tags links))
        nodes (sort-by (fn [t] [(get pinned t 2) (str/lower-case t)]) (distinct (mapcat db/ancestors-of tags)))
        root (DefaultMutableTreeNode. (->Item (str "All  (" (count links) ")") nil))
        by-tag (atom {})]
    ;; parents sort before their children, so each parent exists when its child arrives
    (doseq [t nodes]
      (let [parent-tag (str/join "/" (butlast (str/split t #"/")))
            parent (if (str/blank? parent-tag) root (get @by-tag parent-tag root))
            node (DefaultMutableTreeNode. (->Item (str (last (str/split t #"/")) "  (" (count-under t) ")") t))]
        (.add ^DefaultMutableTreeNode parent node)
        (swap! by-tag assoc t node)))
    (let [n (count (filter #(empty? (:tags %)) links))]
      (when (pos? n)
        (.add root (DefaultMutableTreeNode. (->Item (str untagged "  (" n ")") untagged)))))
    root))

(defn- render-table!
  "Rebuild the rows and keep the selection: on target (a row-id) when given,
  else on the selected row, else on the header of the selected link's tag
  when that tag was just folded, else the first row."
  [& [target]]
  (let [was (selected-row)
        _ (swap! state #(let [s (assoc % :visible (visible %))] (assoc s :rows (rows s))))
        ^JTable table (:table @widgets)
        index-of (fn [id] (first (keep-indexed #(when (= id (row-id %2)) %1) (:rows @state))))]
    (.fireTableDataChanged ^AbstractTableModel (.getModel table))
    (when (pos? (.getRowCount table))
      (let [i (or (some-> target index-of)
                  (some-> was row-id index-of)
                  (some->> was :group (vector :header) index-of)
                  0)]
        (.setRowSelectionInterval table i i)
        (.scrollRectToVisible table (.getCellRect table i 0 true))))))

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
  (swap! state #(-> % reload-db (update :undo conj {:label label :changes changes})))
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
  (let [{:keys [label changes]} (peek (:undo @state))]
    (if-not changes
      (status! "Nothing to undo.")
      (do (store/commit! (store/reverse-changes changes))
          (swap! state #(-> % reload-db (update :undo pop)))
          (log/event :undo :label label)
          (db-changed!)
          (status! (str "Undid: " label))))))

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

(defn- open-selected! []
  (when-let [l (selected-entry)]
    (log/event :open :url (:url l))
    (in-background #(web/open-url! (:url l)) (fn [_] (status! (str "Opened " (:url l)))))))

(defn- copy-selected! []
  (when-let [l (selected-entry)]
    (.setContents (.getSystemClipboard (Toolkit/getDefaultToolkit)) (StringSelection. (:url l)) nil)
    (status! (str "Copied " (:url l)))))

(defn- delete-selected! []
  (when-let [l (selected-entry)]
    (apply-changes! (str "Deleted " (link-name l)) (db/plan-remove (:db @state) l))))

(defn- open-file! []
  (let [path (:data-path @state)]
    (in-background #(os/open-in-editor! path)
                   (fn [_] (status! (str "Opened " (store/short-path path) " in the text editor."))))))

(defn- reveal-file! []
  (let [path (:data-path @state)]
    (in-background #(os/reveal! path) (fn [_] (status! (str "Revealed " (store/short-path path)))))))

;; ---------------------------------------------------------------- moving around

(defn- select-row!
  "Select row i of the list, clamped, and scroll it into view."
  [i]
  (let [^JTable table (:table @widgets)
        n (.getRowCount table)]
    (when (pos? n)
      (let [i (max 0 (min (dec n) i))]
        (.setRowSelectionInterval table i i)
        (.scrollRectToVisible table (.getCellRect table i 0 true))))))

(defn- move-row! [delta]
  (select-row! (+ delta (max 0 (.getSelectedRow ^JTable (:table @widgets))))))

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
  (when (neg? (.getSelectedRow ^JTable (:table @widgets)))
    (select-row! 0))
  (.requestFocusInWindow ^JTable (:table @widgets)))

(defn- search-escape!
  "Esc in search: clear it; when already empty, go to the list."
  []
  (let [^JTextField search (:search @widgets)]
    (if (str/blank? (.getText search))
      (focus-list!)
      (.setText search ""))))

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

;; ---------------------------------------------------------------- sorting

(defn- view-file [] (io/file (log/state-dir) "view.edn"))

(defn- load-view []
  (try
    (let [v (edn/read-string (slurp (view-file)))]
      (if (some #{(get-in v [:sort :key])} sort-keys) v (dissoc v :sort)))
    (catch Exception _ {})))

(defn- save-view!
  "view.edn: the sort, the theme, and that the first-run defaults were applied."
  []
  (try
    (io/make-parents (view-file))
    (spit (view-file) (pr-str {:sort (:sort @state) :theme (:theme @state) :tag-defaults true}))
    (catch Exception e (log/event :error :where "save-view" :message (str e)))))

(defn- sort-label [{:keys [key desc?]}]
  (if key
    (str "Sorted by " (str/capitalize (name key)) (if desc? " ▼" " ▲"))
    "Newest first"))

(defn- show-sort-in-header!
  "The sorted column's name gets ▲ or ▼."
  []
  (let [^JTable table (:table @widgets)
        {:keys [key desc?]} (:sort @state)]
    (doseq [[i [col-name]] (map-indexed vector columns)]
      (.setHeaderValue (.getColumn (.getColumnModel table) (int i))
                       (str col-name (when (= key (get sort-keys (inc i))) (if desc? "  ▼" "  ▲")))))
    (.repaint (.getTableHeader table))))

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
  "Enter: open the link; on a folded header unfold it, on an open one go to
  its first link."
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
  "r: To Read <-> Read on the selected link (no reading tag: To Read is added).
  The cursor stays on the same row, so the next link to triage is under it."
  []
  (when-let [l (selected-entry)]
    (let [row (.getSelectedRow ^JTable (:table @widgets))
          tags (db/toggle-read (:tags l))]
      (apply-changes! (str (if (some #{db/done} tags) "Read: " "To read: ") (link-name l))
                      (db/plan-tags (:db @state) l tags))
      (select-row! row))))

;; ---------------------------------------------------------------- forms

(defn- gbc [x y weight & {:keys [width] :or {width 1}}]
  (let [c (GridBagConstraints.)]
    (set! (.gridx c) x)
    (set! (.gridy c) y)
    (set! (.gridwidth c) width)
    (set! (.weightx c) (double weight))
    (set! (.fill c) GridBagConstraints/HORIZONTAL)
    (set! (.anchor c) GridBagConstraints/WEST)
    (set! (.insets c) (Insets. 4 4 4 4))
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

(defn- show-form
  "A modal OK/Cancel dialog around panel, focus in first. True on OK."
  [heading ^JPanel panel ^JComponent first]
  (let [pane (JOptionPane. panel JOptionPane/PLAIN_MESSAGE JOptionPane/OK_CANCEL_OPTION)
        dialog (.createDialog pane (:frame @widgets) heading)]
    (.addWindowListener dialog (proxy [WindowAdapter] []
                                 (windowOpened [_] (.requestFocusInWindow first))))
    (.setVisible dialog true)
    (= JOptionPane/OK_OPTION (.getValue pane))))

(defn- link-form
  "URL, Title, Tags (comma separated, / nests) and Note. A blank title is
  fetched in the background when fetch? is set. {:url :title :tags :note} or nil."
  [{:keys [heading url title tags note fetch?]}]
  (let [url-field (JTextField. (str url) 48)
        title-field (JTextField. (str title) 48)
        tags-field (JTextField. (str/join ", " tags) 48)
        note-field (JTextField. (str note) 48)
        fetch-button (doto (JButton. "Fetch title") (.setMnemonic KeyEvent/VK_E))
        note-label (JLabel. " ")
        panel (JPanel. (GridBagLayout.))
        fetch! (fn []
                 (let [u (str/trim (.getText url-field))]
                   (when (re-matches #"https?://\S+" u)
                     (.setText note-label "Fetching title ...")
                     (in-background #(web/fetch-title u)
                                    (fn [t]
                                      (when (str/blank? (.getText title-field)) (.setText title-field t))
                                      (.setText note-label " "))))))]
    (.addActionListener fetch-button (reify java.awt.event.ActionListener (actionPerformed [_ _] (fetch!))))
    (.putClientProperty url-field "JTextField.placeholderText" "https://")
    (.putClientProperty title-field "JTextField.placeholderText" "optional; Fetch title reads the page's")
    (.putClientProperty tags-field "JTextField.placeholderText" "comma separated; a / nests: Hardware/FPGA, To Read")
    (.putClientProperty note-field "JTextField.placeholderText" "optional")
    ;; mnemonics: Alt+letter (Ctrl+Option on macOS) jumps to a field or presses a button
    (doseq [[row label mnemonic comp extra] [[0 "URL" KeyEvent/VK_U url-field nil]
                                             [1 "Title" KeyEvent/VK_T title-field fetch-button]
                                             [2 "Tags" KeyEvent/VK_G tags-field nil]
                                             [3 "Note" KeyEvent/VK_N note-field nil]]]
      (.add panel (doto (JLabel. ^String label)
                    (.setDisplayedMnemonic (int mnemonic))
                    (.setLabelFor comp))
            (gbc 0 row 0))
      (.add panel ^JComponent comp (gbc 1 row 1))
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
  "Add a link: the clipboard's url, the tree's tag. No tags means To Read."
  []
  (run-form {:heading "Add link" :url (clipboard-url) :tags (keep identity [(scope-tag)]) :fetch? true}
            #(db/plan-add (:db @state) %)
            #(str "Added " (link-name %))))

(defn edit! []
  (when-let [l (selected-entry)]
    (run-form {:heading "Edit link" :url (:url l) :title (:title l) :tags (:tags l) :note (:note l)}
              #(db/plan-edit (:db @state) l %)
              #(str "Saved " (link-name %)))))

(defn- tag-in-focus
  "The tag Rename acts on: the tree's when the tree has focus, else the list's
  header or link group."
  []
  (if (.isFocusOwner ^JTree (:tree @widgets))
    (scope-tag)
    (let [g (current-group)] (when-not (= g untagged) g))))

(defn rename-tag!
  "F2: rename a tag on every link. A / moves it: FPGA -> Hardware/FPGA. Tags
  below it move along."
  []
  (if-let [from (tag-in-focus)]
    (let [field (JTextField. ^String from 40)
          panel (doto (JPanel. (GridBagLayout.))
                  (.add (JLabel. (str "Rename \"" from "\" and every tag below it to:")) (gbc 0 0 1))
                  (.add field (gbc 0 1 1)))]
      (when (show-form "Rename tag" panel field)
        (let [to (db/clean-tag (.getText field))
              scope (:scope @state)]
          (when (and to (not= to from))
            ;; the tree follows the rename: a scope at or below from moves with it
            (when (and scope (db/under? scope from))
              (swap! state assoc :scope (str to (subs scope (count from)))))
            (apply-changes! (str "Renamed " from " to " to) (db/plan-rename-tag (:db @state) from to))))))
    (status! "Choose a tag in the tree, or a tag header in the list, to rename it.")))

;; ---------------------------------------------------------------- themes

(def row-height 26)
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
  "What a look and feel switch does not redo by itself: the hint's dim
  colour and the row heights."
  []
  (let [{:keys [^JLabel hint ^JTable table ^JTree tree]} @widgets]
    (when hint (.setForeground hint (UIManager/getColor "Label.disabledForeground")))
    (when table (.setRowHeight table (int row-height)))
    (when tree (.setRowHeight tree (int tree-row-height)))))

(defn- set-theme!
  "Switch every window to the look and feel class cls; save it unless preview?."
  [cls & {:keys [preview?]}]
  (UIManager/setLookAndFeel ^String cls)
  (FlatLaf/updateUI)
  (style-widgets!)
  (when-not preview?
    (swap! state assoc :theme cls)
    (save-view!))
  (status! (str "Theme: " (theme-name cls))))

(defn- cycle-theme!
  "Cmd+] / Cmd+[: the next or previous theme, saved."
  [delta]
  (let [ts (themes)
        i (or (first (keep-indexed #(when (= (second %2) (current-theme)) %1) ts)) 0)]
    (set-theme! (second (nth ts (mod (+ i delta) (count ts)))))))

(defn choose-theme!
  "Theme...: the dark themes in a list; moving through it previews each one in
  the whole window. Enter or OK keeps it, Esc or Cancel goes back."
  []
  (let [ts (themes)
        was (current-theme)
        lst (doto (JList. ^"[Ljava.lang.Object;" (into-array Object (map first ts)))
              (.setSelectionMode ListSelectionModel/SINGLE_SELECTION)
              (.setVisibleRowCount 18))
        start (or (first (keep-indexed #(when (= (second %2) was) %1) ts)) 0)]
    (.setSelectedIndex lst start)
    (.ensureIndexIsVisible lst start)
    (.addListSelectionListener lst (reify javax.swing.event.ListSelectionListener
                                     (valueChanged [_ e]
                                       (let [i (.getSelectedIndex lst)]
                                         (when (and (not (.getValueIsAdjusting e)) (>= i 0))
                                           ;; switch after this event: a theme switch rebuilds the list's
                                           ;; own UI, which is still handling the selection now; then give
                                           ;; focus back, since the rebuild moves it
                                           (on-edt (fn []
                                                     ((guarded #(set-theme! (second (nth ts i)) :preview? true)))
                                                     (.requestFocusInWindow lst))))))))
    (let [panel (doto (JPanel. (BorderLayout.))
                  (.add (JLabel. "Arrows preview, Enter keeps, Esc goes back.") BorderLayout/NORTH)
                  (.add (doto (JScrollPane. lst) (.setPreferredSize (Dimension. 340 440))) BorderLayout/CENTER))
          keep? (show-form "Theme" panel lst)]
      (set-theme! (if keep? (second (nth ts (max 0 (.getSelectedIndex lst)))) was)))))

(declare show-shortcuts! show-context-menu!)

;; ---------------------------------------------------------------- keys

(defn- pane-keys
  "Single keys inside one pane, as KeyStroke strings: [keys description f].
  f nil means Swing (or the z-prefix listener) does it. This table drives the
  bindings and Help > Keyboard Shortcuts; Cmd shortcuts are the menu accelerators."
  []
  {:search [[["DOWN"] "Go to the links" focus-list!]
            [["ENTER"] "Open the selected link" open-selected!]
            [["ESCAPE"] "Clear the search; when empty, go to the links" search-escape!]
            [["TAB" "shift TAB"] "Next, previous pane" nil]]
   :tree [[["UP" "DOWN" "typed k" "typed j"] "Previous, next tag" #(move-tree! 1)]
          [["LEFT" "RIGHT"] "Collapse, expand a branch" nil]
          [["ENTER" "typed l"] "Go to its links" focus-list!]
          [["typed a"] "Add a link with this tag" add!]
          [["typed u"] "Undo" undo!]
          [["F2"] "Rename this tag (a / moves it in the tree)" rename-tag!]
          [["typed /" "ESCAPE"] "Search" focus-search!]
          [["shift F10" "CONTEXT_MENU"] "Context menu" #(show-context-menu! :tree)]
          [["typed ?"] "Keyboard shortcuts" show-shortcuts!]]
   :list [[["UP" "DOWN" "typed k" "typed j"] "Previous, next row (links and tag headers)" #(move-row! 1)]
          [["PAGE_UP" "PAGE_DOWN"] "A page up, down" nil]
          [["HOME" "typed g"] "First row" #(select-row! 0)]
          [["END" "typed G"] "Last row" #(select-row! Integer/MAX_VALUE)]
          [["ENTER"] "Open the link; on a folded header, unfold it" list-enter!]
          [["typed r"] "Mark read, or back to To Read" toggle-read!]
          [["SPACE" "za"] "Fold or unfold this tag" #(fold! :toggle)]
          [["zc" "zo"] "Fold, unfold this tag" nil]
          [["zM" "zR"] "Fold all, unfold all tags" nil]
          [["typed s"] "Sort: newest first, Title, URL, Tags in turn" cycle-sort!]
          [["typed S"] "Flip the sort between ascending and descending" flip-sort!]
          [["typed e"] "Edit the link: url, title, tags, note" edit!]
          [["typed y"] "Copy the URL" copy-selected!]
          [["DELETE" "BACK_SPACE"] "Delete the link (from every tag)" delete-selected!]
          [["typed u"] "Undo" undo!]
          [["typed a"] "Add a link" add!]
          [["F2"] "Rename this tag (on a header or a link in it)" rename-tag!]
          [["LEFT" "typed h"] "To the link's tag header; from a header, to the tags" list-left!]
          [["typed /" "ESCAPE"] "Search" focus-search!]
          [["shift F10" "CONTEXT_MENU"] "Context menu" #(show-context-menu! :list)]
          [["typed ?"] "Keyboard shortcuts" show-shortcuts!]]})

;; UP and k move the other way from DOWN and j: one row, opposite sign.
(def ^:private reverse-keys #{"UP" "typed k"})

(def ^:private hints
  {:search "Down links   Enter open   Esc clear   Tab next pane   Cmd+/ keys"
   :tree "j k move   Enter links   a add here   F2 rename   / search   Shift+F10 menu   ? keys"
   :list "Enter open   r read   Space fold   s sort   e edit   Del delete   F2 rename   / search   ? keys"})

(defn- pretty-key
  "\"typed j\" -> j, \"shift F10\" -> Shift+F10, \"BACK_SPACE\" -> Backspace"
  [s]
  (let [names {"ESCAPE" "Esc" "BACK_SPACE" "Backspace" "PAGE_UP" "Page Up" "PAGE_DOWN" "Page Down"
               "CONTEXT_MENU" "Menu key" "TAB" "Tab" "shift TAB" "Shift+Tab" "ENTER" "Enter"
               "DELETE" "Delete" "HOME" "Home" "END" "End" "UP" "Up" "DOWN" "Down" "SPACE" "Space"
               "LEFT" "Left" "RIGHT" "Right" "shift F10" "Shift+F10"}]
    (or (names s) (str/replace s #"^typed " ""))))

(defn- accelerator-text [^KeyStroke ks]
  (let [mods (KeyEvent/getModifiersExText (.getModifiers ks))]
    (str mods (when (seq mods) "+") (KeyEvent/getKeyText (.getKeyCode ks)))))

(defn- menu-key [code & [extra]]
  (KeyStroke/getKeyStroke (int code)
                          (int (bit-or (.getMenuShortcutKeyMaskEx (Toolkit/getDefaultToolkit)) (or extra 0)))))

(defn- action [label key f]
  (let [body (guarded f)]
    (doto (proxy [AbstractAction] [label]
            (actionPerformed [_] (body)))
      (.putValue Action/ACCELERATOR_KEY key))))

(defn- bind-key!
  "Bind ks on c. A two-key sequence like \"za\" is no KeyStroke: the z-prefix
  listener handles those, so they are skipped here."
  [^JComponent c ^String ks f]
  (when-let [stroke (KeyStroke/getKeyStroke ks)]
    (let [id (str "bookmark2 " ks)]
      (.put (.getInputMap c JComponent/WHEN_FOCUSED) stroke id)
      (.put (.getActionMap c) id (proxy [AbstractAction] [] (actionPerformed [_] ((guarded f))))))))

(defn- bind-pane! [^JComponent c pane]
  (doseq [[ks _ f] (get (pane-keys) pane)
          :when f
          k ks]
    (bind-key! c k (if (reverse-keys k) (fn [] ((get {:tree move-tree! :list move-row!} pane) -1)) f))))

(defn- shortcuts-html []
  (let [row (fn [k d] (str "<tr><td><b>" k "</b>&nbsp;&nbsp;&nbsp;</td><td>" d "</td></tr>"))
        section (fn [title rows] (str "<tr><td colspan=2><br><b><u>" title "</u></b></td></tr>" (str/join rows)))
        menu-bar (if (os/mac?) "Ctrl+F2 (macOS: Keyboard Navigation on)" "F10, or Alt+the underlined letter")]
    (str "<html><body style='padding:6px'><table>"
         (section "Anywhere"
                  (for [^Action a (:menu-actions @widgets)]
                    (row (accelerator-text (.getValue a Action/ACCELERATOR_KEY)) (.getValue a Action/NAME))))
         (str/join (for [[pane title] [[:search "Search box"] [:tree "Tags (left)"] [:list "Links (right)"]]]
                     (section title (for [[ks d] (get (pane-keys) pane)]
                                      (row (str/join " or " (map pretty-key ks)) d)))))
         (section "Dialogs"
                  [(row "Tab / Shift+Tab" "Next, previous field")
                   (row "Enter" "OK")
                   (row "Esc" "Cancel")
                   (row "Alt+letter" "The underlined field or button (Ctrl+Option on macOS)")])
         (section "Menus"
                  [(row "Menu bar" menu-bar)
                   (row "Arrows, Enter, Esc" "Move, choose, close")])
         "</table><br>Data: " (:data-path @state)
         "<br>Log: " (log/log-file) "</body></html>")))

(defn show-shortcuts! []
  (let [view (doto (JEditorPane. "text/html" (shortcuts-html))
               (.setEditable false)
               (.setCaretPosition 0))
        scroll (doto (JScrollPane. view) (.setPreferredSize (Dimension. 640 560)))
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
                        (selected-entry) [:open :copy :toggle-read :edit :delete :sep :rename]
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
       (let [r (.getCellRect table (max 0 (.getSelectedRow table)) 0 true)]
         (show-popup! (popup-for :list) table (+ (.x r) 40) (+ (.y r) (.height r)) true))))))

(defn- right-click-menu
  "Mouse: a right-click selects what is under the pointer, then shows its menu.
  In the list, one click on a tag header folds it, a double click opens a link."
  [pane]
  (let [show (fn [^java.awt.event.MouseEvent e]
               (when (.isPopupTrigger e)
                 (let [{:keys [^JTree tree ^JTable table]} @widgets]
                   (if (= pane :tree)
                     (when-let [p (.getPathForLocation tree (.getX e) (.getY e))]
                       (.setSelectionPath tree p))
                     (let [row (.rowAtPoint table (.getPoint e))]
                       (when (>= row 0) (.setRowSelectionInterval table row row))))
                   (show-popup! (popup-for pane) (.getComponent e) (.getX e) (.getY e) false))))]
    (proxy [MouseAdapter] []
      (mousePressed [e] (show e))
      (mouseReleased [e] (show e))
      (mouseClicked [^java.awt.event.MouseEvent e]
        (when (and (= pane :list) (SwingUtilities/isLeftMouseButton e))
          (cond
            (:header? (selected-row)) (fold! :toggle)
            (= 2 (.getClickCount e)) (open-selected!)))))))

;; ---------------------------------------------------------------- building

(defn- accent []
  (or (UIManager/getColor "Component.accentColor") (UIManager/getColor "Component.focusColor")
      (Color. 0x4c 0x8d 0xff)))

(defn- dim [] (or (UIManager/getColor "Label.disabledForeground") Color/GRAY))

(defn- band
  "A header's background: the table's, a little lighter."
  [^Color b]
  (Color. (min 255 (+ 14 (.getRed b))) (min 255 (+ 14 (.getGreen b))) (min 255 (+ 18 (.getBlue b)))))

(defn- hex [^Color c] (format "#%02x%02x%02x" (.getRed c) (.getGreen c) (.getBlue c)))

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

(defn- tags-html [tags]
  (let [esc #(-> (str %) (str/replace "&" "&amp;") (str/replace "<" "&lt;") (str/replace ">" "&gt;"))]
    (str "<html>" (str/join "&nbsp;&nbsp;&nbsp;"
                            (map #(str "<span style='color:" (hex (tag-color %)) "'>" (esc %) "</span>") tags))
         "</html>")))

(defn- setup-look!
  "The saved theme, or FlatLaf Dark. UIManager/put values outlive theme switches."
  []
  (UIManager/put "Table.showHorizontalLines" true)
  (UIManager/put "Tree.showDefaultIcons" true)
  (try
    (UIManager/setLookAndFeel ^String (current-theme))
    (catch Exception e
      (log/event :error :where "theme" :message (str e))
      (if (os/mac?) (FlatMacDarkLaf/setup) (FlatDarkLaf/setup)))))

(defn- build! []
  (let [frame (JFrame. app-name)
        search (JTextField.)
        model (proxy [AbstractTableModel] []
                (getRowCount [] (count (:rows @state)))
                (getColumnCount [] (count columns))
                (getColumnName [c] (first (nth columns c)))
                (getValueAt [r c] (let [[_ cell header-cell] (nth columns c)
                                        row (nth (:rows @state) r)]
                                    ((if (:header? row) header-cell cell) row)))
                (isCellEditable [_ _] false))
        table (doto (JTable. model)
                (.setFillsViewportHeight true)
                (.setSelectionMode ListSelectionModel/SINGLE_SELECTION))
        tree (doto (JTree. (DefaultTreeModel. (DefaultMutableTreeNode. (->Item "Loading ..." nil))))
               (.setRootVisible true))
        status (doto (JLabel. " ") (.setBorder (BorderFactory/createEmptyBorder 4 10 4 10)))
        hint (doto (JLabel. " ")
               (.setBorder (BorderFactory/createEmptyBorder 4 10 4 10))
               (.setForeground (UIManager/getColor "Label.disabledForeground")))
        shift KeyEvent/SHIFT_DOWN_MASK
        actions {:add (action "Add Link..." (menu-key KeyEvent/VK_N) add!)
                 :reload (action "Reload" (menu-key KeyEvent/VK_R) reload!)
                 :open-file (action "Open data.rec in Text Editor" (menu-key KeyEvent/VK_O shift) open-file!)
                 :reveal (action (if (os/mac?) "Reveal data.rec in Finder" "Show data.rec in Folder")
                                 (menu-key KeyEvent/VK_R shift) reveal-file!)
                 :undo (action "Undo" (menu-key KeyEvent/VK_Z) undo!)
                 :edit (action "Edit..." (menu-key KeyEvent/VK_E) edit!)
                 :delete (action "Delete" (menu-key KeyEvent/VK_BACK_SPACE) delete-selected!)
                 :copy (action "Copy URL" (menu-key KeyEvent/VK_C shift) copy-selected!)
                 :toggle-read (action "Mark Read / To Read" (menu-key KeyEvent/VK_D) toggle-read!)
                 :rename (action "Rename Tag..." (KeyStroke/getKeyStroke KeyEvent/VK_F2 0) rename-tag!)
                 :open (action "Open in Browser" (menu-key KeyEvent/VK_O) open-selected!)
                 :find (action "Search" (menu-key KeyEvent/VK_F) focus-search!)
                 :tags (action "Go to Tags" (menu-key KeyEvent/VK_1) focus-tree!)
                 :links (action "Go to Links" (menu-key KeyEvent/VK_2) focus-list!)
                 :menu (action "Show Context Menu" (menu-key KeyEvent/VK_J) show-context-menu!)
                 :help (action "Keyboard Shortcuts" (menu-key KeyEvent/VK_SLASH) show-shortcuts!)
                 :theme (action "Theme..." (menu-key KeyEvent/VK_T) choose-theme!)
                 :next-theme (action "Next Theme" (menu-key KeyEvent/VK_CLOSE_BRACKET) #(cycle-theme! 1))
                 :prev-theme (action "Previous Theme" (menu-key KeyEvent/VK_OPEN_BRACKET) #(cycle-theme! -1))}
        menus [["File" KeyEvent/VK_F [:add :reload :sep :open-file :reveal]]
               ["Edit" KeyEvent/VK_E [:undo :sep :edit :delete :copy :toggle-read :sep :rename]]
               ["View" KeyEvent/VK_V [:find :tags :links :sep :open :menu :sep :theme :next-theme :prev-theme]]
               ["Help" KeyEvent/VK_H [:help]]]]
    (reset! widgets {:frame frame :search search :table table :tree tree :status status :hint hint
                     :actions actions
                     :menu-actions (for [[_ _ ks] menus k ks :when (not= k :sep)] (actions k))})

    ;; search: filters on every keystroke
    (.putClientProperty search "JTextField.placeholderText" "Search title, URL, tag, note")
    (.putClientProperty search "JTextField.showClearButton" true)
    (.addDocumentListener (.getDocument search)
                          (let [changed (fn [] (swap! state assoc :query (.getText search)) (render-table!) (status! (summary)))]
                            (reify DocumentListener
                              (insertUpdate [_ _] (changed))
                              (removeUpdate [_ _] (changed))
                              (changedUpdate [_ _] (changed)))))
    (bind-pane! search :search)

    ;; list: Tab leaves the table instead of moving between cells
    (.setFocusTraversalKeys table KeyboardFocusManager/FORWARD_TRAVERSAL_KEYS nil)
    (.setFocusTraversalKeys table KeyboardFocusManager/BACKWARD_TRAVERSAL_KEYS nil)
    (bind-pane! table :list)
    (.addMouseListener table (right-click-menu :list))

    ;; a click on a column header sorts by it; columns keep their places so the
    ;; click maps to the right sort key
    (let [header (.getTableHeader table)]
      (.setReorderingAllowed header false)
      (.addMouseListener header (proxy [MouseAdapter] []
                                  (mouseClicked [^java.awt.event.MouseEvent e]
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

    ;; tag headers bold in the accent colour on a lighter band; urls dimmed;
    ;; tags coloured by branch. DefaultTableCellRenderer remembers a colour set
    ;; on it for the next cell, so every unselected cell sets both colours.
    (.setDefaultRenderer table Object
                         (proxy [DefaultTableCellRenderer] []
                           (getTableCellRendererComponent [^JTable t value selected? focus? row col]
                             (let [^JLabel c (proxy-super getTableCellRendererComponent t value selected? focus? row col)
                                   r (get (:rows @state) row)
                                   header? (:header? r)
                                   font (.getFont t)]
                               (.setFont c (if header? (.deriveFont font Font/BOLD) font))
                               (when-not selected?
                                 (.setBackground c (if header? (band (.getBackground t)) (.getBackground t)))
                                 (.setForeground c (cond header? (accent) (= col 1) (dim) :else (.getForeground t)))
                                 (when (and (= col 2) (not header?) (seq (:tags r)))
                                   (.setText c (tags-html (:tags r)))))
                               c))))
    (doseq [[i w] (map-indexed vector [420 400 260])]
      (.setPreferredWidth (.getColumn (.getColumnModel table) (int i)) (int w)))

    ;; tree: no type-to-select, so its single keys are free; a tag narrows the list
    (doseq [l (.getKeyListeners tree)] (.removeKeyListener tree l))
    (bind-pane! tree :tree)
    (.addMouseListener tree (right-click-menu :tree))
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
                                      (.setText hint (str/replace (hints pane) "Cmd" (if (os/mac?) "Cmd" "Ctrl")))))))

    (.setJMenuBar frame (let [bar (JMenuBar.)]
                          (doseq [[title mnemonic ks] menus]
                            (let [m (doto (JMenu. ^String title) (.setMnemonic (int mnemonic)))]
                              (doseq [k ks] (if (= k :sep) (.addSeparator m) (.add m (JMenuItem. ^Action (actions k)))))
                              (.add bar m)))
                          bar))

    ;; Tab cycles search -> tags -> links; the toolbar buttons have shortcuts instead
    (let [buttons (for [k [:add :reload]] (doto (JButton. ^Action (actions k)) (.setFocusable false)))
          toolbar (doto (JToolBar.)
                    (.setFloatable false)
                    (.setFocusable false)
                    (.setBorder (BorderFactory/createEmptyBorder 6 8 6 8))
                    (.add search)
                    (.addSeparator))
          split (doto (JSplitPane. JSplitPane/HORIZONTAL_SPLIT (JScrollPane. tree) (JScrollPane. table))
                  (.setFocusable false)
                  (.setDividerLocation (int tree-width)))
          bottom (doto (JPanel. (BorderLayout.))
                   (.add status BorderLayout/CENTER)
                   (.add hint BorderLayout/EAST))]
      (doseq [b buttons] (.add toolbar ^JButton b))
      (doto (.getContentPane frame)
        (.setLayout (BorderLayout.))
        (.add toolbar BorderLayout/NORTH)
        (.add split BorderLayout/CENTER)
        (.add bottom BorderLayout/SOUTH)))

    (doto frame
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.setSize (Dimension. (first window-size) (second window-size)))
      (.setLocationRelativeTo nil)
      (.setVisible true)
      (.toFront))
    (.requestFocusInWindow search)))

(defn start!
  "Open the window on the event thread and load data.rec."
  [{:keys [data-path]}]
  (let [view (load-view)
        first-run? (not (:tag-defaults view))]
    ;; Read starts folded; after that the folds are yours
    (swap! state assoc :data-path data-path
           :folded (cond-> (load-folds) first-run? (conj db/done))
           :sort (or (:sort view) {:key nil :desc? false})
           :theme (:theme view))
    (when first-run?
      (save-folds!)
      (save-view!)))
  (on-edt (fn []
            (setup-look!)
            (build!)
            (style-widgets!)
            (show-sort-in-header!)
            (log/event :start :data data-path)
            (reload!))))
