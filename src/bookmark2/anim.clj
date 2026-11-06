(ns bookmark2.anim
  "Motion for the window: short, eased, and never in the way.

    glide!        scroll a rect into view; a jump of more than three rows
                  glides over 180 ms instead of snapping
    cross-fade!   snapshot the window, run a change under it (a theme switch),
                  then fade the snapshot out over 250 ms
    fade-in!      show a window from transparent to opaque over 120 ms
    fade-popups!  every heavy-weight pop-up (menus, tooltips) fades in
    spinner       a small arc that turns at 60 frames a second while it shows

  Everything runs on the Swing event thread through FlatLaf's Animator (a
  frame every 10 ms). With `on?` false the glides, fades and cross-fades are
  skipped; the spinner still turns, since it says the app is busy."
  (:import (com.formdev.flatlaf.util Animator Animator$TimingTarget CubicBezierEasing UIScale)
           (java.awt AlphaComposite BasicStroke Color Component Dimension Graphics Graphics2D
                     GraphicsDevice$WindowTranslucency Point Rectangle RenderingHints Window)
           (java.awt.event ActionListener HierarchyListener)
           (java.awt.image BufferedImage)
           (java.util WeakHashMap)
           (javax.swing JComponent JFrame JLayeredPane JViewport Popup PopupFactory SwingUtilities Timer UIManager)))

(defonce on? (atom true))

(defn- animate!
  "(step f) with f eased from 0 to 1 over ms, then (done). Returns the
  Animator, so a newer motion can cancel it."
  [ms step & [done]]
  (doto (Animator. (int ms) (reify Animator$TimingTarget
                              (timingEvent [_ f] (step f))
                              (end [_] (when done (done)))))
    (.setInterpolator CubicBezierEasing/EASE_OUT)
    .start))

;; ---------------------------------------------------------------- glide

(defonce ^:private glides (WeakHashMap.))

(defn glide!
  "Scroll c so r shows, as scrollRectToVisible does. A jump of more than
  three rows (r's height) glides; a short step is instant, so holding j or k
  never lags."
  [^JComponent c ^Rectangle r]
  (let [^JViewport vp (SwingUtilities/getAncestorOfClass JViewport c)]
    (some-> ^Animator (.get glides vp) .cancel)
    (if-not (and vp @on?)
      (.scrollRectToVisible c r)
      (let [from (.getViewPosition vp)
            _ (.scrollRectToVisible c r)
            to (.getViewPosition vp)
            dy (- (.y to) (.y from))]
        (when (> (Math/abs dy) (* 3 (.height r)))
          (.setViewPosition vp from)
          (.put glides vp (animate! 180 #(.setViewPosition vp (Point. (.x to) (int (+ (.y from) (* (double %) dy))))))))))))

;; ---------------------------------------------------------------- cross-fade

(defonce ^:private fading (atom nil))

(defn- snapshot
  "The frame's root pane as an image at the screen's own scale, so a Retina
  snapshot stays sharp."
  [^JFrame frame]
  (let [root (.getRootPane frame)
        scale (.getScaleX (.getDefaultTransform (.getGraphicsConfiguration frame)))
        img (BufferedImage. (int (Math/ceil (* scale (.getWidth root))))
                            (int (Math/ceil (* scale (.getHeight root))))
                            BufferedImage/TYPE_INT_ARGB)
        g (.createGraphics img)]
    (.scale g scale scale)
    (.paint root g)
    (.dispose g)
    img))

(defn cross-fade!
  "Run (change) under a snapshot of frame, then fade the snapshot out, so a
  theme switch dissolves instead of flashing. A fade still running is
  dropped first."
  [^JFrame frame change]
  (when-let [{:keys [^Animator anim ^JComponent overlay]} @fading]
    (reset! fading nil)
    (.cancel anim)
    (when-let [p (.getParent overlay)] (.remove p overlay) (.repaint p)))
  (if-not (and @on? frame (.isShowing frame))
    (change)
    (let [img (snapshot frame)
          layers (.getLayeredPane frame)
          w (.getWidth layers)
          h (.getHeight layers)
          alpha (atom 1.0)
          overlay (proxy [JComponent] []
                    (paintComponent [^Graphics g]
                      (let [g2 ^Graphics2D (.create g)]
                        (.setComposite g2 (AlphaComposite/getInstance AlphaComposite/SRC_OVER (float @alpha)))
                        (.setRenderingHint g2 RenderingHints/KEY_INTERPOLATION RenderingHints/VALUE_INTERPOLATION_BILINEAR)
                        (.drawImage g2 img 0 0 (int w) (int h) nil)
                        (.dispose g2))))]
      (.setBounds overlay 0 0 w h)
      (.add layers overlay JLayeredPane/DRAG_LAYER)
      (try
        (change)
        (finally
          (reset! fading {:overlay overlay
                          :anim (animate! 250
                                          (fn [f] (reset! alpha (- 1.0 f)) (.repaint overlay))
                                          (fn []
                                            (.remove layers overlay)
                                            (.repaint layers)
                                            (reset! fading nil)))}))))))

;; ---------------------------------------------------------------- fades

(defonce ^:private fades (WeakHashMap.))

(defn- set-opacity! [^Window w f]
  (try (.setOpacity w (float f)) (catch Exception _ nil)))

(defn- translucent? [^Window w]
  (some-> (.getGraphicsConfiguration w) .getDevice
          (.isWindowTranslucencySupported GraphicsDevice$WindowTranslucency/TRANSLUCENT)))

(defn fade-in!
  "Show w with (show), from transparent to opaque over 120 ms. A window that
  cannot be translucent just shows."
  [^Window w show]
  (some-> ^Animator (.get fades w) .cancel)
  (if-not (and @on? w (translucent? w))
    (show)
    (do (set-opacity! w 0)
        (show)
        (.put fades w (animate! 120 #(set-opacity! w %) #(set-opacity! w 1))))))

(defonce ^:private wrapper (atom nil))

(defn fade-popups!
  "Wrap the shared pop-up factory so heavy-weight pop-ups (their own window:
  menus, tooltips) fade in. FlatLaf installs a new factory with every theme,
  so this runs again after each switch; a second call without one is a no-op."
  []
  (let [^PopupFactory base (PopupFactory/getSharedInstance)]
    (when-not (identical? base @wrapper)
      (let [wrap (fn [^Popup p ^Component owner ^Component contents]
                   (let [w (SwingUtilities/getWindowAncestor contents)]
                     (if (and w (not= w owner) (not= w (some-> owner SwingUtilities/getWindowAncestor)))
                       (proxy [Popup] []
                         (show [] (fade-in! w #(.show p)))
                         (hide [] (.hide p)))
                       p)))
            factory (proxy [PopupFactory] []
                      (getPopup
                        ([owner contents x y] (wrap (.getPopup base owner contents x y) owner contents))
                        ([owner contents x y _] (wrap (.getPopup base owner contents x y) owner contents))))]
        (reset! wrapper factory)
        (PopupFactory/setSharedInstance factory)))))

;; ---------------------------------------------------------------- spinner

(defn spinner
  "A small arc in the accent colour that turns once a second, 60 frames a
  second, while it is showing; its timer stops when it is hidden."
  []
  (let [angle (atom 0)
        c (proxy [JComponent] []
            (getPreferredSize []
              (let [d (UIScale/scale (int 16))] (Dimension. d d)))
            (paintComponent [^Graphics g]
              (let [g2 ^Graphics2D (.create g)
                    pad (UIScale/scale (int 2))
                    d (int (- (min (.getWidth ^JComponent this) (.getHeight ^JComponent this)) (* 2 pad)))]
                (.setRenderingHint g2 RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
                (.setStroke g2 (BasicStroke. (float (UIScale/scale (float 2))) BasicStroke/CAP_ROUND BasicStroke/JOIN_ROUND))
                (.setColor g2 (or (UIManager/getColor "Component.accentColor") (UIManager/getColor "Component.focusColor") Color/GRAY))
                (.drawArc g2 pad pad d d (- (int @angle)) 270)
                (.dispose g2))))
        timer (Timer. 16 (reify ActionListener
                           (actionPerformed [_ _]
                             (swap! angle #(mod (+ % 6) 360))
                             (.repaint ^JComponent c))))]
    (.addHierarchyListener c (reify HierarchyListener
                               (hierarchyChanged [_ _]
                                 (if (.isShowing ^JComponent c) (.start timer) (.stop timer)))))
    c))
