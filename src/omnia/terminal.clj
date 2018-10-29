(ns omnia.terminal
  (:import (com.googlecode.lanterna TextColor$ANSI SGR)
           (com.googlecode.lanterna.terminal Terminal DefaultTerminalFactory)
           (com.googlecode.lanterna.input KeyType KeyStroke)
           (java.nio.charset Charset)))

; === Lanterna Terminal ===

(def colours
  {:black   TextColor$ANSI/BLACK
   :white   TextColor$ANSI/WHITE
   :red     TextColor$ANSI/RED
   :green   TextColor$ANSI/GREEN
   :blue    TextColor$ANSI/BLUE
   :cyan    TextColor$ANSI/CYAN
   :magenta TextColor$ANSI/MAGENTA
   :yellow  TextColor$ANSI/YELLOW
   :default TextColor$ANSI/DEFAULT})

(def styles
  {:bold          SGR/BOLD
   :reverse       SGR/REVERSE
   :blinking      SGR/BLINK
   :underline     SGR/UNDERLINE
   :circled       SGR/CIRCLED
   :strikethrough SGR/CROSSED_OUT
   :fraktur       SGR/FRAKTUR})

(def key-events
  {KeyType/Character  :character
   KeyType/Escape     :escape
   KeyType/Backspace  :backspace
   KeyType/ArrowLeft  :left
   KeyType/ArrowRight :right
   KeyType/ArrowUp    :up
   KeyType/ArrowDown  :down
   KeyType/Insert     :insert
   KeyType/Delete     :delete
   KeyType/Home       :home
   KeyType/End        :end
   KeyType/PageUp     :page-up
   KeyType/PageDown   :page-down
   KeyType/Tab        :tab
   KeyType/ReverseTab :reverse-tab
   KeyType/Enter      :enter
   KeyType/F1         :f1
   KeyType/F2         :f2
   KeyType/F3         :f3
   KeyType/F4         :f4
   KeyType/F5         :f5
   KeyType/F6         :f6
   KeyType/F7         :f7
   KeyType/F8         :f8
   KeyType/F9         :f9
   KeyType/F10        :f10
   KeyType/F11        :f11
   KeyType/F12        :f12
   KeyType/F13        :f13
   KeyType/F14        :f14
   KeyType/F15        :f15
   KeyType/F16        :f16
   KeyType/F17        :f17
   KeyType/F18        :f18
   KeyType/F19        :f19
   KeyType/Unknown    :unknown
   KeyType/CursorLocation :cursor-location
   KeyType/MouseEvent     :mouse-event
   KeyType/EOF            :eof})

(defrecord Term
  [background!
   foreground!
   style!
   un-style!
   size
   move!
   clear!
   visible!
   stop!
   start!
   poll-key!
   get-key!])

(defn background! [^Term terminal colour]
  ((:background! terminal) colour))

(defn foreground! [^Term terminal colour]
  ((:foreground! terminal) colour))

(defn style! [^Term terminal style]
  ((:style! terminal) style))

(defn un-style! [^Term terminal style]
  ((:un-style! terminal) style))

(defn visible! [^Term terminal bool]
  ((:visible! terminal) bool))

(defn move! [^Term terminal x y]
  ((:move! terminal) x y))

(defn put! [^Term terminal ch x y]
  ((:put! terminal) ch x y))

(defn clear! [^Term terminal]
  ((:clear! terminal)))

(defn stop! [^Term terminal]
  ((:stop! terminal)))

(defn start! [^Term terminal]
  ((:start! terminal)))

(defn get-key! [^Term terminal]
  ((:get-key! terminal)))

(defn poll-key! [^Term terminal]
  ((:poll-key! terminal)))

(defn size [^Term terminal]
  ((:size terminal)))

(defn- match-key [^KeyStroke pressed]
  (let [event (-> pressed (.getKeyType) (key-events))]
    {:key   (if (= :character event) (.getCharacter pressed) event)
     :ctrl  (.isCtrlDown pressed)
     :alt   (.isAltDown  pressed)
     :shift (.isShiftDown pressed)}))

(defn terminal []
  (let [charset (Charset/forName "UTF-8")
        terminal ^Terminal (-> (DefaultTerminalFactory. System/out System/in charset)
                               (.setForceTextTerminal true)
                               (.createTerminal))]
    (map->Term
      {:background!   (fn [colour]
                        (->> :default (colours colour) (.setBackgroundColor terminal)))
       :foreground!   (fn [colour]
                        (->> :white (colours colour) (.setForegroundColor terminal)))
       :style!        (fn [style]
                        (some->> (styles style) (.enableSGR terminal)))
       :un-style!     (fn [style]
                        (some->> (styles style) (.disableSGR terminal)))
       :size          (fn []
                        (-> terminal (.getTerminalSize) (.getRows)))
       :move!         (fn [x y]
                        (.setCursorPosition terminal x y))
       :put!          (fn [ch x y]
                        (doto terminal
                          (.setCursorPosition x y)
                          (.putCharacter ch)))
       :clear!        (fn []
                        (doto terminal
                          (.flush)
                          (.clearScreen)))
       :visible!      (fn [bool]
                        (.setCursorVisible terminal bool))
       :stop!         (fn []
                        (.exitPrivateMode terminal))
       :start!        (fn []
                        (.enterPrivateMode terminal))
       :poll-key!     (fn []
                        (some-> terminal (.pollInput) (match-key)))
       :get-key!      (fn []
                        (-> terminal (.readInput) (match-key)))})))