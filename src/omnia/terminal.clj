(ns omnia.terminal
  (:import (com.googlecode.lanterna TextColor$ANSI SGR)
           (com.googlecode.lanterna.terminal Terminal DefaultTerminalFactory)
           (com.googlecode.lanterna.input KeyType)
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

(defn background! [terminal colour]
  ((:background! terminal) colour))

(defn foreground! [terminal colour]
  ((:foreground! terminal) colour))

(defn style! [terminal style]
  ((:style! terminal) style))

(defn un-style! [terminal style]
  ((:un-style! terminal) style))

(defn visible! [terminal bool]
  ((:visible! terminal) bool))

(defn clear! [terminal]
  ((:clear! terminal)))

(defn move! [terminal x y]
  ((:move! terminal) x y))

(defn put! [terminal ch x y]
  ((:put! terminal) ch x y))

(defn stop! [terminal]
  ((:stop! terminal)))

(defn start! [terminal]
  ((:start! terminal)))

(defn keystroke! [terminal]
  ((:keystroke! terminal)))

(defn size [terminal]
  ((:size terminal)))

(defn terminal []
  (let [charset (Charset/forName "UTF-8")
        factory (doto
                  (DefaultTerminalFactory. System/out System/in charset)
                  (.setForceTextTerminal true))
        terminal ^Terminal (.createTerminal factory)]
    {:background!   (fn [colour]
                      (->> :default (colours colour) (.setBackgroundColor terminal)))
     :foreground!   (fn [colour]
                      (->> :white (colours colour) (.setForegroundColor terminal)))
     :style!        (fn [style]
                      (some->> (styles style) (.enableSGR terminal)))
     :un-style!     (fn [style]
                      (some->> (styles style) (.disableSGR terminal)))
     :clear!        (fn []
                      (doto terminal (.clearScreen) (.setCursorPosition 0 0) (.flush)))
     :size          (fn []
                      (-> terminal (.getTerminalSize) (.getRows)))
     :move!         (fn [x y]
                      (.setCursorPosition terminal x y))
     :put!          (fn [ch x y]
                      (doto terminal
                        (.setCursorPosition x y)
                        (.putCharacter ch)))
     :visible!      (fn [bool]
                      (.setCursorVisible terminal bool))
     :stop!         (fn []
                      (.exitPrivateMode terminal))
     :start!        (fn []
                      (.enterPrivateMode terminal))
     :keystroke!    (fn []
                      (let [pressed (.readInput terminal)
                            event   (-> pressed (.getKeyType) (key-events))]
                        {:key   (if (= :character event) (.getCharacter pressed) event)
                         :ctrl  (.isCtrlDown pressed)
                         :alt   (.isAltDown  pressed)
                         :shift (.isShiftDown pressed)}))}))