(ns omnia.terminal
  (:import (com.googlecode.lanterna TextColor$ANSI SGR TerminalPosition TextCharacter TextColor$Indexed TextColor)
           (com.googlecode.lanterna.terminal Terminal DefaultTerminalFactory)
           (com.googlecode.lanterna.input KeyType KeyStroke)
           (java.nio.charset Charset)
           (com.googlecode.lanterna.screen TerminalScreen)
           (com.googlecode.lanterna.terminal.swing SwingTerminalFrame TerminalEmulatorColorConfiguration TerminalEmulatorPalette AWTTerminalFontConfiguration)
           (java.awt Color Font)))

; === Lanterna Terminal ===

;; terminal gray   (TextColor$Indexed/fromRGB (int 51) (int 51) (int 51))
;; terminal yellow (TextColor$Indexed/fromRGB (int 180) (int 148) (int 6))
;; terminal white  (TextColor$Indexed/fromRGB (int 51) (int 51) (int 51))

(def colours                                                ;; FIXME Customize for screen
  {:black   TextColor$ANSI/BLACK
   :white   (TextColor$Indexed/fromRGB (int 171) (int 174) (int 168)) ;; TextColor$ANSI/WHITE
   :red     TextColor$ANSI/RED
   :green   TextColor$ANSI/GREEN
   :blue    TextColor$ANSI/BLUE
   :cyan    TextColor$ANSI/CYAN
   :magenta TextColor$ANSI/MAGENTA
   :yellow  (TextColor$Indexed/fromRGB (int 180) (int 148) (int 6)) ;;TextColor$ANSI/YELLOW
   :default TextColor$ANSI/DEFAULT}
  )

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
  [size
   move!
   clear!
   stop!
   start!
   get-key!])

(defn move! [^Term terminal x y]                            ;; FIXME: Find a way to make these implementations
  ((:move! terminal) x y))

(defn put! [^Term terminal ch x y foreground background styles]
  ((:put! terminal) ch x y foreground background styles))

(defn clear! [^Term terminal]
  ((:clear! terminal)))

(defn stop! [^Term terminal]
  ((:stop! terminal)))

(defn start! [^Term terminal]
  ((:start! terminal)))

(defn refresh! [^Term terminal]
  ((:refresh! terminal)))

(defn get-key! [^Term terminal]
  ((:get-key! terminal)))

(defn size [^Term terminal]
  ((:size terminal)))

(defn- match-key [^KeyStroke pressed]
  (let [event (-> pressed (.getKeyType) (key-events))]
    {:key   (if (= :character event) (.getCharacter pressed) event)
     :ctrl  (.isCtrlDown pressed)
     :alt   (.isAltDown  pressed)
     :shift (.isShiftDown pressed)}))

(defn- pos [x y]
  (TerminalPosition. x y))

(defn- text-char [^Character ch fg bg stls]
  (-> (TextCharacter. ch)
      (.withBackgroundColor (colours bg :default))
      (.withForegroundColor (colours fg :white))
      (cond->
        (not (empty? stls)) (.withModifiers (mapv styles stls)))))


(defn terminal []
  (let [colour-config (TerminalEmulatorColorConfiguration/newInstance TerminalEmulatorPalette/GNOME_TERMINAL)
        font-config   (AWTTerminalFontConfiguration/newInstance (into-array Font [(Font/decode "Monospaced")]))
        terminal (-> (DefaultTerminalFactory.)
                     (.setTerminalEmulatorColorConfiguration colour-config)
                     (.setTerminalEmulatorFontConfiguration font-config)
                     (.createTerminalEmulator)
                     (TerminalScreen.))]
    (map->Term
      {:size      (fn []
                    (-> terminal (.getTerminalSize) (.getRows)))

       :move!     (fn [x y]
                    (.setCursorPosition terminal (pos x y)))

       :put!      (fn [^Character ch x y fg bg stls]
                    (.setCharacter terminal x y (text-char ch fg bg stls)))

       :clear!    (fn [] (.clear terminal))

       :refresh!  (fn [] (.refresh terminal))

       :stop!     (fn []
                    (.stopScreen terminal true))

       :start!    (fn []
                    (.startScreen terminal))

       :get-key!  (fn []
                    (-> terminal (.readInput) (match-key)))})))