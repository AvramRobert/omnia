(ns omnia.terminal
  (:require [schema.core :as s]
            [omnia.config :as c]
            [omnia.event :as e]
            [omnia.more :refer [=> omnia-version]])
  (:import (com.googlecode.lanterna TextColor$ANSI SGR TerminalPosition TextCharacter TextColor$Indexed)
           (com.googlecode.lanterna.terminal DefaultTerminalFactory)
           (com.googlecode.lanterna.input KeyType KeyStroke)
           (com.googlecode.lanterna.screen TerminalScreen)
           (com.googlecode.lanterna.terminal.swing TerminalEmulatorColorConfiguration TerminalEmulatorPalette SwingTerminalFontConfiguration)
           (java.awt Font)
           (java.io File)))

; === Lanterna Terminal ===

;; terminal gray   (TextColor$Indexed/fromRGB (int 51) (int 51) (int 51))
;; terminal yellow (TextColor$Indexed/fromRGB (int 180) (int 148) (int 6))
;; terminal white  (TextColor$Indexed/fromRGB (int 51) (int 51) (int 51))

(def title (str "omnia-" (omnia-version)))

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

(def Terminal
  {:size       s/Any
   :move!      s/Any
   :clear!     s/Any
   :refresh!   s/Any
   :stop!      s/Any
   :start!     s/Any
   :put!       s/Any
   :get-event! s/Any})

(s/defn to-key-binding :- c/InternalKeyBinding
  [pressed :- KeyStroke]
  (let [event (-> pressed (.getKeyType) (key-events))]
    {:key   (if (= :character event) (.getCharacter pressed) event)
     :ctrl  (.isCtrlDown pressed)
     :alt   (.isAltDown  pressed)
     :shift (.isShiftDown pressed)}))

(s/defn to-event :- e/Event
  [config :- c/Config,
   key-binding :- c/InternalKeyBinding]
  (let [key        (:key key-binding)
        action     (-> config (:keymap) (get key-binding))
        ;control?   (and (char? key)
        ;                (Character/isISOControl ^Character char)) ;; Swing gives me control characters
        unknown?   (and (nil? action)
                        (not (char? key)))
        character? (and (nil? action)
                        (char? key))]
    (cond
      ;      control?   ()
      unknown?   (e/event e/ignore)
      character? (e/event e/character key)
      :else      (e/event action))))

(defn move! [terminal x y]                            ;; FIXME: Find a way to make these implementations
  ((:move! terminal) x y))

(defn put! [terminal ch x y foreground background styles]
  ((:put! terminal) ch x y foreground background styles))

(defn clear! [terminal]
  ((:clear! terminal)))

(defn stop! [terminal]
  ((:stop! terminal)))

(defn start! [terminal]
  ((:start! terminal)))

(defn refresh! [terminal]
  ((:refresh! terminal)))

(defn get-event! [terminal]
  ((:get-event! terminal)))

(defn size [terminal]
  ((:size terminal)))

(defn- pos [x y]
  (TerminalPosition. x y))

(defn- text-char [^Character ch fg bg stls]
  (-> (TextCharacter. ch)
      (.withBackgroundColor (colours bg :default))
      (.withForegroundColor (colours fg :white))
      (cond->
        (not (empty? stls)) (.withModifiers (mapv styles stls)))))

(defn- custom-font [^String path]
  (->> (File. path) (Font/createFont Font/TRUETYPE_FONT)))

(defn- font-of [path size]
  (-> path (custom-font) (.deriveFont Font/BOLD (float size))))

(defn- derive-palette [config]
  (TerminalEmulatorColorConfiguration/newInstance TerminalEmulatorPalette/GNOME_TERMINAL))

(defn- derive-font [config]
  (SwingTerminalFontConfiguration/newInstance (into-array Font [(font-of "./Hasklig-Regular.otf" 15)])))

(s/defn terminal [config :- c/Config] :- Terminal
  (let [terminal (-> (DefaultTerminalFactory.)
                     (.setTerminalEmulatorColorConfiguration (derive-palette config))
                     (.setTerminalEmulatorFontConfiguration (derive-font config))
                     (.setTerminalEmulatorTitle title)
                     (.createTerminalEmulator)
                     (TerminalScreen.))]
    {:size       (fn []
                   (-> terminal (.getTerminalSize) (.getRows)))

     :move!      (fn [x y]
                   (.setCursorPosition terminal (pos x y)))

     :put!       (fn [^Character ch x y fg bg stls]
                   (.setCharacter terminal x y (text-char ch fg bg stls)))

     :clear!     (fn [] (.clear terminal))

     :refresh!   (fn [] (.refresh terminal))

     :stop!      (fn []
                   (.stopScreen terminal true))

     :start!     (fn []
                   (.startScreen terminal))

     :get-event! (fn []
                   (->> terminal (.readInput) (to-key-binding) (to-event config)))}))