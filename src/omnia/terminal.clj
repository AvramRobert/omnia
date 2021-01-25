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
           (java.io File)
           (clojure.lang IPersistentVector Keyword)))

(def TerminalFn (s/enum :move! :put! :size :clear! :refresh! :stop! :start! :get-event!))

(def TerminalSpec
  {TerminalFn s/Any})

(defprotocol Terminal
  (move! [_
          ^Integer x
          ^Integer y])
  (put! [_
         ^Character char
         ^Integer x
         ^Integer y
         ^Keyword fg
         ^Keyword bg
         ^IPersistentVector styles])
  (size [_])
  (clear! [_])
  (refresh! [_])
  (stop! [_])
  (start! [_])
  (get-event! [_]))

;; terminal gray   (TextColor$Indexed/fromRGB (int 51) (int 51) (int 51))
;; terminal yellow (TextColor$Indexed/fromRGB (int 180) (int 148) (int 6))
;; terminal white  (TextColor$Indexed/fromRGB (int 51) (int 51) (int 51))

(def title (str "omnia-" (omnia-version)))

(def Colour (s/enum :black :white :red :green :blue :cyan :magenta :yellow :default))
(def Style (s/enum :bold :blinking :underline :strikethrough)) ;; :reverse :circled :fraktur :reverse (could support, but not yet)

(def preset-colours
  {:black   TextColor$ANSI/BLACK
   :white   (TextColor$Indexed/fromRGB (int 171) (int 174) (int 168)) ;; TextColor$ANSI/WHITE
   :red     TextColor$ANSI/RED
   :green   TextColor$ANSI/GREEN
   :blue    TextColor$ANSI/BLUE
   :cyan    TextColor$ANSI/CYAN
   :magenta TextColor$ANSI/MAGENTA
   :yellow  (TextColor$Indexed/fromRGB (int 180) (int 148) (int 6)) ;;TextColor$ANSI/YELLOW
   :default TextColor$ANSI/DEFAULT})

(def all-styles
  {:bold          SGR/BOLD
   :reverse       SGR/REVERSE
   :blinking      SGR/BLINK
   :underline     SGR/UNDERLINE
   :circled       SGR/CIRCLED
   :strikethrough SGR/CROSSED_OUT
   :fraktur       SGR/FRAKTUR})

(def key-events
  {KeyType/Character      :character
   KeyType/Escape         :escape
   KeyType/Backspace      :backspace
   KeyType/ArrowLeft      :left
   KeyType/ArrowRight     :right
   KeyType/ArrowUp        :up
   KeyType/ArrowDown      :down
   KeyType/Insert         :insert
   KeyType/Delete         :delete
   KeyType/Home           :home
   KeyType/End            :end
   KeyType/PageUp         :page-up
   KeyType/PageDown       :page-down
   KeyType/Tab            :tab
   KeyType/ReverseTab     :reverse-tab
   KeyType/Enter          :enter
   KeyType/F1             :f1
   KeyType/F2             :f2
   KeyType/F3             :f3
   KeyType/F4             :f4
   KeyType/F5             :f5
   KeyType/F6             :f6
   KeyType/F7             :f7
   KeyType/F8             :f8
   KeyType/F9             :f9
   KeyType/F10            :f10
   KeyType/F11            :f11
   KeyType/F12            :f12
   KeyType/F13            :f13
   KeyType/F14            :f14
   KeyType/F15            :f15
   KeyType/F16            :f16
   KeyType/F17            :f17
   KeyType/F18            :f18
   KeyType/F19            :f19
   KeyType/Unknown        :unknown
   KeyType/CursorLocation :cursor-location
   KeyType/MouseEvent     :mouse-event
   KeyType/EOF            :eof})

(s/defn to-key-binding :- c/InternalKeyBinding
  [pressed :- KeyStroke]
  (let [event (-> pressed (.getKeyType) (key-events))]
    {:key   (if (= :character event) (.getCharacter pressed) event)
     :ctrl  (.isCtrlDown pressed)
     :alt   (.isAltDown  pressed)
     :shift (.isShiftDown pressed)}))

;; Can't i actually skip one hop and just read the input directly to an event?
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

(s/defn text-char :- TextCharacter
  [char       :- Character
   foreground :- Colour
   background :- Colour
   styles     :- [Style]]
  (let [styles    ^IPersistentVector styles
        text-char ^TextCharacter (-> (TextCharacter. char)
                                     (.withBackgroundColor (preset-colours background :default))
                                     (.withForegroundColor (preset-colours foreground :white)))]
    (if (> (.count styles) 0) ;; much faster
      (->> styles (mapv all-styles) (.withModifiers text-char))
      text-char)))

(s/defn impl-put! :- nil
  [terminal   :- TerminalScreen
   char       :- Character
   x          :- s/Int
   y          :- s/Int
   foreground :- Colour
   background :- Colour
   styles     :- [Style]]
  (->> (text-char char foreground background styles)
       (.setCharacter terminal x y)))

(s/defn impl-move! [t :- TerminalScreen
                    x :- s/Int
                    y :- s/Int]
  (.setCursorPosition t (TerminalPosition. x y)))

(s/defn impl-size! :- s/Int
  [screen :- TerminalScreen]
  (-> screen (.getTerminalSize) (.getRows)))

(s/defn impl-clear! :- nil
  [screen :- TerminalScreen]
  (.clear screen))

(s/defn impl-stop! :- nil
  [screen :- TerminalScreen]
  (.stopScreen screen))

(s/defn impl-start! :- nil
  [screen :- TerminalScreen]
  (.startScreen screen))

(s/defn impl-refresh! :- nil
  [screen :- TerminalScreen]
  (.refresh screen))

(s/defn impl-get-event! :- nil
  [screen :- TerminalScreen, config :- c/Config]
  (-> screen (.readInput) (to-key-binding) (to-event config)))

(defn- custom-font [^String path]
  (->> (File. path) (Font/createFont Font/TRUETYPE_FONT)))

(defn- font-of [path size]
  (-> path (custom-font) (.deriveFont Font/BOLD (float size))))

(defn- derive-palette [config]
  (TerminalEmulatorColorConfiguration/newInstance TerminalEmulatorPalette/GNOME_TERMINAL))

(defn- derive-font [config]
  (SwingTerminalFontConfiguration/newInstance (into-array Font [(font-of "./Hasklig-Regular.otf" 15)])))

(s/defn terminal :- Terminal
  [config :- c/Config]
  (let [screen ^TerminalScreen (-> (DefaultTerminalFactory.)
                                   (.setTerminalEmulatorColorConfiguration (derive-palette config))
                                   (.setTerminalEmulatorFontConfiguration (derive-font config))
                                   (.setTerminalEmulatorTitle title)
                                   (.createTerminalEmulator)
                                   (TerminalScreen.))]
    (reify Terminal
      (refresh! [_]
        (impl-refresh! screen))
      (stop! [_]
        (impl-stop! screen))
      (start! [_]
        (impl-start! screen))
      (clear! [_]
        (impl-clear! screen))
      (move! [_ x y]
        (impl-move! screen x y))
      (get-event! [_]
        (impl-get-event! screen config))
      (size [_]
        (impl-size! screen))
      (put! [_ ch x y fg bg stls]
        (impl-put! screen ch x y fg bg stls)))))