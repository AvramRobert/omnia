(ns omnia.view.terminal
  (:require [schema.core :as s]
            [omnia.config.components.event :as e]
            [omnia.config.components.text :as t]
            [omnia.config.components.core :as c]
            [omnia.config.components.input :as i]
            [omnia.util.schema :refer [=>]]
            [omnia.config.core :refer [Config]]
            [omnia.util.collection :refer [map-vals]]
            [omnia.util.misc :refer [omnia-version]]
            [omnia.util.debug :refer [debug]])
  (:import (com.googlecode.lanterna SGR TerminalPosition TextCharacter TextColor TextColor$RGB)
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

(s/defn to-text-colour :- TextColor
  [[r g b] :- t/RGBColour]
  (TextColor$RGB. ^Integer r ^Integer g ^Integer b))

(s/defn text-colours :- {t/RGBColour TextColor}
  [syntax :- c/Syntax]
  (->> syntax (vals) (mapcat vals) (set) (map (juxt identity to-text-colour)) (into {})))

(s/def to-sgr :- {t/Style SGR}
  {t/bold          SGR/BOLD
   t/blinking      SGR/BLINK
   t/underline     SGR/UNDERLINE
   t/strikethrough SGR/CROSSED_OUT})

(s/def to-key :- {KeyType i/Key}
  {KeyType/Escape         i/escape
   KeyType/Backspace      i/backspace
   KeyType/ArrowLeft      i/left
   KeyType/ArrowRight     i/right
   KeyType/ArrowUp        i/up
   KeyType/ArrowDown      i/down
   KeyType/Insert         i/insert
   KeyType/Delete         i/delete
   KeyType/Home           i/home
   KeyType/End            i/end
   KeyType/PageUp         i/page-up
   KeyType/PageDown       i/page-down
   KeyType/Tab            i/tab
   KeyType/ReverseTab     i/reverse-tab
   KeyType/Enter          i/enter
   KeyType/F1             i/f1
   KeyType/F2             i/f2
   KeyType/F3             i/f3
   KeyType/F4             i/f4
   KeyType/F5             i/f5
   KeyType/F6             i/f6
   KeyType/F7             i/f7
   KeyType/F8             i/f8
   KeyType/F9             i/f9
   KeyType/F10            i/f10
   KeyType/F11            i/f11
   KeyType/F12            i/f12
   KeyType/F13            i/f13
   KeyType/F14            i/f14
   KeyType/F15            i/f15
   KeyType/F16            i/f16
   KeyType/F17            i/f17
   KeyType/F18            i/f18
   KeyType/F19            i/f19
   KeyType/Unknown        i/unknown})

(s/defn to-key-binding :- c/KeyBinding
  [pressed :- KeyStroke]
  ;; control characters should be caught here and transformed into appropriate key bindings
  (let [key-type (.getKeyType pressed)]
    {:key   (if (= KeyType/Character key-type)
              (.getCharacter pressed)
              (to-key key-type i/unknown))
     :ctrl  (.isCtrlDown pressed)
     :alt   (.isAltDown  pressed)
     :shift (.isShiftDown pressed)}))

;; Can't i actually skip one hop and just read the input directly to an event?
(s/defn to-event :- e/Event
  [keymap      :- c/KeyMap,
   key-binding :- c/KeyBinding]
  (let [key        (:key key-binding)
        action     (get keymap key-binding)
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

(s/defn impl-put! :- nil
  [terminal   :- TerminalScreen
   char       :- Character
   x          :- s/Int
   y          :- s/Int
   foreground :- t/RGBColour
   background :- t/RGBColour
   styles     :- [t/Style]
   colours    :- {t/RGBColour TextColor}]
  (let [styles ^IPersistentVector styles
        text-char ^TextCharacter (-> (TextCharacter. char)
                                     (.withBackgroundColor (colours background))
                                     (.withForegroundColor (colours foreground)))]
    (if (> (.count styles) 0)
      (->> styles (mapv to-sgr) (.withModifiers text-char) (.setCharacter terminal x y))
      (.setCharacter terminal x y text-char))))

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

(s/defn impl-get-event! :- e/Event
  [screen :- TerminalScreen, keymap :- c/KeyMap]
  (->> screen (.readInput) (to-key-binding) (to-event keymap)))

(s/defn derive-palette :- TerminalEmulatorColorConfiguration
  [config :- Config]
  (if-let [palette (-> config (:terminal) (:palette))]
    (TerminalEmulatorColorConfiguration/newInstance palette)
    (TerminalEmulatorColorConfiguration/newInstance TerminalEmulatorPalette/GNOME_TERMINAL)))

(s/defn derive-font :- SwingTerminalFontConfiguration
  [config :- Config]
  (let [font-path ^String (-> config (:terminal) (:font-path))
        font-size ^Float  (-> config (:terminal) (:font-size) (float))
        font      ^Font   (-> Font/TRUETYPE_FONT
                              (Font/createFont (File. font-path))
                              (.deriveFont Font/BOLD font-size))]
    (SwingTerminalFontConfiguration/newInstance (into-array Font [font]))))

(s/defn terminal :- Terminal
  [config :- Config]
  (let [screen  ^TerminalScreen (-> (DefaultTerminalFactory.)
                                    (.setTerminalEmulatorColorConfiguration (derive-palette config))
                                    (.setTerminalEmulatorFontConfiguration (derive-font config))
                                    (.setTerminalEmulatorTitle (str "omnia-" (omnia-version)))
                                    (.createTerminalEmulator)
                                    (TerminalScreen.))
        memoised-keymap             (-> config (:keymap))
        memoised-colours            (-> config (:syntax) (text-colours))]
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
      (size [_]
        (impl-size! screen))
      (get-event! [_]
        (impl-get-event! screen memoised-keymap))
      (put! [_ ch x y fg bg stls]
        (impl-put! screen ch x y fg bg stls memoised-colours)))))