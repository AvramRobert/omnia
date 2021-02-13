(ns omnia.view.terminal
  (:require [schema.core :as s]
            [omnia.config.components.event :as e]
            [omnia.config.components.text :as t]
            [omnia.config.components.core :as c]
            [omnia.config.components.input :as i]
            [clojure.set :refer [map-invert]]
            [omnia.config.core :refer [Config]]
            [omnia.util.collection :refer [map-vals map-keys]]
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

(s/def to-sgr :- {t/Style SGR}
  {t/bold          SGR/BOLD
   t/blinking      SGR/BLINK
   t/underline     SGR/UNDERLINE
   t/strikethrough SGR/CROSSED_OUT})

(s/def to-key-type :- {i/Key KeyType}
  {i/escape      KeyType/Escape
   i/backspace   KeyType/Backspace
   i/left        KeyType/ArrowLeft
   i/right       KeyType/ArrowRight
   i/up          KeyType/ArrowUp
   i/down        KeyType/ArrowDown
   i/insert      KeyType/Insert
   i/delete      KeyType/Delete
   i/home        KeyType/Home
   i/end         KeyType/End
   i/page-up     KeyType/PageUp
   i/page-down   KeyType/PageDown
   i/tab         KeyType/Tab
   i/reverse-tab KeyType/ReverseTab
   i/enter       KeyType/Enter
   i/f1          KeyType/F1
   i/f2          KeyType/F2
   i/f3          KeyType/F3
   i/f4          KeyType/F4
   i/f5          KeyType/F5
   i/f6          KeyType/F6
   i/f7          KeyType/F7
   i/f8          KeyType/F8
   i/f9          KeyType/F9
   i/f10         KeyType/F10
   i/f11         KeyType/F11
   i/f12         KeyType/F12
   i/f13         KeyType/F13
   i/f14         KeyType/F14
   i/f15         KeyType/F15
   i/f16         KeyType/F16
   i/f17         KeyType/F17
   i/f18         KeyType/F18
   i/f19         KeyType/F19})

(s/defn to-text-colour :- TextColor
  [[r g b] :- t/RGBColour]
  (TextColor$RGB. ^Integer r ^Integer g ^Integer b))

(s/defn to-key-stroke :- KeyStroke
  [{:keys [key ctrl alt shift]} :- c/KeyBinding]
   (let [key-type (cond (char? key) key
                        (contains? to-key-type key) (to-key-type key)
                        :else KeyType/Unknown)]
     (KeyStroke. key-type ctrl alt shift)))

(s/defn text-colours :- {t/RGBColour TextColor}
  [syntax :- c/Syntax]
  (->> syntax (vals) (mapcat vals) (set) (map (juxt identity to-text-colour)) (into {})))

;; maps out all unicode characters; sums up to about 350 kbytes
;; the first 32 unicode character are control characters
;; the supported ones are already mapped by lanterna to special keys
(s/def text-events :- {Character e/Event}
  (->> (range 32 65535)
       (map (juxt char (comp e/char-event char)))
       (into {})))

(s/defn context-events :- {KeyStroke e/Event}
  [keymap :- c/KeyMap]
   (->> keymap
        (map (juxt (comp to-key-stroke val)
                   (comp e/event key)))
        (into {})))

;; uses a memoised hashmap of all objects to reduce overhead
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

;; uses a memoised hashmap of all objects to reduce overhead
(s/defn impl-get-event! :- e/Event
  [screen         :- TerminalScreen
   context-events :- {KeyStroke e/ContextEvent}
   text-events    :- {Character e/TextEvent}]
  (let [input ^KeyStroke (.readInput screen)]
    (or (get context-events input)
        (get text-events (.getCharacter input))
        e/ignore-event)))

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

(s/defn derive-palette :- TerminalEmulatorColorConfiguration
  [config :- Config]
  (if-let [palette (-> config (:terminal) (t/palette))]
    (TerminalEmulatorColorConfiguration/newInstance palette)
    (TerminalEmulatorColorConfiguration/newInstance TerminalEmulatorPalette/GNOME_TERMINAL)))

(s/defn derive-font :- SwingTerminalFontConfiguration
  [config :- Config]
  (let [font-path ^String (-> config (:terminal) (t/font-path))
        font-size ^Float  (-> config (:terminal) (t/font-size) (float))
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
        memoised-context-events     (-> config (:keymap) (context-events))
        memoised-text-events        text-events
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
        (impl-get-event! screen memoised-context-events memoised-text-events))
      (put! [_ ch x y fg bg stls]
        (impl-put! screen ch x y fg bg stls memoised-colours)))))