(ns omnia.view.terminal
  (:require [schema.core :as s]
            [omnia.schema.event :as e]
            [omnia.schema.syntax :as st]
            [omnia.schema.config :as c]
            [omnia.schema.keymap :as k]
            [clojure.set :refer [map-invert]]
            [omnia.repl.events :refer [character event ignore]]
            [omnia.util.collection :refer [map-vals]]
            [omnia.util.misc :refer [omnia-version]])
  (:import (com.googlecode.lanterna SGR TerminalPosition TextCharacter TextColor TextColor$RGB)
           (com.googlecode.lanterna.terminal DefaultTerminalFactory TerminalResizeListener)
           (com.googlecode.lanterna.input KeyType KeyStroke)
           (com.googlecode.lanterna.screen TerminalScreen)
           (com.googlecode.lanterna.terminal.swing TerminalEmulatorColorConfiguration TerminalEmulatorPalette SwingTerminalFontConfiguration)
           (java.awt Font)
           (java.io File)
           (java.util EnumSet Collection)
           (clojure.lang IPersistentVector Keyword Atom)))

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

(s/def empty-sgr :- EnumSet
  (EnumSet/noneOf SGR))

(s/def style->sgr :- {st/Style SGR}
  {st/bold          SGR/BOLD
   st/blinking      SGR/BLINK
   st/underline     SGR/UNDERLINE
   st/strikethrough SGR/CROSSED_OUT})

(s/def key->key-type :- {k/Key KeyType}
  {k/escape      KeyType/Escape
   k/backspace   KeyType/Backspace
   k/left        KeyType/ArrowLeft
   k/right       KeyType/ArrowRight
   k/up          KeyType/ArrowUp
   k/down        KeyType/ArrowDown
   k/insert      KeyType/Insert
   k/delete      KeyType/Delete
   k/home        KeyType/Home
   k/end         KeyType/End
   k/page-up     KeyType/PageUp
   k/page-down   KeyType/PageDown
   k/tab         KeyType/Tab
   k/reverse-tab KeyType/ReverseTab
   k/enter       KeyType/Enter
   k/f1          KeyType/F1
   k/f2          KeyType/F2
   k/f3          KeyType/F3
   k/f4          KeyType/F4
   k/f5          KeyType/F5
   k/f6          KeyType/F6
   k/f7          KeyType/F7
   k/f8          KeyType/F8
   k/f9          KeyType/F9
   k/f10         KeyType/F10
   k/f11         KeyType/F11
   k/f12         KeyType/F12
   k/f13         KeyType/F13
   k/f14         KeyType/F14
   k/f15         KeyType/F15
   k/f16         KeyType/F16
   k/f17         KeyType/F17
   k/f18         KeyType/F18
   k/f19         KeyType/F19})

(s/defn to-text-colour :- TextColor
  [[r g b] :- st/RGBColour]
  (TextColor$RGB. ^Integer r ^Integer g ^Integer b))

(s/defn to-key-stroke :- KeyStroke
  [{:keys [key ctrl alt shift]} :- c/KeyBinding]
  (cond (char? key)
        (KeyStroke. ^Character key ^Boolean ctrl ^Boolean alt ^Boolean shift)

        (contains? key->key-type key)
        (KeyStroke. ^KeyType (get key->key-type key) ^Boolean ctrl ^Boolean alt ^Boolean shift)

        :else KeyType/Unknown))

;; maps out all unicode characters; sums up to about 350 kbytes
;; the first 32 unicode character are control characters
;; the supported ones are already mapped by lanterna to special keys
(s/def char->event :- {Character e/Event}
  (->> (range 32 65535)
       (map (juxt char (comp character char)))
       (into {})))

(s/defn key-stroke->event :- {KeyStroke e/Event}
        [keymap :- c/KeyMap]
        (->> keymap
        (map (juxt (comp to-key-stroke val)
                   (comp event key)))
        (into {})))

(s/defn rgb->text-colour :- {st/RGBColour TextColor}
  [syntax :- c/Syntax]
  (->> syntax (vals) (mapcat vals) (set) (map (juxt identity to-text-colour)) (into {})))

;; uses a memoised hashmap of all objects to reduce overhead
(s/defn impl-put! :- nil
  [terminal    :- TerminalScreen
   char        :- Character
   x           :- s/Int
   y           :- s/Int
   foreground  :- st/RGBColour
   background  :- st/RGBColour
   styles      :- [st/Style]
   char-map    :- {Character e/Event}
   style-map   :- {st/Style SGR}
   colour-map  :- {st/RGBColour TextColor}]
  (when (contains? char-map char)
    (let [styles    ^EnumSet (if (> (.count styles) 0)
                               (EnumSet/copyOf ^Collection (mapv style-map styles))
                               empty-sgr)
          text-char ^TextCharacter (TextCharacter.
                                     ^Character char
                                     ^TextColor (colour-map foreground)
                                     ^TextColor (colour-map background)
                                     ^EnumSet styles)]
      (.setCharacter terminal x y text-char))))

;; uses a memoised hashmap of all objects to reduce overhead
(s/defn impl-get-input-event! :- e/Event
  [screen         :- TerminalScreen
   context-events :- {KeyStroke e/Event}
   text-events    :- {Character e/Event}]
  (let [input ^KeyStroke (.readInput screen)]
    (or (get context-events input)
        (get text-events (.getCharacter input))
        ignore)))

(s/defn impl-get-resize-event! :- (s/maybe e/Event)
  [screen   :- TerminalScreen
   resized? :- Atom]
  (when @resized?
    (reset! resized? false)
    (.doResizeIfNecessary screen)
    (e/resize (-> screen (.getTerminalSize) (.getColumns))
              (-> screen (.getTerminalSize) (.getRows)))))

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
  [config :- c/Config]
  (if-let [palette (-> config (:terminal) (st/palette))]
    (TerminalEmulatorColorConfiguration/newInstance palette)
    (TerminalEmulatorColorConfiguration/newInstance TerminalEmulatorPalette/GNOME_TERMINAL)))

(s/defn derive-font :- SwingTerminalFontConfiguration
  [config :- c/Config]
  (let [font-path ^String (-> config (:terminal) (st/font-path))
        font-size ^Float  (-> config (:terminal) (st/font-size) (float))
        font      ^Font   (-> Font/TRUETYPE_FONT
                              (Font/createFont (File. font-path))
                              (.deriveFont Font/BOLD font-size))]
    (SwingTerminalFontConfiguration/newInstance (into-array Font [font]))))

(s/defn create-screen :- TerminalScreen
  [config        :- c/Config
   resizing-sink :- Atom]
  (let [listener (reify TerminalResizeListener (onResized [_ _ _] (reset! resizing-sink true)))]
    (-> (DefaultTerminalFactory.)
        (.setTerminalEmulatorColorConfiguration (derive-palette config))
        (.setTerminalEmulatorFontConfiguration (derive-font config))
        (.setTerminalEmulatorTitle (str "omnia-" (omnia-version)))
        (.createTerminalEmulator)
        (doto (.addResizeListener listener))
        (TerminalScreen.))))

(s/defn terminal :- Terminal
  [config :- c/Config]
  (let [resizing-sink           (atom false)
        screen ^TerminalScreen  (create-screen config resizing-sink)
        memoised-context-events (-> config (:keymap) (key-stroke->event))
        memoised-char-events    char->event
        memoised-styles         style->sgr
        memoised-colours        (-> config (:syntax) (rgb->text-colour))]
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
        (or (impl-get-resize-event! screen resizing-sink)
            (impl-get-input-event! screen memoised-context-events memoised-char-events)))
      (put! [_ ch x y fg bg stls]
        (impl-put! screen ch x y fg bg stls memoised-char-events memoised-styles memoised-colours)))))