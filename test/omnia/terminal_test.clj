(ns omnia.terminal-test
  (:require [schema.core :as s]
            [clojure.test.check.generators :as gen]
            [omnia.view.terminal :as t]
            [omnia.config.core :as c]
            [omnia.schema.syntax :as tc]
            [omnia.schema.keymap :as kc]
            [omnia.repl.events :as e]
            [clojure.test :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :refer [for-all]]
            [clojure.set :refer [difference map-invert]]
            [omnia.util.collection :refer [map-vals]]
            [omnia.util.generator :refer [do-gen gen-rgb]]
            [omnia.schema.common :refer [Point]])
  (:import (com.googlecode.lanterna.terminal Terminal)
           (com.googlecode.lanterna.screen TerminalScreen)
           (com.googlecode.lanterna TerminalSize TextCharacter)
           (com.googlecode.lanterna.input KeyStroke)))

(def ^:const NR-OF-TESTS 100)

(def ScreenDef {(s/optional-key :read-input)    [KeyStroke]
                (s/optional-key :terminal-size) Point})

(s/defn terminal-screen-with :- TerminalScreen
  [def :- ScreenDef]
  (let [input (atom (:read-input def []))
        [w h] (:terminal-size def [10 10])]
    (TerminalScreen.
      (reify Terminal
        (addResizeListener [_ _])
        (removeResizeListener [_ _])
        (getTerminalSize [_]
          (TerminalSize. w h))
        (readInput [_]
          (when (not-empty @input)
            (let [item (first @input)
                  _    (swap! input rest)]
              item)))))))

(s/defn char-stroke :- KeyStroke
  [char :- s/Any]
  (t/to-key-stroke {:key char :ctrl false :alt false :shift false}))

(defn gen-key-binding-from [keyset]
  (do-gen [key   (gen/elements keyset)
           ctrl  gen/boolean
           alt   gen/boolean
           shift gen/boolean]
    {:key key :ctrl ctrl :alt alt :shift shift}))

(deftest get-context-event
  (let [context-strokes   (-> c/default-config (:keymap) (t/key-stroke->event))
        expected-events   (set (vals context-strokes))
        screen            (terminal-screen-with {:read-input (keys context-strokes)})]
    (dotimes [_ (count context-strokes)]
      (is (contains? expected-events (t/impl-get-input-event! screen context-strokes {}))))))

(deftest get-text-event
  (let [context-strokes (-> c/default-config (:keymap) (t/key-stroke->event))
        text-strokes    (->> t/char->event (keys) (mapv char-stroke))
        expected-events (set (vals t/char->event))
        screen          (terminal-screen-with {:read-input text-strokes})]
    (dotimes [_ (count text-strokes)]
      (is (contains? expected-events (t/impl-get-input-event! screen context-strokes t/char->event))))))

(defspec get-unknown-event NR-OF-TESTS
         (let [context-strokes (-> c/default-config (:keymap) (t/key-stroke->event))
        text-events     t/char->event]
           (for-all [binding (->> c/default-config
                           (:keymap)
                           (vals)
                           (mapv :key)
                           (set)
                           (difference kc/key-set)
                           (gen-key-binding-from))]
             (let [screen (terminal-screen-with {:read-input [(t/to-key-stroke binding)]})]
               (is (= e/ignore (t/impl-get-input-event! screen context-strokes text-events)))))))
/
(defspec put-char NR-OF-TESTS
  (for-all [char  gen/char-alphanumeric
            fg    gen-rgb
            bg    gen-rgb
            x     (gen/choose 0 9)
            y     (gen/choose 0 9)
            style (gen/set (gen/elements tc/styles))]
     (let [screen            ^TerminalScreen (terminal-screen-with {:terminal-size [10 10]})
           colours           {fg (t/to-text-colour fg)
                              bg (t/to-text-colour bg)}
           styles            t/style->sgr
           chars             t/char->event
           expected-colours  (map-invert colours)
           expected-styles   (map-invert styles)
           _                 (t/impl-put! screen char x y fg bg style chars styles colours)
           actual-char        ^TextCharacter (.getBackCharacter screen x y)
           actual-style      (->> actual-char (.getModifiers) (mapv expected-styles) (set))
           actual-background (->> actual-char (.getBackgroundColor) (expected-colours))
           actual-foreground (->> actual-char (.getForegroundColor) (expected-colours))]
       (is (= char (.getCharacter actual-char)))
       (is (= style actual-style))
       (is (= bg actual-background))
       (is (= fg actual-foreground)))))

(deftest ignore-control-char
  (let [screen        ^TerminalScreen (terminal-screen-with {:terminal-size [10 10]})
        [x y]         [0 0]
        fg            [255 255 255]
        bg            [0 0 0]
        colours       {fg (t/to-text-colour fg)
                       bg (t/to-text-colour bg)}
        styles        t/style->sgr
        chars         t/char->event
        print         (fn [char]
                        (t/impl-put! screen char x y fg bg [] chars styles colours))
        control-chars (->> 32 (range 0) (mapv char))]
    (doseq [char control-chars]
      (print char)
      (is (= TextCharacter/DEFAULT_CHARACTER (.getBackCharacter screen x y))))))