(ns omnia.terminal-test
  (:require [clojure.test :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :refer [for-all]]
            [clojure.set :refer [difference map-invert]]
            [omnia.util.collection :refer [map-vals]]
            [omnia.util.generator :refer [do-gen]]
            [clojure.test.check.generators :as gen]
            [omnia.view.terminal :as t]
            [omnia.config.core :as c]
            [omnia.config.components.input :as ic]
            [omnia.config.components.event :as e])
  (:import (com.googlecode.lanterna.terminal Terminal)
           (com.googlecode.lanterna.screen TerminalScreen)
           (com.googlecode.lanterna TerminalSize)))

(def ^:const NR-OF-TESTS 100)

(defn to-key-binding [char]
  {:key char :ctrl false :alt false :shift false})

(defn feed-input [keystrokes]
  (let [current-stroke (atom 0)]
    (fn []
      (when (< @current-stroke (count keystrokes))
        (let [item (nth keystrokes @current-stroke)
              _    (swap! current-stroke inc)]
          item)))))

(defn terminal-screen-with [fns]
  (let [read-input (:read-input fns (constantly nil))
        terminal-size (:terminal-size fns (constantly (TerminalSize. 10 10)))]
    (TerminalScreen.
      (reify Terminal
        (addResizeListener [_ _])
        (removeResizeListener [_ _])
        (getTerminalSize [_] (terminal-size))
        (readInput [_] (read-input))))))

(defn gen-key-binding-from [keyset]
  (do-gen [key   (gen/elements keyset)
           ctrl  gen/boolean
           alt   gen/boolean
           shift gen/boolean]
    {:key key :ctrl ctrl :alt alt :shift shift}))

(deftest get-context-event-test
  (let [context-strokes   (-> c/default-config (:keymap) (t/context-events))
        expected-events   (set (vals context-strokes))
        screen            (terminal-screen-with {:read-input (feed-input (keys context-strokes))})]
    (dotimes [_ (count context-strokes)]
      (is (contains? expected-events (t/impl-get-event! screen context-strokes {}))))))

(deftest get-text-event-test
  (let [text-strokes    (->> t/text-events (keys) (mapv to-key-binding) (mapv t/to-key-stroke))
        expected-events (set (vals t/text-events))
        screen          (terminal-screen-with {:read-input (feed-input text-strokes)})]
    (dotimes [_ (count text-strokes)]
      (is (contains? expected-events (t/impl-get-event! screen {} t/text-events))))))

(defspec get-unknown-event-test NR-OF-TESTS
  (for-all [binding (->> c/default-config
                         (:keymap)
                         (vals)
                         (mapv :key)
                         (set)
                         (difference ic/key-set)
                         (gen-key-binding-from))]
           (let [screen (terminal-screen-with {:read-input (feed-input [(t/to-key-stroke binding)])})]
             (is (= e/ignore-event (t/impl-get-event! screen {} {}))))))