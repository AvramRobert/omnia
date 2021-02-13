(ns omnia.terminal-test
  (:require [clojure.test :refer :all]
            [omnia.view.terminal :as t]
            [omnia.util.collection :as c]
            [omnia.config.components.input :as ic]
            [omnia.config.components.event :as ec]
            [omnia.config.components.event :as e])
  (:import (com.googlecode.lanterna.input InputProvider)
           (javax.swing KeyStroke)
           (com.googlecode.lanterna.terminal Terminal)
           (com.googlecode.lanterna.screen TerminalScreen)
           (com.googlecode.lanterna TerminalSize)))

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

(defn to-key-binding [type]
  {:key type :ctrl false :alt false :shift false})

(defn unfold [f b]
  (when-let [[a b'] (f b)]
    (cons a (lazy-seq (unfold f b')))))

(deftest get-event!-test
  (let [strokes        (mapv (comp t/to-key-stroke to-key-binding) ic/key-set)
        expected-event (ec/event e/ignore)
        context-events (->> strokes (map (juxt identity (constantly expected-event))) (into {}))
        text-events    {}
        events (->> (terminal-screen-with {:read-input (feed-input strokes)})
                    (unfold (fn [screen]
                              [(t/impl-get-event! screen context-events text-events) screen]))
                    (take (count strokes)))]
    (is (= (set events) #{expected-event}))))
