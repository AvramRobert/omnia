(ns omnia.store-test
  (:require [clojure.test :refer :all]
            [omnia.test-utils :refer :all]
            [omnia.repl.store :as h]
            [omnia.repl.store :as st]))

(deftest adds-elements-to-eval-history
  (let [history         (h/create-store 2)
        eval            (-> ["some"
                             "eval"]
                            (derive-text))
        result          (-> history (h/add-to-eval-history eval) (h/eval-history))
        actual-frame    (h/timeframe result)
        actual-position (h/position result)
        actual-limit    (h/limit result)
        actual-temp     (h/temp result)]
    (is (= actual-frame [eval]))
    (is (= actual-position 1))
    (is (= actual-limit 2))
    (is (= actual-temp nil))))

(deftest limits-eval-history-elements
  (let [history         (h/create-store 2)
        eval1           (-> ["one"]
                            (derive-text))
        eval2           (-> ["two"]
                            (derive-text))
        eval3           (-> ["three"]
                            (derive-text))
        result          (-> history
                            (h/add-to-eval-history eval1)
                            (h/add-to-eval-history eval2)
                            (h/add-to-eval-history eval3)
                            (h/eval-history))
        actual-frame    (h/timeframe result)
        actual-position (h/position result)
        actual-limit    (h/limit result)]
    (is (= actual-frame [eval2 eval3]))
    (is (= actual-position 2))
    (is (= actual-limit 2))))

(deftest stops-early-when-less-elements-in-eval-history-than-limit
  (let [history         (h/create-store 4)
        eval1           (-> ["one"]
                            (derive-text))
        eval2           (-> ["two"]
                            (derive-text))
        eval3           (-> ["three"]
                            (derive-text))
        result          (-> history
                            (h/add-to-eval-history eval1)
                            (h/add-to-eval-history eval2)
                            (h/add-to-eval-history eval3)
                            (st/travel-to-previous-position)
                            (st/travel-to-next-position)
                            (st/travel-to-next-position)
                            (st/travel-to-next-position)
                            (h/eval-history))
        actual-frame    (h/timeframe result)
        actual-position (h/position result)
        actual-limit    (h/limit result)]
    (is (= actual-frame [eval1 eval2 eval3]))
    (is (= actual-position 3))
    (is (= actual-limit 4))))

(deftest allows-traversing-the-eval-history
  (let [eval1               (-> ["one"]
                                (derive-text))
        eval2               (-> ["two"]
                                (derive-text))
        store               (-> (h/create-store 2)
                                (h/add-to-eval-history eval1)
                                (h/add-to-eval-history eval2))
        actual-near-past    (-> store
                                (h/travel-to-previous-position)
                                (h/evaluation))
        actual-distant-past (-> store
                                (h/travel-to-previous-position)
                                (h/travel-to-previous-position)
                                (h/evaluation))
        actual-cycle        (-> store
                                (h/travel-to-previous-position)
                                (h/travel-to-previous-position)
                                (h/travel-to-next-position)
                                (h/evaluation))
        actual-preset       (-> store (h/evaluation))
        actual-frame        (h/timeframe (h/eval-history store))]
    (is (= actual-frame [eval1 eval2]))
    (is (= actual-distant-past eval1))
    (is (= actual-near-past actual-cycle eval2))
    (is (= actual-preset nil))))

(deftest returns-temporary-value-outside-eval-history-known-bounds
  (let [temp            (-> ["temp"]
                            (derive-text))
        eval            (-> ["one"]
                            (derive-text))
        empty-store     (-> (h/create-store 2)
                            (h/add-temporary temp))
        store           (-> empty-store
                            (h/add-to-eval-history eval))
        actual1-present (st/evaluation empty-store)
        actual2-present (st/evaluation store)]
    (is (= actual1-present actual2-present temp))))

(deftest eval-history-is-reset-appropriately
  (let [temp    (-> ["temp"]
                    (derive-text))
        eval    (-> ["one"]
                    (derive-text))
        store   (-> (h/create-store 2)
                    (h/add-to-eval-history eval))
        actual1 (-> store
                    (st/add-temporary temp)
                    (st/travel-to-previous-position)
                    (st/reset-eval-history))
        actual2 (-> store (st/reset-eval-history))]
    (is (= actual1 actual2 store))))

(deftest adds-undo-history-element
  (let [text1                 (-> ["text1"]
                                  (derive-text))
        text2                 (-> ["text2"]
                                  (derive-text))
        store                 (-> (h/create-store 3)
                                  (h/add-to-undo-history text1)
                                  (h/add-to-undo-history text2))
        actual-undo-value     (-> store (h/next-undo-value))
        actual-undo-timeframe (-> store (h/undo-history) (h/timeframe))
        actual-redo-timeframe (-> store (h/redo-history) (h/timeframe))]
    (is (= actual-undo-value text2))
    (is (= actual-undo-timeframe [text2 text1]))
    (is (= actual-redo-timeframe []))))

(deftest adds-redo-history-element
  (let [text1                 (-> ["text1"]
                                  (derive-text))
        text2                 (-> ["text2"]
                                  (derive-text))
        store                 (-> (h/create-store 3)
                                  (h/add-to-redo-history text1)
                                  (h/add-to-redo-history text2))
        actual-redo-value     (-> store (h/next-redo-value))
        actual-redo-timeframe (-> store (h/redo-history) (h/timeframe))
        actual-undo-timeframe (-> store (h/undo-history) (h/timeframe))]
    (is (= actual-redo-value text2))
    (is (= actual-redo-timeframe [text2 text1]))
    (is (= actual-undo-timeframe []))))

(deftest undos
  (let [text1              (-> ["text1"]
                               (derive-text))
        text2              (-> ["text2"]
                               (derive-text))
        store              (-> (h/create-store 3)
                               (h/add-to-undo-history text1)
                               (h/add-to-undo-history text2)
                               (h/undo))
        actual-value       (-> store (h/next-undo-value))
        expected-value     text1
        actual-timeframe   (-> store (h/undo-history) (h/timeframe))
        expected-timeframe [text1]]
    (is (= actual-value expected-value))
    (is (= actual-timeframe expected-timeframe))))

(deftest redos
  (let [text1              (-> ["text1"]
                               (derive-text))
        text2              (-> ["text2"]
                               (derive-text))
        store              (-> (h/create-store 3)
                               (h/add-to-redo-history text1)
                               (h/add-to-redo-history text2)
                               (h/redo))
        actual-value       (-> store (h/next-redo-value))
        expected-value     text1
        actual-timeframe   (-> store (h/redo-history) (h/timeframe))
        expected-timeframe [text1]]
    (is (= actual-value expected-value))
    (is (= actual-timeframe expected-timeframe))))

(deftest limits-undo-history
  (let [text1              (-> ["text1"]
                               (derive-text))
        text2              (-> ["text2"]
                               (derive-text))
        text3              (-> ["text3"]
                               (derive-text))
        store              (-> (h/create-store 2)
                               (h/add-to-undo-history text1)
                               (h/add-to-undo-history text2)
                               (h/add-to-undo-history text3))
        actual-value       (-> store (h/next-undo-value))
        expected-value     text3
        actual-timeframe   (-> store (h/undo-history) (h/timeframe))
        expected-timeframe [text3 text2]]
    (is (= actual-value expected-value))
    (is (= actual-timeframe expected-timeframe))))

(deftest limits-redo-history
  (let [text1              (-> ["text1"]
                               (derive-text))
        text2              (-> ["text2"]
                               (derive-text))
        text3              (-> ["text3"]
                               (derive-text))
        store              (-> (h/create-store 2)
                               (h/add-to-redo-history text1)
                               (h/add-to-redo-history text2)
                               (h/add-to-redo-history text3))
        actual-value       (-> store (h/next-redo-value))
        expected-value     text3
        actual-timeframe   (-> store (h/redo-history) (h/timeframe))
        expected-timeframe [text3 text2]]
    (is (= actual-value expected-value))
    (is (= actual-timeframe expected-timeframe))))