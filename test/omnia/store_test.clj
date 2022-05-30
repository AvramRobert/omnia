(ns omnia.store-test
  (:require [clojure.test :refer :all]
            [omnia.test-utils :refer :all]
            [omnia.repl.store :as h]
            [omnia.repl.store :as st]))

(deftest eval-history-adds-evaluations-to-store
  (let [history        (h/create-store 2)
        eval           (-> ["some"
                            "eval"]
                           (derive-text))
        result         (-> history (h/add-to-eval-history eval) (h/eval-history))
        actual-frame   (h/timeframe result)
        actual-instant (h/instant result)
        actual-limit   (h/limit result)
        actual-temp    (h/temp result)]
    (is (= actual-frame [eval]))
    (is (= actual-instant 1))
    (is (= actual-limit 2))
    (is (= actual-temp nil))))

(deftest eval-history-limits-evaluation-history-size
  (let [history        (h/create-store 2)
        eval1          (-> ["one"]
                           (derive-text))
        eval2          (-> ["two"]
                           (derive-text))
        eval3          (-> ["three"]
                           (derive-text))
        result         (-> history
                           (h/add-to-eval-history eval1)
                           (h/add-to-eval-history eval2)
                           (h/add-to-eval-history eval3)
                           (h/eval-history))
        actual-frame   (h/timeframe result)
        actual-instant (h/instant result)
        actual-limit   (h/limit result)]
    (is (= actual-frame [eval2 eval3]))
    (is (= actual-instant 2))
    (is (= actual-limit 2))))

(deftest eval-history-stops-early-when-less-frames-than-limit
  (let [history        (h/create-store 4)
        eval1          (-> ["one"]
                           (derive-text))
        eval2          (-> ["two"]
                           (derive-text))
        eval3          (-> ["three"]
                           (derive-text))
        result         (-> history
                           (h/add-to-eval-history eval1)
                           (h/add-to-eval-history eval2)
                           (h/add-to-eval-history eval3)
                           (st/travel-to-previous-instant)
                           (st/travel-to-next-instant)
                           (st/travel-to-next-instant)
                           (st/travel-to-next-instant)
                           (h/eval-history))
        actual-frame   (h/timeframe result)
        actual-instant (h/instant result)
        actual-limit   (h/limit result)]
    (is (= actual-frame [eval1 eval2 eval3]))
    (is (= actual-instant 3))
    (is (= actual-limit 4))))

(deftest eval-history-allows-traveling
  (let [eval1               (-> ["one"]
                                (derive-text))
        eval2               (-> ["two"]
                                (derive-text))
        store               (-> (h/create-store 2)
                                (h/add-to-eval-history eval1)
                                (h/add-to-eval-history eval2))
        actual-near-past    (-> store
                                (h/travel-to-previous-instant)
                                (h/evaluation))
        actual-distant-past (-> store
                                (h/travel-to-previous-instant)
                                (h/travel-to-previous-instant)
                                (h/evaluation))
        actual-cycle        (-> store
                                (h/travel-to-previous-instant)
                                (h/travel-to-previous-instant)
                                (h/travel-to-next-instant)
                                (h/evaluation))
        actual-preset       (-> store (h/evaluation))
        actual-frame        (h/timeframe (h/eval-history store))]
    (is (= actual-frame [eval1 eval2]))
    (is (= actual-distant-past eval1))
    (is (= actual-near-past actual-cycle eval2))
    (is (= actual-preset nil))))

(deftest eval-history-returns-temporary-outside-known-bounds
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
                    (st/travel-to-previous-instant)
                    (st/reset-eval-history))
        actual2 (-> store (st/reset-eval-history))]
    (is (= actual1 actual2 store))))