(ns omnia.store-test
  (:require [clojure.test :refer :all]
            [omnia.test-utils :refer :all]
            [omnia.repl.store :as h]))

(deftest adds-evaluations-to-store
  (let [history        (h/create-store 2)
        eval           (-> ["some"
                            "eval"]
                           (derive-text)
                           (:lines))
        result         (-> history (h/add-to-eval-history eval) (h/eval-history))
        actual-frame   (h/timeframe result)
        actual-instant (h/instant result)
        actual-limit   (h/limit result)]
    (is (= actual-frame [eval]))
    (is (= actual-instant 0))
    (is (= actual-limit 2))))

(deftest limits-evaluation-history-size
  (let [history        (h/create-store 2)
        eval1          (-> ["one"]
                           (derive-text)
                           (:lines))
        eval2          (-> ["two"]
                           (derive-text)
                           (:lines))
        eval3          (-> ["three"]
                           (derive-text)
                           (:lines))
        result         (-> history
                           (h/add-to-eval-history eval1)
                           (h/add-to-eval-history eval2)
                           (h/add-to-eval-history eval3)
                           (h/eval-history))
        actual-frame   (h/timeframe result)
        actual-instant (h/instant result)
        actual-limit   (h/limit result)]
    (is (= actual-frame [eval2 eval3]))
    (is (= actual-instant 1))
    (is (= actual-limit 2))))

(deftest travels-evaluation-history
  (let [eval1          (-> ["one"]
                           (derive-text)
                           (:lines))
        eval2          (-> ["two"]
                           (derive-text)
                           (:lines))
        history        (-> (h/create-store 2)
                           (h/add-to-eval-history eval1)
                           (h/add-to-eval-history eval2))
        actual-past    (-> history (h/travel-to-previous-instant) (h/evaluation))
        actual-cycle   (-> history (h/travel-to-previous-instant) (h/travel-to-next-instant) (h/evaluation))
        actual-preset  (-> history (h/evaluation))
        actual-frame   (h/timeframe (h/eval-history history))]
    (is (= actual-frame [eval1 eval2]))
    (is (= actual-past eval1))
    (is (= actual-preset actual-cycle eval2))))