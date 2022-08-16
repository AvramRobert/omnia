(ns omnia.eval-history-test
  (:require [clojure.test :refer :all]
            [omnia.test-utils :refer :all]
            [omnia.repl.eval-history :as eh]))

(deftest inserts-evaluation
  (let [history         (eh/create-eval-history 2)
        eval            (-> ["some"
                             "eval"]
                            (derive-text))
        result          (eh/insert eval history)
        actual-frame    (eh/evaluations result)
        actual-position (eh/position result)
        actual-limit    (eh/limit result)
        actual-temp     (eh/temp result)]
    (is (= actual-frame [eval]))
    (is (= actual-position 1))
    (is (= actual-limit 2))
    (is (= actual-temp nil))))

(deftest limits-evaluations
  (let [history         (eh/create-eval-history 2)
        eval1           (-> ["one"]
                            (derive-text))
        eval2           (-> ["two"]
                            (derive-text))
        eval3           (-> ["three"]
                            (derive-text))
        result          (->> history
                             (eh/insert eval1)
                             (eh/insert eval2)
                             (eh/insert eval3))
        actual-frame    (eh/evaluations result)
        actual-position (eh/position result)
        actual-limit    (eh/limit result)]
    (is (= actual-frame [eval2 eval3]))
    (is (= actual-position 2))
    (is (= actual-limit 2))))

(deftest stops-early-when-fewer-elements-than-limit
  (let [history         (eh/create-eval-history 4)
        eval1           (-> ["one"]
                            (derive-text))
        eval2           (-> ["two"]
                            (derive-text))
        eval3           (-> ["three"]
                            (derive-text))
        result          (->> history
                             (eh/insert eval1)
                             (eh/insert eval2)
                             (eh/insert eval3)
                             (eh/travel-back)
                             (eh/travel-forward)
                             (eh/travel-forward)
                             (eh/travel-forward))
        actual-frame    (eh/evaluations result)
        actual-position (eh/position result)
        actual-limit    (eh/limit result)]
    (is (= actual-frame [eval1 eval2 eval3]))
    (is (= actual-position 3))
    (is (= actual-limit 4))))

(deftest allows-traversing-evaluations
  (let [eval1               (-> ["one"]
                                (derive-text))
        eval2               (-> ["two"]
                                (derive-text))
        history             (->> (eh/create-eval-history 2)
                                 (eh/insert eval1)
                                 (eh/insert eval2))
        actual-near-past    (-> history
                                (eh/travel-back)
                                (eh/current-eval))
        actual-distant-past (-> history
                                (eh/travel-back)
                                (eh/travel-back)
                                (eh/current-eval))
        actual-cycle        (-> history
                                (eh/travel-back)
                                (eh/travel-back)
                                (eh/travel-forward)
                                (eh/current-eval))
        actual-preset       (eh/current-eval history)
        actual-frame        (eh/evaluations history)]
    (is (= actual-frame [eval1 eval2]))
    (is (= actual-distant-past eval1))
    (is (= actual-near-past actual-cycle eval2))
    (is (= actual-preset nil))))

(deftest returns-temporary-value-outside-known-bounds
  (let [temp            (-> ["temp"]
                            (derive-text))
        eval            (-> ["one"]
                            (derive-text))
        empty-history   (->> (eh/create-eval-history 2)
                             (eh/keep-temp temp))
        store           (eh/insert eval empty-history)
        actual1-present (eh/current-eval empty-history)
        actual2-present (eh/current-eval store)]
    (is (= actual1-present actual2-present temp))))

(deftest resets-evaluations
  (let [temp    (-> ["temp"]
                    (derive-text))
        eval    (-> ["one"]
                    (derive-text))
        history (->> (eh/create-eval-history 2)
                     (eh/insert eval))
        actual1 (->> history
                     (eh/keep-temp temp)
                     (eh/travel-back)
                     (eh/reset))
        actual2 (eh/reset history)]
    (is (= actual1 actual2 history))))