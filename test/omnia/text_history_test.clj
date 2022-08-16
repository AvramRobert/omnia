(ns omnia.text-history-test
  (:require [clojure.test :refer :all]
            [omnia.test-utils :refer :all]
            [omnia.repl.text-history :as th]))

(deftest inserts
  (let [text1            (-> ["text1"]
                             (derive-text))
        text2            (-> ["text2"]
                             (derive-text))
        history          (->> (th/create-text-history 2)
                              (th/insert text1)
                              (th/insert text2))
        actual-value     (th/next-record history)
        expected-value   text2
        actual-records   (th/records history)
        expected-records [text2 text1]]
    (is (= actual-value expected-value))
    (is (= actual-records expected-records))))

(deftest limits-history
  (let [text1            (-> ["text1"]
                             (derive-text))
        text2            (-> ["text2"]
                             (derive-text))
        text3            (-> ["text3"]
                             (derive-text))
        history          (->> (th/create-text-history 2)
                              (th/insert text1)
                              (th/insert text2)
                              (th/insert text3))
        actual-value     (th/next-record history)
        expected-value   text3
        actual-records   (th/records history)
        expected-records [text3 text2]]
    (is (= actual-value expected-value))
    (is (= actual-records expected-records))))

(deftest reverts
  (let [text1            (-> ["text1"]
                             (derive-text))
        text2            (-> ["text2"]
                             (derive-text))
        history          (->> (th/create-text-history 3)
                              (th/insert text1)
                              (th/insert text2)
                              (th/revert))
        actual-value     (th/next-record history)
        expected-value   text1
        actual-records   (th/records history)
        expected-records [text1]
        actual-size      (th/size history)
        expected-size    1]
    (is (= actual-value expected-value))
    (is (= actual-records expected-records))
    (is (= actual-size expected-size))))

(deftest returns-past-records
  (let [text1                 (-> ["text1"]
                                  (derive-text))
        text2                 (-> ["text2"]
                                  (derive-text))
        history               (->> (th/create-text-history 2)
                                   (th/insert text1)
                                   (th/insert text2))
        actual-first-value    (th/next-record history)
        expected-first-value  text2
        actual-second-value   (-> history (th/revert) (th/next-record))
        expected-second-value text1]
    (is (= actual-first-value expected-first-value))
    (is (= actual-second-value expected-second-value))))