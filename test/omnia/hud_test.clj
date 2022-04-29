(ns omnia.hud-test
  (:require [clojure.test :refer :all]
            [omnia.test-utils :refer :all]
            [omnia.repl.text :as i]
            [omnia.repl.hud :as h]))

(deftest supports-pop-up-windows
  (let [hud               (-> ["1"] (derive-hud))
        window            (-> ["a" "b" "c"]
                              (i/from-tagged-strings)
                              (h/riffle-window 2))
        expected1         (-> ["1"
                               "------"
                               " a|"
                               " b"
                               " ..."
                               "------"]
                              (derive-hud))
        expected2         (-> ["1"
                               "------"
                               " a"
                               " b|"
                               " ..."
                               "------"]
                              (derive-hud))
        expected3         (-> ["1"
                               "------"
                               " b"
                               " c|"
                               "------"]
                              (derive-hud))
        processed1        (h/pop-up hud window)
        processed2        (h/pop-up hud (h/riffle window))
        processed3        (h/pop-up hud (h/riffle (h/riffle window)))
        processed4        (h/pop-up hud (h/riffle (h/riffle (h/riffle window))))

        actual-view1      (-> processed1 (h/text) (:lines))
        actual-cursor1    (-> processed1 (h/text) (:cursor))

        expected-preview1 (-> expected1 (h/text) (:lines))
        expected-cursor1  (-> expected1 (h/text) (:cursor))

        actual-view2      (-> processed2 (h/text) (:lines))
        actual-cursor2    (-> processed2 (h/text) (:cursor))

        expected-preview2 (-> expected2 (h/text) (:lines))
        expected-cursor2  (-> expected2 (h/text) (:cursor))

        actual-view3      (-> processed3 (h/text) (:lines))
        actual-cursor3    (-> processed3 (h/text) (:cursor))

        expected-preview3 (-> expected3 (h/text) (:lines))
        expected-cursor3  (-> expected3 (h/text) (:cursor))

        actual-view4      (-> processed4 (h/text) (:lines))
        actual-cursor4    (-> processed4 (h/text) (:cursor))]
    (is (= actual-view1 expected-preview1))
    (is (= actual-cursor1 expected-cursor1))

    (is (= actual-view2 expected-preview2))
    (is (= actual-cursor2 expected-cursor2))

    (is (= actual-view3 expected-preview3))
    (is (= actual-cursor3 expected-cursor3))

    (is (= actual-view4 expected-preview1))
    (is (= actual-cursor4 expected-cursor1))))


(deftest supports-calibration-in-pop-up-windows
  (let [window              (-> ["a" "b" "c"]
                                (i/from-tagged-strings)
                                (h/riffle-window 2))
        hud1                (-> [-x-
                                 "current"
                                 "text|"
                                 "input"
                                 -x-
                                 "not viewable"
                                 "not viewable"]
                                (derive-hud)
                                (h/pop-up window))
        hud2                (-> [-x-
                                 "current|"
                                 "text"
                                 "that"
                                 "is"
                                 "large"
                                 "enough"
                                 -x-
                                 "not viewable"
                                 "not viewable"]
                                (derive-hud)
                                (h/pop-up window))
        hud3                (-> ["current"
                                 -x-
                                 "text"
                                 "that|"
                                 "is"
                                 -x-]
                                (derive-hud)
                                (h/pop-up window))
        expected1           (-> ["------"
                                 " a|"
                                 " b"]
                                (derive-hud))
        expected2           (-> ["current|"
                                 "------"
                                 " a|"
                                 " b"
                                 " ..."
                                 "------"]
                                (derive-hud))
        expected3           (-> [" b|"
                                 " ..."
                                 "------"]
                                (derive-hud))
        actual-view-offset1 (h/view-offset hud1)
        actual-view1        (-> hud1 (h/project-hud) (:lines))
        actual-cursor1      (-> hud1 (h/project-hud) (:cursor))

        actual-view-offset2 (h/view-offset hud2)
        actual-view2        (-> hud2 (h/project-hud) (:lines))
        actual-cursor2      (-> hud2 (h/project-hud) (:cursor))

        actual-view-offset3 (h/view-offset hud3)
        actual-view3        (-> hud3 (h/project-hud) (:lines))
        actual-cursor3      (-> hud3 (h/project-hud) (:cursor))

        expected-view1      (-> expected1 (h/text) (:lines))
        expected-cursor1    (-> expected1 (h/text) (:cursor))

        expected-view2      (-> expected2 (h/text) (:lines))
        expected-cursor2    (-> expected2 (h/text) (:cursor))

        expected-view3      (-> expected3 (h/text) (:lines))
        expected-cursor3    (-> expected3 (h/text) (:cursor))]
    (is (= actual-view1 expected-view1))
    (is (= actual-view-offset1 2))
    (is (= actual-cursor1 expected-cursor1))

    (is (= actual-view2 expected-view2))
    (is (= actual-view-offset2 2))
    (is (= actual-cursor2 expected-cursor2))

    (is (= actual-view3 expected-view3))
    (is (= actual-view-offset3 0))
    (is (= actual-cursor3 expected-cursor3))))

(deftest supports-empty-pop-windows
  (let [context         (-> ["input|"]
                            (derive-hud)
                            (h/pop-up h/empty-hud))
        expected        (-> ["input"
                             "------"
                             "|------"]
                            (derive-hud))
        actual-view     (-> context (h/text) (:lines))
        actual-cursor   (-> context (h/text) (:cursor))
        expected-view   (-> expected (h/text) (:lines))
        expected-cursor (-> expected (h/text) (:cursor))]
    (is (= actual-view expected-view))
    (is (= expected-cursor actual-cursor))))