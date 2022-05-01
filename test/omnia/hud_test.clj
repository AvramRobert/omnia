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


(deftest supports-correction-in-pop-up-windows
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

(deftest corrects-exceeding-upper-view
  (let [then             (-> ["persisted"
                              ---
                              "some"
                              "input"
                              -x-
                              "of|"
                              "mine"
                              "that"
                              "is"
                              "this"
                              "long"
                              -x-]
                             (derive-hud))
        now1             (-> ["persisted"
                              ---
                              "some"
                              "input|"
                              -x-
                              "of"
                              "mine"
                              "that"
                              "is"
                              "this"
                              "long"
                              -x-]
                             (derive-hud))
        now2             (-> ["persisted"
                              ---
                              "some|"
                              "input"
                              -x-
                              "of"
                              "mine"
                              "that"
                              "is"
                              "this"
                              "long"
                              -x-]
                             (derive-hud))
        expected1        (-> ["input|"
                              "of"
                              "mine"
                              "that"
                              "is"
                              "this"]
                             (i/from-tagged-strings))
        expected2        (-> ["some|"
                              "input"
                              "of"
                              "mine"
                              "that"
                              "is"]
                             (i/from-tagged-strings))
        actual-voff1     (h/correct-between now1 then)
        actual-view1     (-> now1 (h/with-view-offset actual-voff1) (h/project-hud) (:lines))
        actual-cursor1   (-> now1 (h/with-view-offset actual-voff1) (h/project-hud) (:cursor))

        actual-voff2     (h/correct-between now2 then)
        actual-view2     (-> now2 (h/with-view-offset actual-voff2) (h/project-hud) (:lines))
        actual-cursor2   (-> now2 (h/with-view-offset actual-voff2) (h/project-hud) (:cursor))

        expected-voff1   1
        expected-view1   (:lines expected1)
        expected-cursor1 (:cursor expected1)

        expected-voff2   2
        expected-view2   (:lines expected2)
        expected-cursor2 (:cursor expected2)]
    (is (= actual-voff1 expected-voff1))
    (is (= actual-view1 expected-view1))
    (is (= actual-cursor1 expected-cursor1))

    (is (= actual-voff2 expected-voff2))
    (is (= actual-view2 expected-view2))
    (is (= actual-cursor2 expected-cursor2))))

(deftest corrects-exceeding-lower-view
  (let [then             (-> [-x-
                              "some"
                              "input"
                              "of"
                              "mine"
                              "that"
                              "is|"
                              -x-
                              "this"
                              "long"]
                             (derive-hud))
        now1             (-> [-x-
                              "some"
                              "input"
                              "of"
                              "mine"
                              "that"
                              "is"
                              -x-
                              "th|is"
                              "long"]
                             (derive-hud))
        now2             (-> [-x-
                              "some"
                              "input"
                              "of"
                              "mine"
                              "that"
                              "is"
                              -x-
                              "this"
                              "lo|ng"]
                             (derive-hud))
        expected1        (-> ["input"
                              "of"
                              "mine"
                              "that"
                              "is"
                              "th|is"]
                             (i/from-tagged-strings))
        expected2        (-> ["of"
                              "mine"
                              "that"
                              "is"
                              "this"
                              "lo|ng"]
                             (i/from-tagged-strings))
        actual-voff1     (h/correct-between now1 then)
        actual-view1     (-> now1 (h/with-view-offset actual-voff1) (h/project-hud) (:lines))
        actual-cursor1   (-> now1 (h/with-view-offset actual-voff1) (h/project-hud) (:cursor))

        actual-voff2     (h/correct-between now2 then)
        actual-view2     (-> now2 (h/with-view-offset actual-voff2) (h/project-hud) (:lines))
        actual-cursor2   (-> now2 (h/with-view-offset actual-voff2) (h/project-hud) (:cursor))

        expected-voff1   1
        expected-view1   (:lines expected1)
        expected-cursor1 (:cursor expected1)

        expected-voff2   0
        expected-view2   (:lines expected2)
        expected-cursor2 (:cursor expected2)]
    (is (= actual-voff1 expected-voff1))
    (is (= actual-view1 expected-view1))
    (is (= actual-cursor1 expected-cursor1))

    (is (= actual-voff2 expected-voff2))
    (is (= actual-view2 expected-view2))
    (is (= actual-cursor2 expected-cursor2))))

(deftest does-not-correct-when-view-not-exceeded
  (let [then            (-> [-x-
                             "some"
                             "input"
                             "of"
                             "mine"
                             "that"
                             "is|"
                             -x-
                             "this"
                             "long"]
                            (derive-hud))
        now             (-> [-x-
                             "some"
                             "input"
                             "o|f"
                             "mine"
                             "that"
                             "is"
                             -x-
                             "this"
                             "long"]
                            (derive-hud))
        expected        (-> ["some"
                             "input"
                             "o|f"
                             "mine"
                             "that"
                             "is"]
                            (i/from-tagged-strings))

        actual-voff     (h/correct-between now then)
        actual-view     (-> now (h/with-view-offset actual-voff) (h/project-hud) (:lines))
        actual-cursor   (-> now (h/with-view-offset actual-voff) (h/project-hud) (:cursor))

        expected-voff   2
        expected-view   (:lines expected)
        expected-cursor (:cursor expected)]
    (is (= actual-voff expected-voff))
    (is (= actual-view expected-view))
    (is (= actual-cursor expected-cursor))))

(deftest corrects-when-text-shrinks
  (let [then             (-> [-x-
                              "some"
                              "input"
                              "of"
                              "mine"
                              "that"
                              "is|"
                              -x-
                              "this"
                              "long"]
                             (derive-hud))
        now1             (-> [-x-
                              "some"
                              "input"
                              "of"
                              "mine"
                              "that|"
                              ...
                              -x-
                              "this"
                              "long"]
                             (derive-hud))
        now2             (-> [-x-
                              ...
                              "input"
                              "of"
                              ...
                              "that"
                              "is|"
                              -x-
                              "this"
                              "long"]
                             (derive-hud))
        expected1        (-> ["some"
                              "input"
                              "of"
                              "mine"
                              "that|"
                              "this"]
                             (i/from-tagged-strings))
        expected2        (-> ["input"
                              "of"
                              "that"
                              "is|"
                              "this"
                              "long"]
                             (i/from-tagged-strings))
        actual-voff1     (h/correct-between now1 then)
        actual-view1     (-> now1 (h/with-view-offset actual-voff1) (h/project-hud) (:lines))
        actual-cursor1   (-> now1 (h/with-view-offset actual-voff1) (h/project-hud) (:cursor))

        actual-voff2     (h/correct-between now2 then)
        actual-view2     (-> now2 (h/with-view-offset actual-voff2) (h/project-hud) (:lines))
        actual-cursor2   (-> now2 (h/with-view-offset actual-voff2) (h/project-hud) (:cursor))

        expected-voff1   1
        expected-view1   (:lines expected1)
        expected-cursor1 (:cursor expected1)

        expected-voff2   0
        expected-view2   (:lines expected2)
        expected-cursor2 (:cursor expected2)]
    (is (= actual-voff1 expected-voff1))
    (is (= actual-view1 expected-view1))
    (is (= actual-cursor1 expected-cursor1))

    (is (= actual-voff2 expected-voff2))
    (is (= actual-view2 expected-view2))
    (is (= actual-cursor2 expected-cursor2))))