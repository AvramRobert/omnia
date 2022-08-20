(ns omnia.view-test
  (:require [clojure.test :refer :all]
            [omnia.test-utils :refer :all]
            [omnia.repl.view :as h]))

; extract (clip-sel (select-all view)) == project view
(deftest total-selection-law
  (is true))

; project-sel (select view) = map project-cursor (select view)
(deftest distributive-projection-law
  (is true))

; extract (select view) `contains` extract (project view) (project-selection (select view))
(deftest containment-projection-law
  (is true))

(deftest projects-y-coordinate
  (let [view        (-> ["beginning"
                        -| "some"
                        -| "t|ext"]
                       (derive-view))
        actual-y   (h/project-y view (-> view (h/text) (:cursor) (second)))
        expected-y 1]
    (is (= expected-y actual-y))))

(deftest projects-offset-y-coordinate
  (let [view        (-> [-| "beginning"
                        -| "so|me"
                        -+ "text"]
                       (derive-view))
        actual-y   (h/project-y view (-> view (h/text) (:cursor) (second)))
        expected-y 1]
    (is (= expected-y actual-y))))

(deftest projects-scrolled-y-coordinate
  (let [view        (-> [ "beginning"
                        -|
                        -| "so|me"
                        -$ "text"]
                       (derive-view))
        actual-y   (h/project-y view (-> view (h/text) (:cursor) (second)))
        expected-y 0]
    (is (= expected-y actual-y))))

(deftest projects-scrolled-and-offset-y-coordinate
  (let [view        (-> ["beginning"
                        -|
                        -+ "so|me"
                        -+ "text"
                        -$ "entry"]
                       (derive-view))
        actual-y   (h/project-y view (-> view (h/text) (:cursor) (second)))
        expected-y 0]
    (is (= expected-y actual-y))))

(deftest projects-entire-view
  (let [view             (-> [-| "beginning"
                             -| "some"
                             -| "text|"]
                            (derive-view))
        expected        (-> ["beginning"
                             "some"
                             "text|"]
                            (derive-text))
        actual-view     (-> view (h/project) (:lines))
        actual-cursor   (-> view (h/project) (:cursor))
        expected-view   (:lines expected)
        expected-cursor (:cursor expected)]
    (is (= actual-view expected-view))
    (is (= actual-cursor expected-cursor))))

(deftest projects-field-of-view
  (let [view             (-> ["beginning"
                             -| "some"
                             -| "t|ext"]
                            (derive-view))
        expected        (-> ["some"
                             "t|ext"]
                            (derive-text))
        actual-view     (-> view (h/project) (:lines))
        actual-cursor   (-> view (h/project) (:cursor))
        expected-view   (:lines expected)
        expected-cursor (:cursor expected)]
    (is (= actual-view expected-view))
    (is (= actual-cursor expected-cursor))))

(deftest projects-offset-field-of-view
  (let [view            (-> ["beginning"
                             -| "so|me"
                             -| "text"
                             -+ "somewhere"]
                            (derive-view))
        expected        (-> ["so|me"
                             "text"]
                            (derive-text))
        actual-view     (-> view (h/project) (:lines))
        actual-cursor   (-> view (h/project) (:cursor))
        expected-view   (:lines expected)
        expected-cursor (:cursor expected)]
    (is (= actual-view expected-view))
    (is (= actual-cursor expected-cursor))))

(deftest projects-scrolled-field-of-view
  (let [view            (-> ["beginning"
                             -| "some"
                             -| "|text"
                             -$ "somewhere"]
                            (derive-view))
        expected        (-> ["|some"
                             "text"]
                            (derive-text))
        actual-view     (-> view (h/project) (:lines))
        actual-cursor   (-> view (h/project) (:cursor))
        expected-view   (:lines expected)
        expected-cursor (:cursor expected)]
    (is (= actual-view expected-view))
    (is (= actual-cursor expected-cursor))))

(deftest projects-scrolled-and-offset-field-of-view
  (let [view            (-> ["beginning"
                             -| "some|"
                             -|
                             -+ "text"
                             -$ "somewhere"]
                            (derive-view))
        expected        (-> ["begi|nning"
                             "some"]
                            (derive-text))
        actual-view     (-> view (h/project) (:lines))
        actual-cursor   (-> view (h/project) (:cursor))
        expected-view   (:lines expected)
        expected-cursor (:cursor expected)]
    (is (= actual-view expected-view))
    (is (= actual-cursor expected-cursor))))

(deftest projects-total-selections
  (let [view          (-> ["some"
                          -| "⦇piece of"
                          -| "text⦈"]
                         (derive-view))
        expected     (-> ["⦇piece of"
                          "text⦈"]
                         (derive-text))
        actual-sel   (h/project-selection view (-> view (h/text) (:selection)))
        expected-sel (:selection expected)]
    (is (= actual-sel expected-sel))))

(deftest projects-offset-total-selections
  (let [view         (-> [-| "⦇some"
                          -| "piece of⦈"
                          -+ "text"]
                         (derive-view))
        expected     (-> ["⦇some"
                          "piece of⦈"]
                         (derive-text))
        actual-sel   (h/project-selection view (-> view (h/text) (:selection)))
        expected-sel (:selection expected)]
    (is (= actual-sel expected-sel))))

(deftest projects-offset-partial-selections
  (let [view         (-> [-| "⦇some"
                          -| "piece of"
                          -+ "text⦈"]
                         (derive-view))
        expected     (-> ["⦇some"
                          "piece of⦈"]
                         (derive-text))
        actual-sel   (h/project-selection view (-> view (h/text) (:selection)))
        expected-sel (:selection expected)]
    (is (= actual-sel expected-sel))))

(deftest supports-pop-up-windows
  (let [view              (-> ["1"] (derive-view))
        window            (-> ["a" "b" "c"]
                              (derive-text)
                              (h/riffle-window 2))
        expected1         (-> ["1"
                               "------"
                               " a|"
                               " b"
                               " ..."
                               "------"]
                              (derive-view))
        expected2         (-> ["1"
                               "------"
                               " a"
                               " b|"
                               " ..."
                               "------"]
                              (derive-view))
        expected3         (-> ["1"
                               "------"
                               " b"
                               " c|"
                               "------"]
                              (derive-view))
        processed1        (h/pop-up view window)
        processed2        (h/pop-up view (h/riffle window))
        processed3        (h/pop-up view (h/riffle (h/riffle window)))
        processed4        (h/pop-up view (h/riffle (h/riffle (h/riffle window))))

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
                                (derive-text)
                                (h/riffle-window 2))
        view1               (-> [-| "current"
                                 -| "text|"
                                 -| "input"
                                 -+ "not viewable"
                                 -+ "not viewable"]
                                (derive-view)
                                (h/pop-up window))
        view2               (-> [-| "current|"
                                 -| "text"
                                 -| "that"
                                 -| "is"
                                 -| "large"
                                 -| "enough"
                                 -+ "not viewable"
                                 -+ "not viewable"]
                                (derive-view)
                                (h/pop-up window))
        view3               (-> ["current"
                                 -| "text"
                                 -| "that|"
                                 -| "is"]
                                (derive-view)
                                (h/pop-up window))
        expected1           (-> ["------"
                                 " a|"
                                 " b"]
                                (derive-view))
        expected2           (-> ["current|"
                                 "------"
                                 " a|"
                                 " b"
                                 " ..."
                                 "------"]
                                (derive-view))
        expected3           (-> [" b|"
                                 " ..."
                                 "------"]
                                (derive-view))
        actual-view-offset1 (h/view-offset view1)
        actual-view1        (-> view1 (h/project) (:lines))
        actual-cursor1      (-> view1 (h/project) (:cursor))

        actual-view-offset2 (h/view-offset view2)
        actual-view2        (-> view2 (h/project) (:lines))
        actual-cursor2      (-> view2 (h/project) (:cursor))

        actual-view-offset3 (h/view-offset view3)
        actual-view3        (-> view3 (h/project) (:lines))
        actual-cursor3      (-> view3 (h/project) (:cursor))

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
  (let [view            (-> ["input|"]
                            (derive-view)
                            (h/pop-up h/empty-view))
        expected        (-> ["input"
                             "------|"
                             "------"]
                            (derive-view))
        actual-view     (-> view (h/text) (:lines))
        actual-cursor   (-> view (h/text) (:cursor))
        expected-view   (-> expected (h/text) (:lines))
        expected-cursor (-> expected (h/text) (:cursor))]
    (is (= actual-view expected-view))
    (is (= expected-cursor actual-cursor))))

(deftest corrects-exceeding-upper-view
  (let [then             (-> ["some"
                              "input"
                              -| "of|"
                              -| "mine"
                              -| "that"
                              -| "is"
                              -| "this"
                              -| "long"]
                             (derive-view))
        now1             (-> ["some"
                              "input|"
                              -| "of"
                              -| "mine"
                              -| "that"
                              -| "is"
                              -| "this"
                              -| "long"]
                             (derive-view))
        now2             (-> ["some|"
                              "input"
                              -| "of"
                              -| "mine"
                              -| "that"
                              -| "is"
                              -| "this"
                              -| "long"]
                             (derive-view))
        expected1        (-> ["some"
                              -| "input|"
                              -| "of"
                              -| "mine"
                              -| "that"
                              -| "is"
                              -| "this"
                              -+ "long"]
                             (derive-view))
        expected2        (-> [-| "some|"
                              -| "input"
                              -| "of"
                              -| "mine"
                              -| "that"
                              -| "is"
                              -+ "this"
                              -+ "long"]
                             (derive-view))
        actual-voff1     (h/correct-between now1 then)
        actual-view1     (-> now1 (h/with-view-offset actual-voff1) (h/project) (:lines))
        actual-cursor1   (-> now1 (h/with-view-offset actual-voff1) (h/project) (:cursor))

        actual-voff2     (h/correct-between now2 then)
        actual-view2     (-> now2 (h/with-view-offset actual-voff2) (h/project) (:lines))
        actual-cursor2   (-> now2 (h/with-view-offset actual-voff2) (h/project) (:cursor))

        expected-voff1   1
        expected-view1   (-> expected1 (h/project) (:lines))
        expected-cursor1 (-> expected1 (h/project) (:cursor))

        expected-voff2   2
        expected-view2   (-> expected2 (h/project) (:lines))
        expected-cursor2 (-> expected2 (h/project) (:cursor))]
    (is (= actual-voff1 expected-voff1))
    (is (= actual-view1 expected-view1))
    (is (= actual-cursor1 expected-cursor1))

    (is (= actual-voff2 expected-voff2))
    (is (= actual-view2 expected-view2))
    (is (= actual-cursor2 expected-cursor2))))

(deftest corrects-exceeding-lower-view
  (let [then             (-> [-| "some"
                              -| "input"
                              -| "of"
                              -| "mine"
                              -| "that"
                              -| "is|"
                              -+ "this"
                              -+ "long"]
                             (derive-view))
        now1             (-> [-| "some"
                              -| "input"
                              -| "of"
                              -| "mine"
                              -| "that"
                              -| "is"
                              -+ "th|is"
                              -+ "long"]
                             (derive-view))
        now2             (-> [-| "some"
                              -| "input"
                              -| "of"
                              -| "mine"
                              -| "that"
                              -| "is"
                              -+ "this"
                              -+ "lo|ng"]
                             (derive-view))
        expected1        (-> [   "some"
                              -| "input"
                              -| "of"
                              -| "mine"
                              -| "that"
                              -| "is"
                              -| "th|is"
                              -+ "long"]
                             (derive-view))
        expected2        (-> [   "some"
                                 "input"
                              -| "of"
                              -| "mine"
                              -| "that"
                              -| "is"
                              -| "this"
                              -| "lo|ng"]
                             (derive-view))
        actual-voff1     (h/correct-between now1 then)
        actual-view1     (-> now1 (h/with-view-offset actual-voff1) (h/project) (:lines))
        actual-cursor1   (-> now1 (h/with-view-offset actual-voff1) (h/project) (:cursor))

        actual-voff2     (h/correct-between now2 then)
        actual-view2     (-> now2 (h/with-view-offset actual-voff2) (h/project) (:lines))
        actual-cursor2   (-> now2 (h/with-view-offset actual-voff2) (h/project) (:cursor))

        expected-voff1   1
        expected-view1   (-> expected1 (h/project) (:lines))
        expected-cursor1 (-> expected1 (h/project) (:cursor))

        expected-voff2   0
        expected-view2   (-> expected2 (h/project) (:lines))
        expected-cursor2 (-> expected2 (h/project) (:cursor))]
    (is (= actual-voff1 expected-voff1))
    (is (= actual-view1 expected-view1))
    (is (= actual-cursor1 expected-cursor1))

    (is (= actual-voff2 expected-voff2))
    (is (= actual-view2 expected-view2))
    (is (= actual-cursor2 expected-cursor2))))

(deftest does-not-correct-when-view-not-exceeded
  (let [then            (-> [-| "some"
                             -| "input"
                             -| "of"
                             -| "mine"
                             -| "that"
                             -| "is|"
                             -+ "this"
                             -+ "long"]
                            (derive-view))
        now             (-> [-| "some"
                             -| "input"
                             -| "o|f"
                             -| "mine"
                             -| "that"
                             -| "is"
                             -+ "this"
                             -+ "long"]
                            (derive-view))
        expected        (-> [-| "some"
                             -| "input"
                             -| "o|f"
                             -| "mine"
                             -| "that"
                             -| "is"
                             -+ "this"
                             -+ "long"]
                            (derive-view))

        actual-voff     (h/correct-between now then)
        actual-view     (-> now (h/with-view-offset actual-voff) (h/project) (:lines))
        actual-cursor   (-> now (h/with-view-offset actual-voff) (h/project) (:cursor))

        expected-voff   2
        expected-view   (-> expected (h/project) (:lines))
        expected-cursor (-> expected (h/project) (:cursor))]
    (is (= actual-voff expected-voff))
    (is (= actual-view expected-view))
    (is (= actual-cursor expected-cursor))))

(deftest corrects-text-shrinkage
  (let [then             (-> [-| "some"
                              -| "input"
                              -| "of"
                              -| "mine"
                              -| "that"
                              -| "is|"
                              -+ "this"
                              -+ "long"]
                             (derive-view))
        now1             (-> [-| "some"
                              -| "input"
                              -| "of"
                              -| "mine"
                              -| "that|"
                              -|
                              -+ "this"
                              -+ "long"]
                             (derive-view))
        now2             (-> [-|
                              -| "input"
                              -| "of"
                              -|
                              -| "that"
                              -| "is|"
                              -+ "this"
                              -+ "long"]
                             (derive-view))
        now3             (-> [-| "some"
                              -| "input"
                              -| "of"
                              -|
                              -|
                              -| "is|"
                              -+ "this"
                              -+ "long"]
                             (derive-view))
        expected1        (-> [-| "some"
                              -| "input"
                              -| "of"
                              -| "mine"
                              -| "that|"
                              -| "this"
                              -+ "long"]
                             (derive-view))
        expected2        (-> [-| "input"
                              -| "of"
                              -| "that"
                              -| "is|"
                              -| "this"
                              -| "long"]
                             (derive-view))
        expected3        (-> [-| "some"
                              -| "input"
                              -| "of"
                              -| "is|"
                              -| "this"
                              -| "long"]
                             (derive-view))
        actual-voff1     (h/correct-between now1 then)
        actual-view1     (-> now1 (h/with-view-offset actual-voff1) (h/project) (:lines))
        actual-cursor1   (-> now1 (h/with-view-offset actual-voff1) (h/project) (:cursor))

        actual-voff2     (h/correct-between now2 then)
        actual-view2     (-> now2 (h/with-view-offset actual-voff2) (h/project) (:lines))
        actual-cursor2   (-> now2 (h/with-view-offset actual-voff2) (h/project) (:cursor))


        actual-voff3     (h/correct-between now3 then)
        actual-view3     (-> now3 (h/with-view-offset actual-voff3) (h/project) (:lines))
        actual-cursor3   (-> now3 (h/with-view-offset actual-voff3) (h/project) (:cursor))

        expected-voff1   1
        expected-view1   (-> expected1 (h/project) (:lines))
        expected-cursor1 (-> expected1 (h/project) (:cursor))

        expected-voff2   0
        expected-view2   (-> expected2 (h/project) (:lines))
        expected-cursor2 (-> expected2 (h/project) (:cursor))

        expected-voff3   0
        expected-view3   (-> expected3 (h/project) (:lines))
        expected-cursor3 (-> expected3 (h/project) (:cursor))]
    (is (= actual-voff1 expected-voff1))
    (is (= actual-view1 expected-view1))
    (is (= actual-cursor1 expected-cursor1))

    (is (= actual-voff2 expected-voff2))
    (is (= actual-view2 expected-view2))
    (is (= actual-cursor2 expected-cursor2))

    (is (= actual-voff3 expected-voff3))
    (is (= actual-view3 expected-view3))
    (is (= actual-cursor3 expected-cursor3))))

(deftest corrects-text-growth-at-lower-view
  (let [then            (-> [-| "some"
                             -| "input"
                             -| "of"
                             -| "mine"
                             -| "that"
                             -| "is|"
                             -+ "this"
                             -+ "long"]
                            (derive-view))
        now             (-> [-| "some"
                             -| "input"
                             -| "of"
                             -| "mine"
                             -| "that"
                             -| "is|"
                             "suddenly"
                             -+ "this"
                             -+ "long"]
                            (derive-view))
        expected        (-> [-| "some"
                             -| "input"
                             -| "of"
                             -| "mine"
                             -| "that"
                             -| "is|"
                             -+ "suddenly"
                             -+ "this"
                             -+ "long"]
                            (derive-view))
        actual-voff     (h/correct-between now then)
        actual-view     (-> now (h/with-view-offset actual-voff) (h/project) (:lines))
        actual-cursor   (-> now (h/with-view-offset actual-voff) (h/project) (:cursor))

        expected-voff   3
        expected-view   (-> expected (h/project) (:lines))
        expected-cursor (-> expected (h/project) (:cursor))]
    (is (= actual-voff expected-voff))
    (is (= actual-view expected-view))
    (is (= actual-cursor expected-cursor))))

(deftest corrects-text-growth-at-upper-view
  (let [then            (-> [-| "|some"
                             -| "input"
                             -| "of"
                             -| "mine"
                             -| "that"
                             -| "is"]
                            (derive-view))
        now             (-> [""
                             ""
                             -| "|some"
                             -| "input"
                             -| "of"
                             -| "mine"
                             -| "that"
                             -| "is"]
                            (derive-view))
        expected        (-> [-| ""
                             -| ""
                             -| "|some"
                             -| "input"
                             -| "of"
                             -| "mine"
                             -+ "that"
                             -+ "is"]
                            (derive-view))
        actual-voff     (h/correct-between now then)
        actual-view     (-> now (h/with-view-offset actual-voff) (h/project) (:lines))
        actual-cursor   (-> now (h/with-view-offset actual-voff) (h/project) (:cursor))

        expected-voff   2
        expected-view   (-> expected (h/project) (:lines))
        expected-cursor (-> expected (h/project) (:cursor))]
    (is (= actual-voff expected-voff))
    (is (= actual-view expected-view))
    (is (= actual-cursor expected-cursor))))

(deftest corrects-text-growth-at-middle-view
  (let [then            (-> [-| "some"
                             -| "input"
                             -| "of|"
                             -| "mine"
                             -| "that"
                             -| "is"
                             -+ "this"
                             -+ "long"]
                            (derive-view))
        now             (-> [-| "some"
                             -| "input"
                             -| "of"
                                ""
                                "|"
                             -| "mine"
                             -| "that"
                             -| "is"
                             -+ "this"
                             -+ "long"]
                            (derive-view))
        expected        (-> [-| "some"
                             -| "input"
                             -| "of"
                             -| ""
                             -| "|"
                             -| "mine"
                             -+ "that"
                             -+ "is"
                             -+ "this"
                             -+ "long"]
                            (derive-view))
        actual-voff     (h/correct-between now then)
        actual-view     (-> now (h/with-view-offset actual-voff) (h/project) (:lines))
        actual-cursor   (-> now (h/with-view-offset actual-voff) (h/project) (:cursor))

        expected-voff   4
        expected-view   (-> expected (h/project) (:lines))
        expected-cursor (-> expected (h/project) (:cursor))]
    (is (= actual-voff expected-voff))
    (is (= actual-view expected-view))
    (is (= actual-cursor expected-cursor))))

(deftest corrects-text-following-natural-bottom-growth
  (let [then            (-> [-| "some"
                             -| "input"
                             -| "of"
                             -| "mine"
                             -| "that"
                             -| "is|"]
                            (derive-view))
        now             (-> [-| "some"
                             -| "input"
                             -| "of"
                             -| "mine"
                             -| "that"
                             -| "is"
                             ""
                             "|"]
                            (derive-view))
        expected        (-> ["some"
                             "input"
                             -| "of"
                             -| "mine"
                             -| "that"
                             -| "is"
                             -| ""
                             -| "|"]
                            (derive-view))
        actual-voff     (h/correct-between now then)
        actual-view     (-> now (h/with-view-offset actual-voff) (h/project) (:lines))
        actual-cursor   (-> now (h/with-view-offset actual-voff) (h/project) (:cursor))

        expected-voff   0
        expected-view   (-> expected (h/project) (:lines))
        expected-cursor (-> expected (h/project) (:cursor))]
    (is (= actual-voff expected-voff))
    (is (= actual-view expected-view))
    (is (= actual-cursor expected-cursor))))

(deftest corrects-text-under-growth-variance
  (let [then             (-> [-| "some"
                              -| "input"
                              -| "of"
                              -| "mine"
                              -| "that"
                              -| "is|"]
                             (derive-view))
        now1             (-> [-| "some"
                              -| "input"
                              -| "of"
                              -| "mine"
                              -| "that"
                              -| "is|"
                              "this"
                              "long"]
                             (derive-view))
        now2             (-> [-| "some"
                              -| "input"
                              -|
                              -| "mine"
                              -| "that"
                              -| "is|"
                              -+ "this"
                              -+ "long"]
                             (derive-view))
        expected1        (-> [-| "some"
                              -| "input"
                              -| "of"
                              -| "mine"
                              -| "that"
                              -| "is|"
                              -+ "this"
                              -+ "long"]
                             (derive-view))
        expected2        (-> [-| "some"
                              -| "input"
                              -| "mine"
                              -| "that"
                              -| "is|"
                              -| "this"
                              -+ "long"]
                             (derive-view))
        actual-voff1     (h/correct-between now1 then)
        actual-view1     (-> now1 (h/with-view-offset actual-voff1) (h/project) (:lines))
        actual-cursor1   (-> now1 (h/with-view-offset actual-voff1) (h/project) (:cursor))

        actual-voff2     (h/correct-between now2 expected1)
        actual-view2     (-> now2 (h/with-view-offset actual-voff2) (h/project) (:lines))
        actual-cursor2   (-> now2 (h/with-view-offset actual-voff2) (h/project) (:cursor))

        expected-voff1   2
        expected-view1   (-> expected1 (h/project) (:lines))
        expected-cursor1 (-> expected1 (h/project) (:cursor))

        expected-voff2   1
        expected-view2   (-> expected2 (h/project) (:lines))
        expected-cursor2 (-> expected2 (h/project) (:cursor))]
    (is (= actual-voff1 expected-voff1))
    (is (= actual-view1 expected-view1))
    (is (= actual-cursor1 expected-cursor1))

    (is (= actual-voff2 expected-voff2))
    (is (= actual-view2 expected-view2))
    (is (= actual-cursor2 expected-cursor2))))

(deftest corrects-text-following-view-enlargement
  (let [then            (-> [-| "some"
                             -| "input"
                             -| "of"
                             -| "mine"
                             -| "that"
                             -| "is|"
                             -+ "this"
                             -+ "large"]
                            (derive-view))
        now             (-> [-| "some"
                             -| "input"
                             -| "of"
                             -| "mine"
                             -| "that"
                             -| "is|"
                             -| "this"
                             -+
                             -+ "large"]
                            (derive-view))
        expected        (-> [-| "some"
                             -| "input"
                             -| "of"
                             -| "mine"
                             -| "that"
                             -| "is|"
                             -| "this"
                             -+ "large"]
                            (derive-view))
        actual-voff     (h/correct-between now then)
        actual-view     (-> now (h/with-view-offset actual-voff) (h/project) (:lines))
        actual-cursor   (-> now (h/with-view-offset actual-voff) (h/project) (:cursor))

        expected-voff   1
        expected-view   (-> expected (h/project) (:lines))
        expected-cursor (-> expected (h/project) (:cursor))]
    (is (= actual-voff expected-voff))
    (is (= actual-view expected-view))
    (is (= actual-cursor expected-cursor))))

(deftest corrects-text-following-view-shrinkage
  (let [then            (-> [-| "some"
                             -| "input"
                             -| "of"
                             -| "mine"
                             -| "that"
                             -| "is|"
                             -+ "this"
                             -+ "large"]
                            (derive-view))
        now             (-> [-| "some"
                             -| "input"
                             -| "of"
                             -| "mine"
                             -| "that|"
                             "is"
                             -+ "this"
                             -+ "large"]
                            (derive-view))
        expected        (-> [-| "some"
                             -| "input"
                             -| "of"
                             -| "mine"
                             -| "that|"
                             -+ "is"
                             -+ "this"
                             -+ "large"]
                            (derive-view))
        actual-voff     (h/correct-between now then)
        actual-view     (-> now (h/with-view-offset actual-voff) (h/project) (:lines))
        actual-cursor   (-> now (h/with-view-offset actual-voff) (h/project) (:cursor))

        expected-voff   3
        expected-view   (-> expected (h/project) (:lines))
        expected-cursor (-> expected (h/project) (:cursor))]
    (is (= actual-voff expected-voff))
    (is (= actual-view expected-view))
    (is (= actual-cursor expected-cursor))))
