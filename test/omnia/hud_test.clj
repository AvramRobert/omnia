(ns omnia.hud-test
  (:require [clojure.test :refer :all]
            [omnia.test-utils :refer :all]
            [omnia.repl.hud :as h]))

; extract (clip-sel (select-all (hud))) == project-hud (hud)
(deftest total-selection-law
  (is true))

; project-sel (select (hud)) = map project-cursor (select (hud))
(deftest distributive-projection-law
  (is true))

; extract (select (hud)) `contains` extract (project-hud (hud)) (project-sel (select (hud))
(deftest containment-projection-law
  (is true))

(deftest projects-y-coordinate
  (let [hud        (-> ["beginning"
                        -| "some"
                        -| "t|ext"]
                       (derive-hud))
        actual-y   (h/project-y hud (-> hud (h/text) (:cursor) (second)))
        expected-y 1]
    (is (= expected-y actual-y))))

(deftest projects-offset-y-coordinate
  (let [hud        (-> [-| "beginning"
                        -| "so|me"
                        -+ "text"]
                       (derive-hud))
        actual-y   (h/project-y hud (-> hud (h/text) (:cursor) (second)))
        expected-y 1]
    (is (= expected-y actual-y))))

(deftest projects-scrolled-y-coordinate
  (let [hud        (-> [ "beginning"
                        -|
                        -| "so|me"
                        -$ "text"]
                       (derive-hud))
        actual-y   (h/project-y hud (-> hud (h/text) (:cursor) (second)))
        expected-y 0]
    (is (= expected-y actual-y))))

(deftest projects-scrolled-and-offset-y-coordinate
  (let [hud        (-> ["beginning"
                        -|
                        -+ "so|me"
                        -+ "text"
                        -$ "entry"]
                       (derive-hud))
        actual-y   (h/project-y hud (-> hud (h/text) (:cursor) (second)))
        expected-y 0]
    (is (= expected-y actual-y))))

(deftest projects-entire-hud
  (let [hud             (-> [-| "beginning"
                             -| "some"
                             -| "text|"]
                            (derive-hud))
        expected        (-> ["beginning"
                             "some"
                             "text|"]
                            (derive-text))
        actual-view     (-> hud (h/project-hud) (:lines))
        actual-cursor   (-> hud (h/project-hud) (:cursor))
        expected-view   (:lines expected)
        expected-cursor (:cursor expected)]
    (is (= actual-view expected-view))
    (is (= actual-cursor expected-cursor))))

(deftest projects-field-of-view
  (let [hud             (-> ["beginning"
                             -| "some"
                             -| "t|ext"]
                            (derive-hud))
        expected        (-> ["some"
                             "t|ext"]
                            (derive-text))
        actual-view     (-> hud (h/project-hud) (:lines))
        actual-cursor   (-> hud (h/project-hud) (:cursor))
        expected-view   (:lines expected)
        expected-cursor (:cursor expected)]
    (is (= actual-view expected-view))
    (is (= actual-cursor expected-cursor))))

(deftest projects-offset-field-of-view
  (let [hud             (-> ["beginning"
                             -| "so|me"
                             -| "text"
                             -+ "somewhere"]
                            (derive-hud))
        expected        (-> ["so|me"
                             "text"]
                            (derive-text))
        actual-view     (-> hud (h/project-hud) (:lines))
        actual-cursor   (-> hud (h/project-hud) (:cursor))
        expected-view   (:lines expected)
        expected-cursor (:cursor expected)]
    (is (= actual-view expected-view))
    (is (= actual-cursor expected-cursor))))

(deftest projects-scrolled-field-of-view
  (let [hud             (-> ["beginning"
                             -| "some"
                             -| "|text"
                             -$ "somewhere"]
                            (derive-hud))
        expected        (-> ["|some"
                             "text"]
                            (derive-text))
        actual-view     (-> hud (h/project-hud) (:lines))
        actual-cursor   (-> hud (h/project-hud) (:cursor))
        expected-view   (:lines expected)
        expected-cursor (:cursor expected)]
    (is (= actual-view expected-view))
    (is (= actual-cursor expected-cursor))))

(deftest projects-scrolled-and-offset-field-of-view
  (let [hud             (-> ["beginning"
                             -| "some|"
                             -|
                             -+ "text"
                             -$ "somewhere"]
                            (derive-hud))
        expected        (-> ["begi|nning"
                             "some"]
                            (derive-text))
        actual-view     (-> hud (h/project-hud) (:lines))
        actual-cursor   (-> hud (h/project-hud) (:cursor))
        expected-view   (:lines expected)
        expected-cursor (:cursor expected)]
    (is (= actual-view expected-view))
    (is (= actual-cursor expected-cursor))))

(deftest projects-total-selections
  (let [hud          (-> ["some"
                          -| "⦇piece of"
                          -| "text⦈"]
                         (derive-hud))
        expected     (-> ["⦇piece of"
                          "text⦈"]
                         (derive-text))
        actual-sel   (h/project-selection hud (-> hud (h/text) (:selection)))
        expected-sel (:selection expected)]
    (is (= actual-sel expected-sel))))

(deftest projects-offset-total-selections
  (let [hud          (-> [-| "⦇some"
                          -| "piece of⦈"
                          -+ "text"]
                         (derive-hud))
        expected     (-> ["⦇some"
                          "piece of⦈"]
                         (derive-text))
        actual-sel   (h/project-selection hud (-> hud (h/text) (:selection)))
        expected-sel (:selection expected)]
    (is (= actual-sel expected-sel))))

(deftest projects-offset-partial-selections
  (let [hud          (-> [-| "⦇some"
                          -| "piece of"
                          -+ "text⦈"]
                         (derive-hud))
        expected     (-> ["⦇some"
                          "piece of⦈"]
                         (derive-text))
        actual-sel   (h/project-selection hud (-> hud (h/text) (:selection)))
        expected-sel (:selection expected)]
    (is (= actual-sel expected-sel))))

(deftest supports-pop-up-windows
  (let [hud               (-> ["1"] (derive-hud))
        window            (-> ["a" "b" "c"]
                              (derive-text)
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
                                (derive-text)
                                (h/riffle-window 2))
        hud1                (-> [-| "current"
                                 -| "text|"
                                 -| "input"
                                 -+ "not viewable"
                                 -+ "not viewable"]
                                (derive-hud)
                                (h/pop-up window))
        hud2                (-> [-| "current|"
                                 -| "text"
                                 -| "that"
                                 -| "is"
                                 -| "large"
                                 -| "enough"
                                 -+ "not viewable"
                                 -+ "not viewable"]
                                (derive-hud)
                                (h/pop-up window))
        hud3                (-> ["current"
                                 -| "text"
                                 -| "that|"
                                 -| "is"]
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
                             "------|"
                             "------"]
                            (derive-hud))
        actual-view     (-> context (h/text) (:lines))
        actual-cursor   (-> context (h/text) (:cursor))
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
                             (derive-hud))
        now1             (-> ["some"
                              "input|"
                              -| "of"
                              -| "mine"
                              -| "that"
                              -| "is"
                              -| "this"
                              -| "long"]
                             (derive-hud))
        now2             (-> ["some|"
                              "input"
                              -| "of"
                              -| "mine"
                              -| "that"
                              -| "is"
                              -| "this"
                              -| "long"]
                             (derive-hud))
        expected1        (-> ["some"
                              -| "input|"
                              -| "of"
                              -| "mine"
                              -| "that"
                              -| "is"
                              -| "this"
                              -+ "long"]
                             (derive-hud))
        expected2        (-> [-| "some|"
                              -| "input"
                              -| "of"
                              -| "mine"
                              -| "that"
                              -| "is"
                              -+ "this"
                              -+ "long"]
                             (derive-hud))
        actual-voff1     (h/correct-between now1 then)
        actual-view1     (-> now1 (h/with-view-offset actual-voff1) (h/project-hud) (:lines))
        actual-cursor1   (-> now1 (h/with-view-offset actual-voff1) (h/project-hud) (:cursor))

        actual-voff2     (h/correct-between now2 then)
        actual-view2     (-> now2 (h/with-view-offset actual-voff2) (h/project-hud) (:lines))
        actual-cursor2   (-> now2 (h/with-view-offset actual-voff2) (h/project-hud) (:cursor))

        expected-voff1   1
        expected-view1   (-> expected1 (h/project-hud) (:lines))
        expected-cursor1 (-> expected1 (h/project-hud) (:cursor))

        expected-voff2   2
        expected-view2   (-> expected2 (h/project-hud) (:lines))
        expected-cursor2 (-> expected2 (h/project-hud) (:cursor))]
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
                             (derive-hud))
        now1             (-> [-| "some"
                              -| "input"
                              -| "of"
                              -| "mine"
                              -| "that"
                              -| "is"
                              -+ "th|is"
                              -+ "long"]
                             (derive-hud))
        now2             (-> [-| "some"
                              -| "input"
                              -| "of"
                              -| "mine"
                              -| "that"
                              -| "is"
                              -+ "this"
                              -+ "lo|ng"]
                             (derive-hud))
        expected1        (-> [   "some"
                              -| "input"
                              -| "of"
                              -| "mine"
                              -| "that"
                              -| "is"
                              -| "th|is"
                              -+ "long"]
                             (derive-hud))
        expected2        (-> [   "some"
                                 "input"
                              -| "of"
                              -| "mine"
                              -| "that"
                              -| "is"
                              -| "this"
                              -| "lo|ng"]
                             (derive-hud))
        actual-voff1     (h/correct-between now1 then)
        actual-view1     (-> now1 (h/with-view-offset actual-voff1) (h/project-hud) (:lines))
        actual-cursor1   (-> now1 (h/with-view-offset actual-voff1) (h/project-hud) (:cursor))

        actual-voff2     (h/correct-between now2 then)
        actual-view2     (-> now2 (h/with-view-offset actual-voff2) (h/project-hud) (:lines))
        actual-cursor2   (-> now2 (h/with-view-offset actual-voff2) (h/project-hud) (:cursor))

        expected-voff1   1
        expected-view1   (-> expected1 (h/project-hud) (:lines))
        expected-cursor1 (-> expected1 (h/project-hud) (:cursor))

        expected-voff2   0
        expected-view2   (-> expected2 (h/project-hud) (:lines))
        expected-cursor2 (-> expected2 (h/project-hud) (:cursor))]
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
                            (derive-hud))
        now             (-> [-| "some"
                             -| "input"
                             -| "o|f"
                             -| "mine"
                             -| "that"
                             -| "is"
                             -+ "this"
                             -+ "long"]
                            (derive-hud))
        expected        (-> [-| "some"
                             -| "input"
                             -| "o|f"
                             -| "mine"
                             -| "that"
                             -| "is"
                             -+ "this"
                             -+ "long"]
                            (derive-hud))

        actual-voff     (h/correct-between now then)
        actual-view     (-> now (h/with-view-offset actual-voff) (h/project-hud) (:lines))
        actual-cursor   (-> now (h/with-view-offset actual-voff) (h/project-hud) (:cursor))

        expected-voff   2
        expected-view   (-> expected (h/project-hud) (:lines))
        expected-cursor (-> expected (h/project-hud) (:cursor))]
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
                             (derive-hud))
        now1             (-> [-| "some"
                              -| "input"
                              -| "of"
                              -| "mine"
                              -| "that|"
                              -|
                              -+ "this"
                              -+ "long"]
                             (derive-hud))
        now2             (-> [-|
                              -| "input"
                              -| "of"
                              -|
                              -| "that"
                              -| "is|"
                              -+ "this"
                              -+ "long"]
                             (derive-hud))
        now3             (-> [-| "some"
                              -| "input"
                              -| "of"
                              -|
                              -|
                              -| "is|"
                              -+ "this"
                              -+ "long"]
                             (derive-hud))
        expected1        (-> [-| "some"
                              -| "input"
                              -| "of"
                              -| "mine"
                              -| "that|"
                              -| "this"
                              -+ "long"]
                             (derive-hud))
        expected2        (-> [-| "input"
                              -| "of"
                              -| "that"
                              -| "is|"
                              -| "this"
                              -| "long"]
                             (derive-hud))
        expected3        (-> [-| "some"
                              -| "input"
                              -| "of"
                              -| "is|"
                              -| "this"
                              -| "long"]
                             (derive-hud))
        actual-voff1     (h/correct-between now1 then)
        actual-view1     (-> now1 (h/with-view-offset actual-voff1) (h/project-hud) (:lines))
        actual-cursor1   (-> now1 (h/with-view-offset actual-voff1) (h/project-hud) (:cursor))

        actual-voff2     (h/correct-between now2 then)
        actual-view2     (-> now2 (h/with-view-offset actual-voff2) (h/project-hud) (:lines))
        actual-cursor2   (-> now2 (h/with-view-offset actual-voff2) (h/project-hud) (:cursor))


        actual-voff3     (h/correct-between now3 then)
        actual-view3     (-> now3 (h/with-view-offset actual-voff3) (h/project-hud) (:lines))
        actual-cursor3   (-> now3 (h/with-view-offset actual-voff3) (h/project-hud) (:cursor))

        expected-voff1   1
        expected-view1   (-> expected1 (h/project-hud) (:lines))
        expected-cursor1 (-> expected1 (h/project-hud) (:cursor))

        expected-voff2   0
        expected-view2   (-> expected2 (h/project-hud) (:lines))
        expected-cursor2 (-> expected2 (h/project-hud) (:cursor))

        expected-voff3   0
        expected-view3   (-> expected3 (h/project-hud) (:lines))
        expected-cursor3 (-> expected3 (h/project-hud) (:cursor))]
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
                            (derive-hud))
        now             (-> [-| "some"
                             -| "input"
                             -| "of"
                             -| "mine"
                             -| "that"
                             -| "is|"
                             "suddenly"
                             -+ "this"
                             -+ "long"]
                            (derive-hud))
        expected        (-> [-| "some"
                             -| "input"
                             -| "of"
                             -| "mine"
                             -| "that"
                             -| "is|"
                             -+ "suddenly"
                             -+ "this"
                             -+ "long"]
                            (derive-hud))
        actual-voff     (h/correct-between now then)
        actual-view     (-> now (h/with-view-offset actual-voff) (h/project-hud) (:lines))
        actual-cursor   (-> now (h/with-view-offset actual-voff) (h/project-hud) (:cursor))

        expected-voff   3
        expected-view   (-> expected (h/project-hud) (:lines))
        expected-cursor (-> expected (h/project-hud) (:cursor))]
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
                            (derive-hud))
        now             (-> [""
                             ""
                             -| "|some"
                             -| "input"
                             -| "of"
                             -| "mine"
                             -| "that"
                             -| "is"]
                            (derive-hud))
        expected        (-> [-| ""
                             -| ""
                             -| "|some"
                             -| "input"
                             -| "of"
                             -| "mine"
                             -+ "that"
                             -+ "is"]
                            (derive-hud))
        actual-voff     (h/correct-between now then)
        actual-view     (-> now (h/with-view-offset actual-voff) (h/project-hud) (:lines))
        actual-cursor   (-> now (h/with-view-offset actual-voff) (h/project-hud) (:cursor))

        expected-voff   2
        expected-view   (-> expected (h/project-hud) (:lines))
        expected-cursor (-> expected (h/project-hud) (:cursor))]
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
                            (derive-hud))
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
                            (derive-hud))
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
                            (derive-hud))
        actual-voff     (h/correct-between now then)
        actual-view     (-> now (h/with-view-offset actual-voff) (h/project-hud) (:lines))
        actual-cursor   (-> now (h/with-view-offset actual-voff) (h/project-hud) (:cursor))

        expected-voff   4
        expected-view   (-> expected (h/project-hud) (:lines))
        expected-cursor (-> expected (h/project-hud) (:cursor))]
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
                            (derive-hud))
        now             (-> [-| "some"
                             -| "input"
                             -| "of"
                             -| "mine"
                             -| "that"
                             -| "is"
                             ""
                             "|"]
                            (derive-hud))
        expected        (-> ["some"
                             "input"
                             -| "of"
                             -| "mine"
                             -| "that"
                             -| "is"
                             -| ""
                             -| "|"]
                            (derive-hud))
        actual-voff     (h/correct-between now then)
        actual-view     (-> now (h/with-view-offset actual-voff) (h/project-hud) (:lines))
        actual-cursor   (-> now (h/with-view-offset actual-voff) (h/project-hud) (:cursor))

        expected-voff   0
        expected-view   (-> expected (h/project-hud) (:lines))
        expected-cursor (-> expected (h/project-hud) (:cursor))]
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
                             (derive-hud))
        now1             (-> [-| "some"
                              -| "input"
                              -| "of"
                              -| "mine"
                              -| "that"
                              -| "is|"
                              "this"
                              "long"]
                             (derive-hud))
        now2             (-> [-| "some"
                              -| "input"
                              -|
                              -| "mine"
                              -| "that"
                              -| "is|"
                              -+ "this"
                              -+ "long"]
                             (derive-hud))
        expected1        (-> [-| "some"
                              -| "input"
                              -| "of"
                              -| "mine"
                              -| "that"
                              -| "is|"
                              -+ "this"
                              -+ "long"]
                             (derive-hud))
        expected2        (-> [-| "some"
                              -| "input"
                              -| "mine"
                              -| "that"
                              -| "is|"
                              -| "this"
                              -+ "long"]
                             (derive-hud))
        actual-voff1     (h/correct-between now1 then)
        actual-view1     (-> now1 (h/with-view-offset actual-voff1) (h/project-hud) (:lines))
        actual-cursor1   (-> now1 (h/with-view-offset actual-voff1) (h/project-hud) (:cursor))

        actual-voff2     (h/correct-between now2 expected1)
        actual-view2     (-> now2 (h/with-view-offset actual-voff2) (h/project-hud) (:lines))
        actual-cursor2   (-> now2 (h/with-view-offset actual-voff2) (h/project-hud) (:cursor))

        expected-voff1   2
        expected-view1   (-> expected1 (h/project-hud) (:lines))
        expected-cursor1 (-> expected1 (h/project-hud) (:cursor))

        expected-voff2   1
        expected-view2   (-> expected2 (h/project-hud) (:lines))
        expected-cursor2 (-> expected2 (h/project-hud) (:cursor))]
    (is (= actual-voff1 expected-voff1))
    (is (= actual-view1 expected-view1))
    (is (= actual-cursor1 expected-cursor1))

    (is (= actual-voff2 expected-voff2))
    (is (= actual-view2 expected-view2))
    (is (= actual-cursor2 expected-cursor2))))

(deftest corrects-text-following-hud-enlargement
  (let [then            (-> [-| "some"
                             -| "input"
                             -| "of"
                             -| "mine"
                             -| "that"
                             -| "is|"
                             -+ "this"
                             -+ "large"]
                            (derive-hud))
        now             (-> [-| "some"
                             -| "input"
                             -| "of"
                             -| "mine"
                             -| "that"
                             -| "is|"
                             -| "this"
                             -+
                             -+ "large"]
                            (derive-hud))
        expected        (-> [-| "some"
                             -| "input"
                             -| "of"
                             -| "mine"
                             -| "that"
                             -| "is|"
                             -| "this"
                             -+ "large"]
                            (derive-hud))
        actual-voff     (h/correct-between now then)
        actual-view     (-> now (h/with-view-offset actual-voff) (h/project-hud) (:lines))
        actual-cursor   (-> now (h/with-view-offset actual-voff) (h/project-hud) (:cursor))

        expected-voff   1
        expected-view   (-> expected (h/project-hud) (:lines))
        expected-cursor (-> expected (h/project-hud) (:cursor))]
    (is (= actual-voff expected-voff))
    (is (= actual-view expected-view))
    (is (= actual-cursor expected-cursor))))

(deftest corrects-text-following-hud-shrinkage
  (let [then            (-> [-| "some"
                             -| "input"
                             -| "of"
                             -| "mine"
                             -| "that"
                             -| "is|"
                             -+ "this"
                             -+ "large"]
                            (derive-hud))
        now             (-> [-| "some"
                             -| "input"
                             -| "of"
                             -| "mine"
                             -| "that|"
                             "is"
                             -+ "this"
                             -+ "large"]
                            (derive-hud))
        expected        (-> [-| "some"
                             -| "input"
                             -| "of"
                             -| "mine"
                             -| "that|"
                             -+ "is"
                             -+ "this"
                             -+ "large"]
                            (derive-hud))
        actual-voff     (h/correct-between now then)
        actual-view     (-> now (h/with-view-offset actual-voff) (h/project-hud) (:lines))
        actual-cursor   (-> now (h/with-view-offset actual-voff) (h/project-hud) (:cursor))

        expected-voff   3
        expected-view   (-> expected (h/project-hud) (:lines))
        expected-cursor (-> expected (h/project-hud) (:cursor))]
    (is (= actual-voff expected-voff))
    (is (= actual-view expected-view))
    (is (= actual-cursor expected-cursor))))
