(ns omnia.context-test
  (:require [omnia.repl.view :as h]
            [omnia.repl.hud :as r]
            [omnia.repl.events :as e]
            [clojure.test :refer [is deftest testing]]
            [omnia.test-utils :refer :all]))

;; I. Manipulation

(deftest replacing-main-view-refreshes-preview
  (let [hud    (-> ["existing input|"]
                   (derive-hud))
        enrichment (derive-text ["some input|"])
        actual     (-> hud
                       (r/switch-view (-> hud
                                          (r/persisted-view)
                                          (h/enrich-with [enrichment])))
                       (r/current-view))
        expected   (-> ["existing input"
                        "some text|"]
                       (derive-hud)
                       (r/current-view))]
    (= expected actual)))

(deftest clipboard-is-renewed
  (let [actual   (-> ["existing input"]
                     (derive-hud)
                     (process [e/select-all e/copy (e/character \a) e/select-all e/copy])
                     (r/input-area)
                     (:clipboard))
        expected (derive-text ["existing inputa"])]
    (is (= expected actual))))

(deftest clipboard-is-propagated
  (let [actual   (-> ["existing |input"]
                     (derive-hud)
                     (process [e/select-all e/copy (e/character \a) (e/character \b)])
                     (r/input-area)
                     (:clipboard))
        expected (derive-text ["existing input"])]
    (is (= expected actual))))

;; II. Scrolling

(deftest scrolls-up
  (let [hud          (-> ["some"
                              "persisted"
                              "area"
                              ---
                              -| "existing"
                              -| "input|"
                              -+ "area"]
                             (derive-hud)
                             (process [e/scroll-up e/scroll-up e/scroll-up]))
         expected         (-> [-| "some"
                              -| "persisted"
                              -$ "area"
                              ---
                              -$ "existing"
                              -$ "input|"
                              -+ "area"]
                              (derive-hud))
        actual-preview   (-> hud (r/current-view) (h/project) (:lines))
        expected-preview (-> expected (r/current-view) (h/project) (:lines))
        expected-cursor  (-> hud (r/current-view) (h/text) (:cursor))
        actual-cursor    (-> expected (r/current-view) (h/text) (:cursor))
        actual-offset    (-> hud (r/current-view) (h/scroll-offset))
        expected-offset  (-> expected (r/current-view) (h/scroll-offset))]
    (is (= actual-offset expected-offset 3))
    (is (= actual-preview expected-preview))
    (is (= actual-cursor expected-cursor))))

(deftest scrolls-down
  (let [hud          (-> ["some"
                              "persisted"
                              "area"
                              ---
                              -| "existing"
                              -| "input|"
                              -+ "area"]
                             (derive-hud)
                             (process [e/scroll-up
                                       e/scroll-up
                                       e/scroll-up]))
        expected         (-> ["some"
                              "persisted"
                              -| "area"
                              ---
                              -| "existing"
                              -$ "input|"
                              -+ "area"]
                             (derive-hud))
        processed        (process hud [e/scroll-down e/scroll-down])
        actual-preview   (-> processed (r/current-view) (h/project) (:lines))
        expected-preview (-> expected (r/current-view) (h/project) (:lines))
        actual-cursor    (-> processed (r/current-view) (h/text) (:cursor))
        expected-cursor  (-> expected (r/current-view) (h/text) (:cursor))
        actual-offset    (-> processed (r/current-view) (h/scroll-offset))
        expected-offset  (-> expected (r/current-view) (h/scroll-offset))]
    (is (= actual-offset expected-offset 1))
    (is (= actual-preview expected-preview))
    (is (= actual-cursor expected-cursor))))

(deftest stops-scrolling-up-at-bounds
  (let [hud         (-> ["some"
                             "persisted"
                             "area"
                             ---
                             -| "existing"
                             -| "input|"
                             -+ "area"]
                        (derive-hud))
        scrolled        (process hud (repeat 100 e/scroll-up))
        init-offset     (-> hud (r/current-view) (h/scroll-offset))
        scrolled-offset (-> scrolled (r/current-view) (h/scroll-offset))]
    (is (= init-offset 0))
    (is (= scrolled-offset 12))))

(deftest stops-scrolling-down-at-bounds
  (let [hud            (-> ["some"
                                "persisted"
                                "area"
                                ---
                                -| "existing"
                                -| "input|"
                                -+ "area"]
                           (derive-hud))
        scrolled-up        (process hud (repeat 100 e/scroll-up))
        scrolled-down      (process hud (repeat 100 e/scroll-down))
        actual-up-offset   (-> scrolled-up (r/current-view) (h/scroll-offset))
        actual-down-offset (-> scrolled-down (r/current-view) (h/scroll-offset))]
    (is (= actual-up-offset 12))
    (is (= actual-down-offset 0))))

(deftest resets-scrolling
  (let [hud             (-> ["some"
                                 "persisted"
                                 "area"
                                 ---
                                 -| "existing"
                                 -| "input|"
                                 -+ "area"]
                            (derive-hud))
        scrolled            (process hud [e/scroll-up e/scroll-up])
        reset               (r/reset-scroll scrolled)
        init-scroll-offset  (-> scrolled (r/current-view) (h/scroll-offset))
        reset-scroll-offset (-> reset (r/current-view) (h/scroll-offset))]
    (is (= init-scroll-offset 2))
    (is (= reset-scroll-offset 0))))

;; III. Capturing

(deftest captures-input
  (let [hud       (-> ["persisted"
                           ---
                           "some input|"]
                      (derive-hud))
        expected      (-> ["persisted"
                           ---
                           "some inputa|"]
                          (derive-hud))
        processed     (process hud [(e/character \a)])
        actual-cursor     (-> processed (r/current-view) (h/text) (:cursor))
        actual-preview    (-> processed (r/current-view) (h/text) (:lines))
        actual-previous   (-> processed (r/previous-view) (h/text) (:lines))
        expected-cursor   (-> expected (r/current-view) (h/text) (:cursor))
        expected-preview  (-> expected (r/current-view) (h/text) (:lines))
        expected-previous (-> hud (r/current-view) (h/text) (:lines))]
    (is (= actual-preview expected-preview))
    (is (= actual-previous expected-previous))
    (is (= actual-cursor expected-cursor))))

;; IV. Clearing

(deftest clears-input
  (let [hud           (-> ["some"
                               "persisted"
                               ---
                               "input|"]
                          (derive-hud))
        expected          (-> ["input|"] (derive-hud))
        processed         (process hud [e/clear])
        actual-cursor     (-> processed (r/current-view) (h/text) (:cursor))
        actual-preview    (-> processed (r/current-view) (h/text) (:lines))
        actual-previous   (-> processed (r/previous-view) (h/text) (:lines))
        expected-cursor   (-> expected (r/current-view) (h/text) (:cursor))
        expected-preview  (-> expected (r/current-view) (h/text) (:lines))
        expected-previous (-> hud (r/current-view) (h/text) (:lines))]
    (is (= actual-preview expected-preview))
    (is (= actual-previous expected-previous))
    (is (= actual-cursor expected-cursor))))

;; V. Evaluating

(deftest evaluates-input
  (let [hud           (-> ["persisted"
                               ---
                               "(+ 1 1)|"]
                              (derive-hud {:response (value-response "2")}))
        processed         (process hud [e/evaluate])
        expected          (-> ["persisted"
                               "(+ 1 1)"
                               ""
                               "2"
                               ""
                               "Ω =>"
                               ---
                               "|"]
                              (derive-hud))
        expected-previous (-> hud (r/current-view) (h/text) (:lines))
        expected-preview  (-> expected (r/current-view) (h/text) (:lines))
        expected-cursor   (-> expected (r/current-view) (h/text) (:cursor))
        actual-preview    (-> processed (r/current-view) (h/text) (:lines))
        actual-previous   (-> processed (r/previous-view) (h/text) (:lines))
        actual-cursor     (-> processed (r/current-view) (h/text) (:cursor))]
    (is (= actual-preview expected-preview))
    (is (= actual-cursor expected-cursor))
    (is (= actual-previous expected-previous))))

;; VI. Previous and next evaluations

(deftest navigates-through-evaluation-history
  (let [hud           (-> ["persisted"
                               ---
                               "(+ 1 1)|"]
                              (derive-hud {:history ["past-eval-1"
                                                         "past-eval-2"]}))
        expected1          (-> ["persisted"
                               ---
                               "past-eval-1|"]
                               (derive-hud))
        expected2         (-> ["persisted"
                               ---
                               "past-eval-2|"]
                              (derive-hud))
        processed1         (process hud [e/prev-eval])
        processed2         (process hud [e/prev-eval e/prev-eval])
        processed3         (process hud [e/prev-eval e/prev-eval e/next-eval])
        actual-preview1    (-> processed1 (r/current-view) (h/text) (:lines))
        actual-cursor1     (-> processed1 (r/current-view) (h/text) (:cursor))
        expected-preview1  (-> expected1 (r/current-view) (h/text) (:lines))
        expected-cursor1   (-> expected1 (r/current-view) (h/text) (:cursor))

        actual-preview2    (-> processed2 (r/current-view) (h/text) (:lines))
        actual-cursor2     (-> processed2 (r/current-view) (h/text) (:cursor))
        expected-preview2  (-> expected2 (r/current-view) (h/text) (:lines))
        expected-cursor2   (-> expected2 (r/current-view) (h/text) (:cursor))

        actual-preview3    (-> processed3 (r/current-view) (h/text) (:lines))
        actual-cursor3     (-> processed3 (r/current-view) (h/text) (:cursor))]
    (is (= actual-preview1 expected-preview1))
    (is (= actual-cursor1 expected-cursor1))

    (is (= actual-preview2 expected-preview2))
    (is (= actual-cursor2 expected-cursor2))

    (is (= actual-preview3 expected-preview1))
    (is (= actual-cursor3 expected-cursor1))))

(deftest preserves-clipboard-during-evaluation-navigation
  (let [hud          (-> ["persisted"
                              ---
                              "some ⦇text⦈|"]
                             (derive-hud {:history ["prev-eval-1"
                                                        "prev-eval-2"]})
                             (process [e/copy
                                       e/prev-eval
                                       e/prev-eval
                                       e/paste]))
        expected         (-> ["persisted"
                              ---
                              "prev-eval-2text|"]
                             (derive-hud))
        actual-preview   (-> hud (r/current-view) (h/text) (:lines))
        actual-cursor    (-> hud (r/current-view) (h/text) (:cursor))
        expected-preview (-> expected (r/current-view) (h/text) (:lines))
        expected-cursor  (-> expected (r/current-view) (h/text) (:cursor))]
    (is (= actual-preview expected-preview))
    (is (= actual-cursor expected-cursor))))

;; VII. Suggesting

(deftest shows-suggestions
  (let [hud           (-> ["persisted"
                               ---
                               "s|ome things are"]
                              (derive-hud {:response
                                               (completion-response ["option-1"
                                                                     "option-2"])}))
        expected1         (-> ["persisted"
                               ---
                               "option-1 things are"
                               "------"
                               " option-1|"
                               " option-2"
                               "------"]
                              (derive-hud))
        expected2         (-> ["persisted"
                               ---
                               "option-2 things are"
                               "------"
                               " option-1"
                               " option-2|"
                               "------"]
                              (derive-hud))
        processed1        (process hud [e/suggest])
        processed2        (process hud [e/suggest e/suggest])

        actual1-preview   (-> processed1 (r/current-view) (h/text) (:lines))
        actual1-cursor    (-> processed1 (r/current-view) (h/text) (:cursor))

        actual2-preview   (-> processed2 (r/current-view) (h/text) (:lines))
        actual2-cursor    (-> processed2 (r/current-view) (h/text) (:cursor))

        expected1-preview (-> expected1 (r/current-view) (h/text) (:lines))
        expected1-cursor  (-> expected1 (r/current-view) (h/text) (:cursor))

        expected2-preview (-> expected2 (r/current-view) (h/text) (:lines))
        expected2-cursor  (-> expected2 (r/current-view) (h/text) (:cursor))]
    (is (= actual1-preview expected1-preview))
    (is (= actual2-preview expected2-preview))
    (is (= actual1-cursor expected1-cursor))
    (is (= actual2-cursor expected2-cursor))))

(deftest keeps-suggestion-upon-input
  (let [hud          (-> ["persisted"
                              ---
                              "1|"]
                             (derive-hud {:response
                                              (completion-response ["option-1"
                                                                    "option-2"])})
                             (process [e/suggest e/suggest (e/character \a)]))
        expected         (-> ["persisted"
                              ---
                              "option-2a|"]
                             (derive-hud))
        actual-preview   (-> hud (r/current-view) (h/text) (:lines))
        actual-cursor    (-> hud (r/current-view) (h/text) (:cursor))
        expected-preview (-> expected (r/current-view) (h/text) (:lines))
        expected-cursor  (-> expected (r/current-view) (h/text) (:cursor))]
    (is (= actual-preview expected-preview))
    (is (= actual-cursor expected-cursor))))

(deftest shows-empty-box-when-no-suggestions
  (let [hud (-> ["persisted"
                     ---
                     "1|"]
                    (derive-hud {:response (completion-response [])})
                    (process [e/suggest]))
        expected (-> ["persisted"
                      ---
                      "1"
                      "------|"
                      "------"]
                     (derive-hud))
        actual-preview   (-> hud (r/current-view) (h/text) (:lines))
        actual-cursor    (-> hud (r/current-view) (h/text) (:cursor))
        expected-preview (-> expected (r/current-view) (h/text) (:lines))
        expected-cursor  (-> expected (r/current-view) (h/text) (:cursor))]
    (is (= actual-preview expected-preview))
    (is (= actual-cursor expected-cursor))))

;; VIII. Highlighting

(deftest gc-same-line-selection
  (testing "Moving left"
    (let [result     (-> ["This| is a line"]
                         (derive-hud)
                         (process [e/select-left e/select-left e/select-right]))
          highlights (-> ["Thi⦇s⦈ is a line"]
                         (derive-hud)
                         (r/highlights)
                         (:manual)
                         (:region))
          garbage    (-> ["Th⦇is⦈ is a line"]
                         (derive-hud)
                         (r/highlights)
                         (:manual)
                         (:region))
          expected-high (-> result (r/highlights) (:selection) (:region))
          expected-gc   (-> result (r/garbage) (:selection) (:region))]
      (is (= expected-high highlights) "Highlights mismatch")
      (is (= expected-gc garbage) "Garbage mismatch")))

  (testing "Moving right"
    (let [result     (-> ["|This is a line"]
                         (derive-hud)
                         (process [e/select-right e/select-right e/select-left]))
          highlights (-> ["⦇T⦈his is a line"]
                         (derive-hud)
                         (r/highlights)
                         (:manual)
                         (:region))
          garbage    (-> ["⦇Th⦈is is a line"]
                         (derive-hud)
                         (r/highlights)
                         (:manual)
                         (:region))
          expected-high (-> result (r/highlights) (:selection) (:region))
          expected-gc   (-> result (r/garbage) (:selection) (:region))]
      (is (= expected-high highlights) "Highlights mismatch")
      (is (= expected-gc garbage) "Garbage mismatch"))))

(deftest gc-multi-line-selection
  (testing "Moving up"
    (let [result        (-> ["This is"
                             "a |large"
                             "piece of text"]
                            (derive-hud)
                            (process [e/select-all e/select-up e/select-up]))
          highlights    (-> ["⦇This is⦈"
                             "a large"
                             "piece of text"]
                            (derive-hud)
                            (r/highlights)
                            (:manual)
                            (:region))
          garbage       (-> ["⦇This is"
                             "a large⦈"
                             "piece of text"]
                            (derive-hud)
                            (r/highlights)
                            (:manual)
                            (:region))
          expected-high (-> result (r/highlights) (:selection) (:region))
          expected-gc   (-> result (r/garbage) (:selection) (:region))]
      (is (= expected-high highlights) "Highlights mismatch")
      (is (= expected-gc garbage) "Garbage mismatch")))

  (testing "Moving up left"
    (let [result        (-> ["This is"
                             "a |large"
                             "piece of text"]
                            (derive-hud)
                            (process [e/select-all e/select-up e/select-up e/select-left]))
          highlights    (-> ["⦇This i⦈s"
                             "a large"
                             "piece of text"]
                            (derive-hud)
                            (r/highlights)
                            (:manual)
                            (:region))
          garbage       (-> ["⦇This is⦈"
                             "a large"
                             "piece of text"]
                            (derive-hud)
                            (r/highlights)
                            (:manual)
                            (:region))
          expected-high (-> result (r/highlights) (:selection) (:region))
          expected-gc   (-> result (r/garbage) (:selection) (:region))]
      (is (= expected-high highlights) "Highlights mismatch")
      (is (= expected-gc garbage) "Garbage mismatch")))

  (testing "Moving up right"
    (let [result        (-> ["This is"
                             "a |large"
                             "piece of text"]
                            (derive-hud)
                            (process [e/select-all e/select-up e/select-up e/select-right]))
          highlights    (-> ["⦇This is"
                             "⦈a large"
                             "piece of text"]
                            (derive-hud)
                            (r/highlights)
                            (:manual)
                            (:region))
          garbage       (-> ["⦇This is⦈"
                             "a large"
                             "piece of text"]
                            (derive-hud)
                            (r/highlights)
                            (:manual)
                            (:region))
          expected-high (-> result (r/highlights) (:selection) (:region))
          expected-gc   (-> result (r/garbage) (:selection) (:region))]
      (is (= expected-high highlights) "Highlights mismatch")
      (is (= expected-gc garbage) "Garbage mismatch")))

  (testing "Moving down"
    (let [result        (-> ["This is"
                             "a |large"
                             "piece of text"]
                            (derive-hud)
                            (process [e/select-all e/select-up e/select-up e/select-down]))
          highlights    (-> ["⦇This is"
                             "a large⦈"
                             "piece of text"]
                            (derive-hud)
                            (r/highlights)
                            (:manual)
                            (:region))
          garbage       (-> ["⦇This is⦈"
                             "a large"
                             "piece of text"]
                            (derive-hud)
                            (r/highlights)
                            (:manual)
                            (:region))
          expected-high (-> result (r/highlights) (:selection) (:region))
          expected-gc   (-> result (r/garbage) (:selection) (:region))]
      (is (= expected-high highlights) "Highlights mismatch")
      (is (= expected-gc garbage) "Garbage mismatch")))

  (testing "Moving down left"
    (let [result        (-> ["This is"
                             "a |large"
                             "piece of text"]
                            (derive-hud)
                            (process [e/select-all e/select-up e/select-up e/select-down e/select-left]))
          highlights    (-> ["⦇This is"
                             "a larg⦈e"
                             "piece of text"]
                            (derive-hud)
                            (r/highlights)
                            (:manual)
                            (:region))
          garbage       (-> ["⦇This is"
                             "a large⦈"
                             "piece of text"]
                            (derive-hud)
                            (r/highlights)
                            (:manual)
                            (:region))
          expected-high (-> result (r/highlights) (:selection) (:region))
          expected-gc   (-> result (r/garbage) (:selection) (:region))]
      (is (= expected-high highlights) "Highlights mismatch")
      (is (= expected-gc garbage) "Garbage mismatch")))

  (testing "Moving down right"
    (let [result        (-> ["This is"
                             "a |large"
                             "piece of text"]
                            (derive-hud)
                            (process [e/select-all e/select-up e/select-up e/select-down e/select-right]))
          highlights    (-> ["⦇This is"
                             "a large"
                             "⦈piece of text"]
                            (derive-hud)
                            (r/highlights)
                            (:manual)
                            (:region))
          garbage       (-> ["⦇This is"
                             "a large⦈"
                             "piece of text"]
                            (derive-hud)
                            (r/highlights)
                            (:manual)
                            (:region))
          expected-high (-> result (r/highlights) (:selection) (:region))
          expected-gc   (-> result (r/garbage) (:selection) (:region))]
      (is (= expected-high highlights) "Highlights mismatch")
      (is (= expected-gc garbage) "Garbage mismatch"))))

(deftest gc-reset
  (testing "Multiple lines"
    (let [result        (-> ["Some |piece of text"
                             "with lines"]
                            (derive-hud)
                            (process [e/select-all e/move-right]))
          garbage       (-> ["⦇Some piece of text"
                             "with lines⦈"]
                            (derive-hud)
                            (r/highlights)
                            (:manual)
                            (:region))
          expected-high (-> result (r/highlights) (:selection) (:region))
          expected-gc   (-> result (r/garbage) (:selection) (:region))]
      (is (= expected-high nil) "Highlights mismatch")
      (is (= expected-gc garbage) "Garbage mismatch")))

  (testing "Same line"
    (testing "Multiple lines"
      (let [result        (-> ["Some |piece of text"]
                              (derive-hud)
                              (process [e/select-right e/select-right e/move-right]))
            garbage       (-> ["Some ⦇pi⦈ece of text"]
                              (derive-hud)
                              (r/highlights)
                              (:manual)
                              (:region))
            expected-high (-> result (r/highlights) (:selection) (:region))
            expected-gc   (-> result (r/garbage) (:selection) (:region))]
        (is (= expected-high nil) "Highlights mismatch")
        (is (= expected-gc garbage) "Garbage mismatch")))))

;; IX. Parenthesis matching

(deftest matches-parentheses
  (let [hud         (-> ["persisted"
                             ---
                             "(+ 1 1|)"]
                            (derive-hud)
                            (process [e/move-right]))
        expected-open   (-> ["persisted"
                             ---
                             "⦇(⦈+ 1 1)|"]
                            (derive-hud))
        expected-closed (-> ["persisted"
                             ---
                             "(+ 1 1⦇)⦈|"]
                            (derive-hud))
        actual-open     (-> hud (r/highlights) (:open-paren) (:region))
        actual-closed   (-> hud (r/highlights) (:closed-paren) (:region))
        expected-open   (-> expected-open (r/highlights) (:manual) (:region))
        expected-closed (-> expected-closed (r/highlights) (:manual) (:region))]
    (is (= actual-open expected-open))
    (is (= actual-closed expected-closed))))

(deftest does-not-highlight-unmatched-parentheses
  (let [hud (-> ["persisted"
                     ---
                     "(|+ 1"]
                    (derive-hud)
                    (process [e/move-left]))
        actual-high (r/highlights hud)]
    (is (= actual-high {}))))
