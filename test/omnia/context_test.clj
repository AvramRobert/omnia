(ns omnia.context-test
  (:require [omnia.repl.view :as h]
            [omnia.repl.context :as r]
            [omnia.repl.events :as e]
            [clojure.test :refer [is deftest testing]]
            [omnia.test-utils :refer :all]))

;; I. Manipulation

(deftest replacing-main-view-refreshes-preview
  (let [context    (-> ["existing input|"]
                       (derive-context))
        enrichment (derive-text ["some input|"])
        actual     (-> context
                       (r/switch-view (-> context
                                          (r/persisted-view)
                                          (h/enrich-with [enrichment])))
                       (r/current-view))
        expected   (-> ["existing input"
                        "some text|"]
                       (derive-context)
                       (r/current-view))]
    (= expected actual)))

(deftest clipboard-is-renewed
  (let [actual   (-> ["existing input"]
                     (derive-context)
                     (process [e/select-all e/copy (e/character \a) e/select-all e/copy])
                     (r/input-area)
                     (:clipboard))
        expected (derive-text ["existing inputa"])]
    (is (= expected actual))))

(deftest clipboard-is-propagated
  (let [actual   (-> ["existing |input"]
                     (derive-context)
                     (process [e/select-all e/copy (e/character \a) (e/character \b)])
                     (r/input-area)
                     (:clipboard))
        expected (derive-text ["existing input"])]
    (is (= expected actual))))

;; II. Scrolling

(deftest scrolls-up
  (let [context          (-> ["some"
                              "persisted"
                              "area"
                              ---
                              -| "existing"
                              -| "input|"
                              -+ "area"]
                             (derive-context)
                             (process [e/scroll-up e/scroll-up e/scroll-up]))
         expected         (-> [-| "some"
                              -| "persisted"
                              -$ "area"
                              ---
                              -$ "existing"
                              -$ "input|"
                              -+ "area"]
                             (derive-context))
        actual-preview   (-> context (r/current-view) (h/project) (:lines))
        expected-preview (-> expected (r/current-view) (h/project) (:lines))
        expected-cursor  (-> context (r/current-view) (h/text) (:cursor))
        actual-cursor    (-> expected (r/current-view) (h/text) (:cursor))
        actual-offset    (-> context (r/current-view) (h/scroll-offset))
        expected-offset  (-> expected (r/current-view) (h/scroll-offset))]
    (is (= actual-offset expected-offset 3))
    (is (= actual-preview expected-preview))
    (is (= actual-cursor expected-cursor))))

(deftest scrolls-down
  (let [context          (-> ["some"
                              "persisted"
                              "area"
                              ---
                              -| "existing"
                              -| "input|"
                              -+ "area"]
                             (derive-context)
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
                             (derive-context))
        processed        (process context [e/scroll-down e/scroll-down])
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
  (let [context         (-> ["some"
                             "persisted"
                             "area"
                             ---
                             -| "existing"
                             -| "input|"
                             -+ "area"]
                            (derive-context))
        scrolled        (process context (repeat 100 e/scroll-up))
        init-offset     (-> context (r/current-view) (h/scroll-offset))
        scrolled-offset (-> scrolled (r/current-view) (h/scroll-offset))]
    (is (= init-offset 0))
    (is (= scrolled-offset 12))))

(deftest stops-scrolling-down-at-bounds
  (let [context            (-> ["some"
                                "persisted"
                                "area"
                                ---
                                -| "existing"
                                -| "input|"
                                -+ "area"]
                               (derive-context))
        scrolled-up        (process context (repeat 100 e/scroll-up))
        scrolled-down      (process context (repeat 100 e/scroll-down))
        actual-up-offset   (-> scrolled-up (r/current-view) (h/scroll-offset))
        actual-down-offset (-> scrolled-down (r/current-view) (h/scroll-offset))]
    (is (= actual-up-offset 12))
    (is (= actual-down-offset 0))))

(deftest resets-scrolling
  (let [context             (-> ["some"
                                 "persisted"
                                 "area"
                                 ---
                                 -| "existing"
                                 -| "input|"
                                 -+ "area"]
                                (derive-context))
        scrolled            (process context [e/scroll-up e/scroll-up])
        reset               (r/reset-scroll scrolled)
        init-scroll-offset  (-> scrolled (r/current-view) (h/scroll-offset))
        reset-scroll-offset (-> reset (r/current-view) (h/scroll-offset))]
    (is (= init-scroll-offset 2))
    (is (= reset-scroll-offset 0))))

;; III. Capturing

(deftest captures-input
  (let [context       (-> ["persisted"
                           ---
                           "some input|"]
                          (derive-context))
        expected      (-> ["persisted"
                           ---
                           "some inputa|"]
                          (derive-context))
        processed     (process context [(e/character \a)])
        actual-cursor     (-> processed (r/current-view) (h/text) (:cursor))
        actual-preview    (-> processed (r/current-view) (h/text) (:lines))
        actual-previous   (-> processed (r/previous-view) (h/text) (:lines))
        expected-cursor   (-> expected (r/current-view) (h/text) (:cursor))
        expected-preview  (-> expected (r/current-view) (h/text) (:lines))
        expected-previous (-> context (r/current-view) (h/text) (:lines))]
    (is (= actual-preview expected-preview))
    (is (= actual-previous expected-previous))
    (is (= actual-cursor expected-cursor))))

;; IV. Clearing

(deftest clears-input
  (let [context           (-> ["some"
                               "persisted"
                               ---
                               "input|"]
                              (derive-context))
        expected          (-> ["input|"] (derive-context))
        processed         (process context [e/clear])
        actual-cursor     (-> processed (r/current-view) (h/text) (:cursor))
        actual-preview    (-> processed (r/current-view) (h/text) (:lines))
        actual-previous   (-> processed (r/previous-view) (h/text) (:lines))
        expected-cursor   (-> expected (r/current-view) (h/text) (:cursor))
        expected-preview  (-> expected (r/current-view) (h/text) (:lines))
        expected-previous (-> context (r/current-view) (h/text) (:lines))]
    (is (= actual-preview expected-preview))
    (is (= actual-previous expected-previous))
    (is (= actual-cursor expected-cursor))))

;; V. Evaluating

(deftest evaluates-input
  (let [context           (-> ["persisted"
                               ---
                               "(+ 1 1)|"]
                              (derive-context {:response (value-response "2")}))
        processed         (process context [e/evaluate])
        expected          (-> ["persisted"
                               "(+ 1 1)"
                               ""
                               "2"
                               ""
                               "Ω =>"
                               ---
                               "|"]
                              (derive-context))
        expected-previous (-> context (r/current-view) (h/text) (:lines))
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
  (let [context           (-> ["persisted"
                               ---
                               "(+ 1 1)|"]
                              (derive-context {:history ["past-eval-1"
                                                         "past-eval-2"]}))
        expected1          (-> ["persisted"
                               ---
                               "past-eval-1|"]
                              (derive-context))
        expected2         (-> ["persisted"
                               ---
                               "past-eval-2|"]
                              (derive-context))
        processed1         (process context [e/prev-eval])
        processed2         (process context [e/prev-eval e/prev-eval])
        processed3         (process context [e/prev-eval e/prev-eval e/next-eval])
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
  (let [context          (-> ["persisted"
                              ---
                              "some ⦇text⦈|"]
                             (derive-context {:history ["prev-eval-1"
                                                        "prev-eval-2"]})
                             (process [e/copy
                                       e/prev-eval
                                       e/prev-eval
                                       e/paste]))
        expected         (-> ["persisted"
                              ---
                              "prev-eval-2text|"]
                             (derive-context))
        actual-preview   (-> context (r/current-view) (h/text) (:lines))
        actual-cursor    (-> context (r/current-view) (h/text) (:cursor))
        expected-preview (-> expected (r/current-view) (h/text) (:lines))
        expected-cursor  (-> expected (r/current-view) (h/text) (:cursor))]
    (is (= actual-preview expected-preview))
    (is (= actual-cursor expected-cursor))))

;; VII. Suggesting

(deftest shows-suggestions
  (let [context           (-> ["persisted"
                               ---
                               "s|ome things are"]
                              (derive-context {:response
                                               (completion-response ["option-1"
                                                                     "option-2"])}))
        expected1         (-> ["persisted"
                               ---
                               "option-1 things are"
                               "------"
                               " option-1|"
                               " option-2"
                               "------"]
                              (derive-context))
        expected2         (-> ["persisted"
                               ---
                               "option-2 things are"
                               "------"
                               " option-1"
                               " option-2|"
                               "------"]
                              (derive-context))
        processed1        (process context [e/suggest])
        processed2        (process context [e/suggest e/suggest])

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
  (let [context          (-> ["persisted"
                              ---
                              "1|"]
                             (derive-context {:response
                                              (completion-response ["option-1"
                                                                    "option-2"])})
                             (process [e/suggest e/suggest (e/character \a)]))
        expected         (-> ["persisted"
                              ---
                              "option-2a|"]
                             (derive-context))
        actual-preview   (-> context (r/current-view) (h/text) (:lines))
        actual-cursor    (-> context (r/current-view) (h/text) (:cursor))
        expected-preview (-> expected (r/current-view) (h/text) (:lines))
        expected-cursor  (-> expected (r/current-view) (h/text) (:cursor))]
    (is (= actual-preview expected-preview))
    (is (= actual-cursor expected-cursor))))

(deftest shows-empty-box-when-no-suggestions
  (let [context (-> ["persisted"
                     ---
                     "1|"]
                    (derive-context {:response (completion-response [])})
                    (process [e/suggest]))
        expected (-> ["persisted"
                      ---
                      "1"
                      "------|"
                      "------"]
                     (derive-context))
        actual-preview   (-> context (r/current-view) (h/text) (:lines))
        actual-cursor    (-> context (r/current-view) (h/text) (:cursor))
        expected-preview (-> expected (r/current-view) (h/text) (:lines))
        expected-cursor  (-> expected (r/current-view) (h/text) (:cursor))]
    (is (= actual-preview expected-preview))
    (is (= actual-cursor expected-cursor))))

;; VIII. Highlighting

(deftest gc-same-line-selection
  (testing "Moving left"
    (let [result     (-> ["This| is a line"]
                         (derive-context)
                         (process [e/select-left e/select-left e/select-right]))
          highlights (-> ["Thi⦇s⦈ is a line"]
                         (derive-context)
                         (r/highlights)
                         (:manual)
                         (:region))
          garbage    (-> ["Th⦇is⦈ is a line"]
                         (derive-context)
                         (r/highlights)
                         (:manual)
                         (:region))
          expected-high (-> result (r/highlights) (:selection) (:region))
          expected-gc   (-> result (r/garbage) (:selection) (:region))]
      (is (= expected-high highlights) "Highlights mismatch")
      (is (= expected-gc garbage) "Garbage mismatch")))

  (testing "Moving right"
    (let [result     (-> ["|This is a line"]
                         (derive-context)
                         (process [e/select-right e/select-right e/select-left]))
          highlights (-> ["⦇T⦈his is a line"]
                         (derive-context)
                         (r/highlights)
                         (:manual)
                         (:region))
          garbage    (-> ["⦇Th⦈is is a line"]
                         (derive-context)
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
                             "context"]
                            (derive-context)
                            (process [e/select-all e/select-up e/select-up]))
          highlights    (-> ["⦇This is⦈"
                             "a large"
                             "context"]
                            (derive-context)
                            (r/highlights)
                            (:manual)
                            (:region))
          garbage       (-> ["⦇This is"
                             "a large⦈"
                             "context"]
                            (derive-context)
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
                             "context"]
                            (derive-context)
                            (process [e/select-all e/select-up e/select-up e/select-left]))
          highlights    (-> ["⦇This i⦈s"
                             "a large"
                             "context"]
                            (derive-context)
                            (r/highlights)
                            (:manual)
                            (:region))
          garbage       (-> ["⦇This is⦈"
                             "a large"
                             "context"]
                            (derive-context)
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
                             "context"]
                            (derive-context)
                            (process [e/select-all e/select-up e/select-up e/select-right]))
          highlights    (-> ["⦇This is"
                             "⦈a large"
                             "context"]
                            (derive-context)
                            (r/highlights)
                            (:manual)
                            (:region))
          garbage       (-> ["⦇This is⦈"
                             "a large"
                             "context"]
                            (derive-context)
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
                             "context"]
                            (derive-context)
                            (process [e/select-all e/select-up e/select-up e/select-down]))
          highlights    (-> ["⦇This is"
                             "a large⦈"
                             "context"]
                            (derive-context)
                            (r/highlights)
                            (:manual)
                            (:region))
          garbage       (-> ["⦇This is⦈"
                             "a large"
                             "context"]
                            (derive-context)
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
                             "context"]
                            (derive-context)
                            (process [e/select-all e/select-up e/select-up e/select-down e/select-left]))
          highlights    (-> ["⦇This is"
                             "a larg⦈e"
                             "context"]
                            (derive-context)
                            (r/highlights)
                            (:manual)
                            (:region))
          garbage       (-> ["⦇This is"
                             "a large⦈"
                             "context"]
                            (derive-context)
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
                             "context"]
                            (derive-context)
                            (process [e/select-all e/select-up e/select-up e/select-down e/select-right]))
          highlights    (-> ["⦇This is"
                             "a large"
                             "⦈context"]
                            (derive-context)
                            (r/highlights)
                            (:manual)
                            (:region))
          garbage       (-> ["⦇This is"
                             "a large⦈"
                             "context"]
                            (derive-context)
                            (r/highlights)
                            (:manual)
                            (:region))
          expected-high (-> result (r/highlights) (:selection) (:region))
          expected-gc   (-> result (r/garbage) (:selection) (:region))]
      (is (= expected-high highlights) "Highlights mismatch")
      (is (= expected-gc garbage) "Garbage mismatch"))))

(deftest gc-reset
  (testing "Multiple lines"
    (let [result        (-> ["Some |context"
                             "with lines"]
                            (derive-context)
                            (process [e/select-all e/move-right]))
          garbage       (-> ["⦇Some context"
                             "with lines⦈"]
                            (derive-context)
                            (r/highlights)
                            (:manual)
                            (:region))
          expected-high (-> result (r/highlights) (:selection) (:region))
          expected-gc   (-> result (r/garbage) (:selection) (:region))]
      (is (= expected-high nil) "Highlights mismatch")
      (is (= expected-gc garbage) "Garbage mismatch")))

  (testing "Same line"
    (testing "Multiple lines"
      (let [result        (-> ["Some |context"]
                              (derive-context)
                              (process [e/select-right e/select-right e/move-right]))
            garbage       (-> ["Some ⦇co⦈ntext"]
                              (derive-context)
                              (r/highlights)
                              (:manual)
                              (:region))
            expected-high (-> result (r/highlights) (:selection) (:region))
            expected-gc   (-> result (r/garbage) (:selection) (:region))]
        (is (= expected-high nil) "Highlights mismatch")
        (is (= expected-gc garbage) "Garbage mismatch")))))

;; IX. Parenthesis matching

(deftest matches-parentheses
  (let [context         (-> ["persisted"
                             ---
                             "(+ 1 1|)"]
                            (derive-context)
                            (process [e/move-right]))
        expected-open   (-> ["persisted"
                             ---
                             "⦇(⦈+ 1 1)|"]
                            (derive-context))
        expected-closed (-> ["persisted"
                             ---
                             "(+ 1 1⦇)⦈|"]
                            (derive-context))
        actual-open     (-> context (r/highlights) (:open-paren) (:region))
        actual-closed   (-> context (r/highlights) (:closed-paren) (:region))
        expected-open   (-> expected-open (r/highlights) (:manual) (:region))
        expected-closed (-> expected-closed (r/highlights) (:manual) (:region))]
    (is (= actual-open expected-open))
    (is (= actual-closed expected-closed))))

(deftest does-not-highlight-unmatched-parentheses
  (let [context (-> ["persisted"
                     ---
                     "(|+ 1"]
                    (derive-context)
                    (process [e/move-left]))
        actual-high (r/highlights context)]
    (is (= actual-high {}))))
