(ns omnia.context-test
  (:require [omnia.repl.view :as v]
            [omnia.repl.hud :as h]
            [omnia.repl.events :as e]
            [omnia.repl.context :as c]
            [clojure.test :refer [is deftest testing]]
            [omnia.test-utils :refer :all]))

;; I. Manipulation

(deftest replacing-main-view-refreshes-preview
  (let [hud        (-> ["existing input|"]
                       (derive-context)
                       (c/hud))
        enrichment (-> ["some input|"] (derive-text))
        actual     (-> hud
                       (h/switch-view (-> hud
                                          (h/persisted-view)
                                          (v/enrich-with [enrichment])))
                       (h/current-view))
        expected   (-> ["existing input"
                        "some text|"]
                       (derive-context)
                       (c/hud)
                       (h/current-view))]
    (= expected actual)))

(deftest clipboard-is-renewed
  (let [actual   (-> ["existing input"]
                     (derive-context)
                     (process [e/select-all e/copy (e/character \a) e/select-all e/copy])
                     (c/hud)
                     (h/input-area)
                     (:clipboard))
        expected (-> ["existing inputa"] (derive-text))]
    (is (= expected actual))))

(deftest clipboard-is-propagated
  (let [actual   (-> ["existing |input"]
                     (derive-context)
                     (process [e/select-all e/copy (e/character \a) (e/character \b)])
                     (c/hud)
                     (h/input-area)
                     (:clipboard))
        expected (-> ["existing input"] (derive-text))]
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
                             (process [e/scroll-up e/scroll-up e/scroll-up])
                             (c/hud))
        expected         (-> [-| "some"
                              -| "persisted"
                              -$ "area"
                              ---
                              -$ "existing"
                              -$ "input|"
                              -+ "area"]
                             (derive-context)
                             (c/hud))
        actual-preview   (-> context (h/current-view) (v/project) (:lines))
        expected-preview (-> expected (h/current-view) (v/project) (:lines))
        expected-cursor  (-> context (h/current-view) (v/text) (:cursor))
        actual-cursor    (-> expected (h/current-view) (v/text) (:cursor))
        actual-offset    (-> context (h/current-view) (v/scroll-offset))
        expected-offset  (-> expected (h/current-view) (v/scroll-offset))]
    (is (= actual-offset expected-offset 3))
    (is (= actual-preview expected-preview))
    (is (= actual-cursor expected-cursor))))

(deftest scrolls-down
  (let [hud              (-> ["some"
                              "persisted"
                              "area"
                              ---
                              -| "existing"
                              -| "input|"
                              -+ "area"]
                             (derive-context)
                             (process [e/scroll-up e/scroll-up e/scroll-up])
                             (c/hud))
        expected         (-> ["some"
                              "persisted"
                              -| "area"
                              ---
                              -| "existing"
                              -$ "input|"
                              -+ "area"]
                             (derive-context)
                             (c/hud))
        processed        (process-old hud [e/scroll-down e/scroll-down])
        actual-preview   (-> processed (h/current-view) (v/project) (:lines))
        expected-preview (-> expected (h/current-view) (v/project) (:lines))
        actual-cursor    (-> processed (h/current-view) (v/text) (:cursor))
        expected-cursor  (-> expected (h/current-view) (v/text) (:cursor))
        actual-offset    (-> processed (h/current-view) (v/scroll-offset))
        expected-offset  (-> expected (h/current-view) (v/scroll-offset))]
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
        init-offset     (-> context (c/hud) (h/current-view) (v/scroll-offset))
        scrolled-offset (-> scrolled (c/hud) (h/current-view) (v/scroll-offset))]
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
        actual-up-offset   (-> scrolled-up (c/hud) (h/current-view) (v/scroll-offset))
        actual-down-offset (-> scrolled-down (c/hud) (h/current-view) (v/scroll-offset))]
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
        reset               (-> scrolled (c/hud) (h/reset-scroll))
        init-scroll-offset  (-> scrolled (c/hud) (h/current-view) (v/scroll-offset))
        reset-scroll-offset (-> reset (h/current-view) (v/scroll-offset))]
    (is (= init-scroll-offset 2))
    (is (= reset-scroll-offset 0))))

;; III. Capturing

(deftest captures-input
  (let [context           (-> ["persisted"
                               ---
                               "some input|"]
                              (derive-context))
        expected          (-> ["persisted"
                               ---
                               "some inputa|"]
                              (derive-context))
        processed         (process context [(e/character \a)])
        actual-cursor     (-> processed (c/hud) (h/current-view) (v/text) (:cursor))
        actual-preview    (-> processed (c/hud) (h/current-view) (v/text) (:lines))
        actual-previous   (-> processed (c/hud) (h/previous-view) (v/text) (:lines))
        expected-cursor   (-> expected (c/hud) (h/current-view) (v/text) (:cursor))
        expected-preview  (-> expected (c/hud) (h/current-view) (v/text) (:lines))
        expected-previous (-> context (c/hud) (h/current-view) (v/text) (:lines))]
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
        actual-cursor     (-> processed (c/hud) (h/current-view) (v/text) (:cursor))
        actual-preview    (-> processed (c/hud) (h/current-view) (v/text) (:lines))
        actual-previous   (-> processed (c/hud) (h/previous-view) (v/text) (:lines))
        expected-cursor   (-> expected (c/hud) (h/current-view) (v/text) (:cursor))
        expected-preview  (-> expected (c/hud) (h/current-view) (v/text) (:lines))
        expected-previous (-> context (c/hud) (h/current-view) (v/text) (:lines))]
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
        expected-previous (-> context (c/hud) (h/current-view) (v/text) (:lines))
        expected-preview  (-> expected (c/hud) (h/current-view) (v/text) (:lines))
        expected-cursor   (-> expected (c/hud) (h/current-view) (v/text) (:cursor))
        actual-preview    (-> processed (c/hud) (h/current-view) (v/text) (:lines))
        actual-previous   (-> processed (c/hud) (h/previous-view) (v/text) (:lines))
        actual-cursor     (-> processed (c/hud) (h/current-view) (v/text) (:cursor))]
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
        expected1         (-> ["persisted"
                               ---
                               "past-eval-1|"]
                              (derive-context))
        expected2         (-> ["persisted"
                               ---
                               "past-eval-2|"]
                              (derive-context))
        processed1        (process context [e/prev-eval])
        processed2        (process context [e/prev-eval e/prev-eval])
        processed3        (process context [e/prev-eval e/prev-eval e/next-eval])
        actual-preview1   (-> processed1 (c/hud) (h/current-view) (v/text) (:lines))
        actual-cursor1    (-> processed1 (c/hud) (h/current-view) (v/text) (:cursor))
        expected-preview1 (-> expected1 (c/hud) (h/current-view) (v/text) (:lines))
        expected-cursor1  (-> expected1 (c/hud) (h/current-view) (v/text) (:cursor))

        actual-preview2   (-> processed2 (c/hud) (h/current-view) (v/text) (:lines))
        actual-cursor2    (-> processed2 (c/hud) (h/current-view) (v/text) (:cursor))
        expected-preview2 (-> expected2 (c/hud) (h/current-view) (v/text) (:lines))
        expected-cursor2  (-> expected2 (c/hud) (h/current-view) (v/text) (:cursor))

        actual-preview3   (-> processed3 (c/hud) (h/current-view) (v/text) (:lines))
        actual-cursor3    (-> processed3 (c/hud) (h/current-view) (v/text) (:cursor))]
    (is (= actual-preview1 expected-preview1))
    (is (= actual-cursor1 expected-cursor1))

    (is (= actual-preview2 expected-preview2))
    (is (= actual-cursor2 expected-cursor2))

    (is (= actual-preview3 expected-preview1))
    (is (= actual-cursor3 expected-cursor1))))

(deftest preserves-clipboard-during-evaluation-navigation
  (let [hud              (-> ["persisted"
                              ---
                              "some ⦇text⦈|"]
                             (derive-hud-old {:history ["prev-eval-1"
                                                        "prev-eval-2"]})
                             (process-old [e/copy
                                           e/prev-eval
                                           e/prev-eval
                                           e/paste]))
        expected         (-> ["persisted"
                              ---
                              "prev-eval-2text|"]
                             (derive-hud-old))
        actual-preview   (-> hud (h/current-view) (v/text) (:lines))
        actual-cursor    (-> hud (h/current-view) (v/text) (:cursor))
        expected-preview (-> expected (h/current-view) (v/text) (:lines))
        expected-cursor  (-> expected (h/current-view) (v/text) (:cursor))]
    (is (= actual-preview expected-preview))
    (is (= actual-cursor expected-cursor))))

;; VII. Suggesting

(deftest shows-suggestions
  (let [hud               (-> ["persisted"
                               ---
                               "s|ome things are"]
                              (derive-hud-old {:response
                                               (completion-response ["option-1"
                                                                     "option-2"])}))
        expected1         (-> ["persisted"
                               ---
                               "option-1 things are"
                               "------"
                               " option-1|"
                               " option-2"
                               "------"]
                              (derive-hud-old))
        expected2         (-> ["persisted"
                               ---
                               "option-2 things are"
                               "------"
                               " option-1"
                               " option-2|"
                               "------"]
                              (derive-hud-old))
        processed1        (process-old hud [e/suggest])
        processed2        (process-old hud [e/suggest e/suggest])

        actual1-preview   (-> processed1 (h/current-view) (v/text) (:lines))
        actual1-cursor    (-> processed1 (h/current-view) (v/text) (:cursor))

        actual2-preview   (-> processed2 (h/current-view) (v/text) (:lines))
        actual2-cursor    (-> processed2 (h/current-view) (v/text) (:cursor))

        expected1-preview (-> expected1 (h/current-view) (v/text) (:lines))
        expected1-cursor  (-> expected1 (h/current-view) (v/text) (:cursor))

        expected2-preview (-> expected2 (h/current-view) (v/text) (:lines))
        expected2-cursor  (-> expected2 (h/current-view) (v/text) (:cursor))]
    (is (= actual1-preview expected1-preview))
    (is (= actual2-preview expected2-preview))
    (is (= actual1-cursor expected1-cursor))
    (is (= actual2-cursor expected2-cursor))))

(deftest keeps-suggestion-upon-input
  (let [hud              (-> ["persisted"
                              ---
                              "1|"]
                             (derive-hud-old {:response
                                              (completion-response ["option-1"
                                                                    "option-2"])})
                             (process-old [e/suggest e/suggest (e/character \a)]))
        expected         (-> ["persisted"
                              ---
                              "option-2a|"]
                             (derive-hud-old))
        actual-preview   (-> hud (h/current-view) (v/text) (:lines))
        actual-cursor    (-> hud (h/current-view) (v/text) (:cursor))
        expected-preview (-> expected (h/current-view) (v/text) (:lines))
        expected-cursor  (-> expected (h/current-view) (v/text) (:cursor))]
    (is (= actual-preview expected-preview))
    (is (= actual-cursor expected-cursor))))

(deftest shows-empty-box-when-no-suggestions
  (let [hud              (-> ["persisted"
                              ---
                              "1|"]
                             (derive-hud-old {:response (completion-response [])})
                             (process-old [e/suggest]))
        expected         (-> ["persisted"
                              ---
                              "1"
                              "------|"
                              "------"]
                             (derive-hud-old))
        actual-preview   (-> hud (h/current-view) (v/text) (:lines))
        actual-cursor    (-> hud (h/current-view) (v/text) (:cursor))
        expected-preview (-> expected (h/current-view) (v/text) (:lines))
        expected-cursor  (-> expected (h/current-view) (v/text) (:cursor))]
    (is (= actual-preview expected-preview))
    (is (= actual-cursor expected-cursor))))

;; VIII. Highlighting

(deftest gc-same-line-selection
  (testing "Moving left"
    (let [result        (-> ["This| is a line"]
                            (derive-hud-old)
                            (process-old [e/select-left e/select-left e/select-right]))
          highlights    (-> ["Thi⦇s⦈ is a line"]
                            (derive-hud-old)
                            (h/highlights)
                            (:manual)
                            (:region))
          garbage       (-> ["Th⦇is⦈ is a line"]
                            (derive-hud-old)
                            (h/highlights)
                            (:manual)
                            (:region))
          expected-high (-> result (h/highlights) (:selection) (:region))
          expected-gc   (-> result (h/garbage) (:selection) (:region))]
      (is (= expected-high highlights) "Highlights mismatch")
      (is (= expected-gc garbage) "Garbage mismatch")))

  (testing "Moving right"
    (let [result        (-> ["|This is a line"]
                            (derive-hud-old)
                            (process-old [e/select-right e/select-right e/select-left]))
          highlights    (-> ["⦇T⦈his is a line"]
                            (derive-hud-old)
                            (h/highlights)
                            (:manual)
                            (:region))
          garbage       (-> ["⦇Th⦈is is a line"]
                            (derive-hud-old)
                            (h/highlights)
                            (:manual)
                            (:region))
          expected-high (-> result (h/highlights) (:selection) (:region))
          expected-gc   (-> result (h/garbage) (:selection) (:region))]
      (is (= expected-high highlights) "Highlights mismatch")
      (is (= expected-gc garbage) "Garbage mismatch"))))

(deftest gc-multi-line-selection
  (testing "Moving up"
    (let [result        (-> ["This is"
                             "a |large"
                             "piece of text"]
                            (derive-hud-old)
                            (process-old [e/select-all e/select-up e/select-up]))
          highlights    (-> ["⦇This is⦈"
                             "a large"
                             "piece of text"]
                            (derive-hud-old)
                            (h/highlights)
                            (:manual)
                            (:region))
          garbage       (-> ["⦇This is"
                             "a large⦈"
                             "piece of text"]
                            (derive-hud-old)
                            (h/highlights)
                            (:manual)
                            (:region))
          expected-high (-> result (h/highlights) (:selection) (:region))
          expected-gc   (-> result (h/garbage) (:selection) (:region))]
      (is (= expected-high highlights) "Highlights mismatch")
      (is (= expected-gc garbage) "Garbage mismatch")))

  (testing "Moving up left"
    (let [result        (-> ["This is"
                             "a |large"
                             "piece of text"]
                            (derive-hud-old)
                            (process-old [e/select-all e/select-up e/select-up e/select-left]))
          highlights    (-> ["⦇This i⦈s"
                             "a large"
                             "piece of text"]
                            (derive-hud-old)
                            (h/highlights)
                            (:manual)
                            (:region))
          garbage       (-> ["⦇This is⦈"
                             "a large"
                             "piece of text"]
                            (derive-hud-old)
                            (h/highlights)
                            (:manual)
                            (:region))
          expected-high (-> result (h/highlights) (:selection) (:region))
          expected-gc   (-> result (h/garbage) (:selection) (:region))]
      (is (= expected-high highlights) "Highlights mismatch")
      (is (= expected-gc garbage) "Garbage mismatch")))

  (testing "Moving up right"
    (let [result        (-> ["This is"
                             "a |large"
                             "piece of text"]
                            (derive-hud-old)
                            (process-old [e/select-all e/select-up e/select-up e/select-right]))
          highlights    (-> ["⦇This is"
                             "⦈a large"
                             "piece of text"]
                            (derive-hud-old)
                            (h/highlights)
                            (:manual)
                            (:region))
          garbage       (-> ["⦇This is⦈"
                             "a large"
                             "piece of text"]
                            (derive-hud-old)
                            (h/highlights)
                            (:manual)
                            (:region))
          expected-high (-> result (h/highlights) (:selection) (:region))
          expected-gc   (-> result (h/garbage) (:selection) (:region))]
      (is (= expected-high highlights) "Highlights mismatch")
      (is (= expected-gc garbage) "Garbage mismatch")))

  (testing "Moving down"
    (let [result        (-> ["This is"
                             "a |large"
                             "piece of text"]
                            (derive-hud-old)
                            (process-old [e/select-all e/select-up e/select-up e/select-down]))
          highlights    (-> ["⦇This is"
                             "a large⦈"
                             "piece of text"]
                            (derive-hud-old)
                            (h/highlights)
                            (:manual)
                            (:region))
          garbage       (-> ["⦇This is⦈"
                             "a large"
                             "piece of text"]
                            (derive-hud-old)
                            (h/highlights)
                            (:manual)
                            (:region))
          expected-high (-> result (h/highlights) (:selection) (:region))
          expected-gc   (-> result (h/garbage) (:selection) (:region))]
      (is (= expected-high highlights) "Highlights mismatch")
      (is (= expected-gc garbage) "Garbage mismatch")))

  (testing "Moving down left"
    (let [result        (-> ["This is"
                             "a |large"
                             "piece of text"]
                            (derive-hud-old)
                            (process-old [e/select-all e/select-up e/select-up e/select-down e/select-left]))
          highlights    (-> ["⦇This is"
                             "a larg⦈e"
                             "piece of text"]
                            (derive-hud-old)
                            (h/highlights)
                            (:manual)
                            (:region))
          garbage       (-> ["⦇This is"
                             "a large⦈"
                             "piece of text"]
                            (derive-hud-old)
                            (h/highlights)
                            (:manual)
                            (:region))
          expected-high (-> result (h/highlights) (:selection) (:region))
          expected-gc   (-> result (h/garbage) (:selection) (:region))]
      (is (= expected-high highlights) "Highlights mismatch")
      (is (= expected-gc garbage) "Garbage mismatch")))

  (testing "Moving down right"
    (let [result        (-> ["This is"
                             "a |large"
                             "piece of text"]
                            (derive-hud-old)
                            (process-old [e/select-all e/select-up e/select-up e/select-down e/select-right]))
          highlights    (-> ["⦇This is"
                             "a large"
                             "⦈piece of text"]
                            (derive-hud-old)
                            (h/highlights)
                            (:manual)
                            (:region))
          garbage       (-> ["⦇This is"
                             "a large⦈"
                             "piece of text"]
                            (derive-hud-old)
                            (h/highlights)
                            (:manual)
                            (:region))
          expected-high (-> result (h/highlights) (:selection) (:region))
          expected-gc   (-> result (h/garbage) (:selection) (:region))]
      (is (= expected-high highlights) "Highlights mismatch")
      (is (= expected-gc garbage) "Garbage mismatch"))))

(deftest gc-reset
  (testing "Multiple lines"
    (let [result        (-> ["Some |piece of text"
                             "with lines"]
                            (derive-hud-old)
                            (process-old [e/select-all e/move-right]))
          garbage       (-> ["⦇Some piece of text"
                             "with lines⦈"]
                            (derive-hud-old)
                            (h/highlights)
                            (:manual)
                            (:region))
          expected-high (-> result (h/highlights) (:selection) (:region))
          expected-gc   (-> result (h/garbage) (:selection) (:region))]
      (is (= expected-high nil) "Highlights mismatch")
      (is (= expected-gc garbage) "Garbage mismatch")))

  (testing "Same line"
    (testing "Multiple lines"
      (let [result        (-> ["Some |piece of text"]
                              (derive-hud-old)
                              (process-old [e/select-right e/select-right e/move-right]))
            garbage       (-> ["Some ⦇pi⦈ece of text"]
                              (derive-hud-old)
                              (h/highlights)
                              (:manual)
                              (:region))
            expected-high (-> result (h/highlights) (:selection) (:region))
            expected-gc   (-> result (h/garbage) (:selection) (:region))]
        (is (= expected-high nil) "Highlights mismatch")
        (is (= expected-gc garbage) "Garbage mismatch")))))

;; IX. Parenthesis matching

(deftest matches-parentheses
  (let [hud             (-> ["persisted"
                             ---
                             "(+ 1 1|)"]
                            (derive-hud-old)
                            (process-old [e/move-right]))
        expected-open   (-> ["persisted"
                             ---
                             "⦇(⦈+ 1 1)|"]
                            (derive-hud-old))
        expected-closed (-> ["persisted"
                             ---
                             "(+ 1 1⦇)⦈|"]
                            (derive-hud-old))
        actual-open     (-> hud (h/highlights) (:open-paren) (:region))
        actual-closed   (-> hud (h/highlights) (:closed-paren) (:region))
        expected-open   (-> expected-open (h/highlights) (:manual) (:region))
        expected-closed (-> expected-closed (h/highlights) (:manual) (:region))]
    (is (= actual-open expected-open))
    (is (= actual-closed expected-closed))))

(deftest does-not-highlight-unmatched-parentheses
  (let [hud         (-> ["persisted"
                         ---
                         "(|+ 1"]
                        (derive-hud-old)
                        (process-old [e/move-left]))
        actual-high (h/highlights hud)]
    (is (= actual-high {}))))

;; X. Injecting

(deftest injects-code
  (let [hud             (-> ["persisted"
                             ---
                             "some text|"]
                            (derive-hud-old {:value-response (value-response "pong")})
                            (process-old [(e/inject "ping")]))
        expected        (-> ["persisted"
                             ---
                             "some text|"
                             "pong"]
                            (derive-hud-old))
        expected-view   (-> expected (h/current-view) (:lines))
        actual-view     (-> hud (h/current-view) (:lines))
        expected-cursor (-> expected (h/current-view) (:cursor))
        actual-cursor   (-> hud (h/current-view) (:cursor))]
    (is (= expected-view actual-view))
    (is (= expected-cursor actual-cursor))))
