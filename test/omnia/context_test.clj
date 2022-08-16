(ns omnia.context-test
  (:require [omnia.repl.text :as t]
            [omnia.repl.view :as v]
            [omnia.repl.hud :as h]
            [omnia.repl.events :as e]
            [omnia.repl.context :as c]
            [clojure.test :refer [is deftest testing]]
            [omnia.test-utils :refer :all]
            [omnia.repl.docs :as d]
            [omnia.repl.store :as st]))

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
                             (process [e/scroll-up e/scroll-up e/scroll-up]))
        expected         (-> [-| "some"
                              -| "persisted"
                              -$ "area"
                              ---
                              -$ "existing"
                              -$ "input|"
                              -+ "area"]
                             (derive-context))
        actual-preview   (-> context (c/hud) (h/current-view) (v/project) (:lines))
        expected-preview (-> expected (c/hud) (h/current-view) (v/project) (:lines))
        expected-cursor  (-> context (c/hud) (h/current-view) (v/text) (:cursor))
        actual-cursor    (-> expected (c/hud) (h/current-view) (v/text) (:cursor))
        actual-offset    (-> context (c/hud) (h/current-view) (v/scroll-offset))
        expected-offset  (-> expected (c/hud) (h/current-view) (v/scroll-offset))]
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
                             (process [e/scroll-up e/scroll-up e/scroll-up]))
        expected         (-> ["some"
                              "persisted"
                              -| "area"
                              ---
                              -| "existing"
                              -$ "input|"
                              -+ "area"]
                             (derive-context))
        processed        (process context [e/scroll-down e/scroll-down])
        actual-preview   (-> processed (c/hud) (h/current-view) (v/project) (:lines))
        expected-preview (-> expected (c/hud) (h/current-view) (v/project) (:lines))
        actual-cursor    (-> processed (c/hud) (h/current-view) (v/text) (:cursor))
        expected-cursor  (-> expected (c/hud) (h/current-view) (v/text) (:cursor))
        actual-offset    (-> processed (c/hud) (h/current-view) (v/scroll-offset))
        expected-offset  (-> expected (c/hud) (h/current-view) (v/scroll-offset))]
    (is (= actual-offset expected-offset 1))
    (is (= actual-preview expected-preview))
    (is (= actual-cursor expected-cursor))))

(deftest stops-scrolling-up-at-bounds
  (let [context                (-> ["some"
                                    "persisted"
                                    "area"
                                    ---
                                    -| "existing"
                                    -| "input|"
                                    -+ "area"]
                                   (derive-context))
        scrolled               (process context (repeat 100 e/scroll-up))
        init-offset            (-> context (c/hud) (h/current-view) (v/scroll-offset))
        scrolled-offset        (-> scrolled (c/hud) (h/current-view) (v/scroll-offset))
        expected-init-offset   0
        expected-scroll-offset (+ (t/size h/header) 3)]
    (is (= init-offset expected-init-offset))
    (is (= scrolled-offset expected-scroll-offset))))

(deftest stops-scrolling-down-at-bounds
  (let [context              (-> ["some"
                                  "persisted"
                                  "area"
                                  ---
                                  -| "existing"
                                  -| "input|"
                                  -+ "area"]
                                 (derive-context))
        scrolled-up          (process context (repeat 100 e/scroll-up))
        scrolled-down        (process context (repeat 100 e/scroll-down))
        actual-up-offset     (-> scrolled-up (c/hud) (h/current-view) (v/scroll-offset))
        actual-down-offset   (-> scrolled-down (c/hud) (h/current-view) (v/scroll-offset))
        expected-up-offset   (+ (t/size h/header) 3)
        expected-down-offset 0]
    (is (= actual-up-offset expected-up-offset))
    (is (= actual-down-offset expected-down-offset))))

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
  (let [context               (-> ["persisted"
                                   ---
                                   "(+ 1 1)|"]
                                  (derive-context))
        processed             (process context
                                       [e/evaluate]
                                       {:response (value-response "2")})
        expected              (-> ["persisted"
                                   "(+ 1 1)"
                                   ""
                                   "2"
                                   ""
                                   "Ω =>"
                                   ---
                                   "|"]
                                  (derive-context))
        expected-eval-history (-> ["(+ 1 1)|"]
                                  (derive-text))
        expected-previous     (-> context (c/hud) (h/current-view) (v/text) (:lines))
        expected-preview      (-> expected (c/hud) (h/current-view) (v/text) (:lines))
        expected-cursor       (-> expected (c/hud) (h/current-view) (v/text) (:cursor))
        actual-preview        (-> processed (c/hud) (h/current-view) (v/text) (:lines))
        actual-previous       (-> processed (c/hud) (h/previous-view) (v/text) (:lines))
        actual-cursor         (-> processed (c/hud) (h/current-view) (v/text) (:cursor))
        actual-eval-history   (-> processed (c/store) (st/eval-history) (st/timeframe))]
    (is (= actual-preview expected-preview))
    (is (= actual-cursor expected-cursor))
    (is (= actual-previous expected-previous))
    (is (= actual-eval-history [expected-eval-history]))))

;; VI. Previous and next evaluations

(deftest navigates-through-evaluation-history
  (let [context           (-> ["persisted"
                               ---
                               "(+ 1 1)|"]
                              (derive-context {:eval-history ["past-ev|al-1" "past|-eval-2"]}))
        expected1         (-> ["persisted"
                               ---
                               "past|-eval-2"]
                              (derive-context))
        expected2         (-> ["persisted"
                               ---
                               "past-ev|al-1"]
                              (derive-context))
        processed1        (process context [e/prev-eval])
        processed2        (process context [e/prev-eval e/prev-eval])
        processed3        (process context [e/prev-eval e/prev-eval e/next-eval])
        processed4        (process context [e/prev-eval e/next-eval])
        actual-preview1   (-> processed1 (c/hud) (h/current-view) (v/text) (:lines))
        actual-cursor1    (-> processed1 (c/hud) (h/current-view) (v/text) (:cursor))
        expected-preview1 (-> expected1 (c/hud) (h/current-view) (v/text) (:lines))
        expected-cursor1  (-> expected1 (c/hud) (h/current-view) (v/text) (:cursor))

        actual-preview2   (-> processed2 (c/hud) (h/current-view) (v/text) (:lines))
        actual-cursor2    (-> processed2 (c/hud) (h/current-view) (v/text) (:cursor))
        expected-preview2 (-> expected2 (c/hud) (h/current-view) (v/text) (:lines))
        expected-cursor2  (-> expected2 (c/hud) (h/current-view) (v/text) (:cursor))

        actual-preview3   (-> processed3 (c/hud) (h/current-view) (v/text) (:lines))
        actual-cursor3    (-> processed3 (c/hud) (h/current-view) (v/text) (:cursor))

        actual-preview4   (-> processed4 (c/hud) (h/current-view) (v/text) (:lines))
        actual-cursor4    (-> processed4 (c/hud) (h/current-view) (v/text) (:cursor))
        expected-preview4 (-> context (c/hud) (h/current-view) (v/text) (:lines))
        expected-cursor4  (-> context (c/hud) (h/current-view) (v/text) (:cursor))]
    (is (= actual-preview1 expected-preview1))
    (is (= actual-cursor1 expected-cursor1))

    (is (= actual-preview2 expected-preview2))
    (is (= actual-cursor2 expected-cursor2))

    (is (= actual-preview3 expected-preview1))
    (is (= actual-cursor3 expected-cursor1))

    (is (= actual-preview4 expected-preview4))
    (is (= actual-cursor4 expected-cursor4))))

(deftest resets-navigation-through-evaluation-history
  (let [context           (-> ["persisted"
                               ---
                               "(+ 1 1)|"]
                              (derive-context {:eval-history ["past-eval-1|" "past-|eval-2"]}))
        expected1         (-> ["persisted"
                               ---
                               "past-eval-1a|"]
                              (derive-context))
        expected2         (-> ["persisted"
                               ---
                               "past-|eval-2"]
                              (derive-context))
        processed1        (process context [e/prev-eval e/prev-eval (e/character \a)])
        processed2        (process processed1 [e/prev-eval])
        actual-preview1   (-> processed1 (c/hud) (h/current-view) (v/text) (:lines))
        actual-cursor1    (-> processed1 (c/hud) (h/current-view) (v/text) (:cursor))
        expected-preview1 (-> expected1 (c/hud) (h/current-view) (v/text) (:lines))
        expected-cursor1  (-> expected1 (c/hud) (h/current-view) (v/text) (:cursor))

        actual-preview2   (-> processed2 (c/hud) (h/current-view) (v/text) (:lines))
        actual-cursor2    (-> processed2 (c/hud) (h/current-view) (v/text) (:cursor))
        expected-preview2 (-> expected2 (c/hud) (h/current-view) (v/text) (:lines))
        expected-cursor2  (-> expected2 (c/hud) (h/current-view) (v/text) (:cursor))]
    (is (= actual-preview1 expected-preview1))
    (is (= actual-cursor1 expected-cursor1))

    (is (= actual-preview2 expected-preview2))
    (is (= actual-cursor2 expected-cursor2))))

(deftest preserves-clipboard-during-evaluation-navigation
  (let [context          (-> ["persisted"
                              ---
                              "some ⦇text⦈|"]
                             (derive-context {:eval-history ["prev-eval-1|" "prev-eval-2|"]})
                             (process [e/copy
                                       e/prev-eval
                                       e/prev-eval
                                       e/paste]))
        expected         (-> ["persisted"
                              ---
                              "prev-eval-1text|"]
                             (derive-context))
        actual-preview   (-> context (c/hud) (h/current-view) (v/text) (:lines))
        actual-cursor    (-> context (c/hud) (h/current-view) (v/text) (:cursor))
        expected-preview (-> expected (c/hud) (h/current-view) (v/text) (:lines))
        expected-cursor  (-> expected (c/hud) (h/current-view) (v/text) (:cursor))]
    (is (= actual-preview expected-preview))
    (is (= actual-cursor expected-cursor))))

;; VII. Suggesting

(deftest shows-suggestions
  (let [context               (-> ["persisted"
                                   ---
                                   "s|ome things are"]
                                  (derive-context))
        expected1             (-> ["persisted"
                                   ---
                                   "option-1 things are"
                                   "------"
                                   " option-1|"
                                   " option-2"
                                   "------"]
                                  (derive-context))
        expected2             (-> ["persisted"
                                   ---
                                   "option-2 things are"
                                   "------"
                                   " option-1"
                                   " option-2|"
                                   "------"]
                                  (derive-context))

        expected1-options     (-> ["option-1|"
                                   "option-2"]
                                  (derive-text))
        expected2-options     (-> ["option-1"
                                   "option-2|"]
                                  (derive-text))

        processed1            (process context
                                       [e/suggest]
                                       {:response (completion-response ["option-1" "option-2"])})
        processed2            (process context
                                       [e/suggest e/suggest]
                                       {:response (completion-response ["option-1" "option-2"])})
        actual1-suggestion    (-> processed1 (c/docs) (d/suggestions) (v/text))
        actual1-documentation (-> processed1 (c/docs) (d/documentation))
        actual1-signatures    (-> processed1 (c/docs) (d/signatures))
        actual1-preview       (-> processed1 (c/hud) (h/current-view) (v/text) (:lines))
        actual1-cursor        (-> processed1 (c/hud) (h/current-view) (v/text) (:cursor))

        actual2-suggestion    (-> processed2 (c/docs) (d/suggestions) (v/text))
        actual2-documentation (-> processed1 (c/docs) (d/documentation))
        actual2-signatures    (-> processed1 (c/docs) (d/signatures))
        actual2-preview       (-> processed2 (c/hud) (h/current-view) (v/text) (:lines))
        actual2-cursor        (-> processed2 (c/hud) (h/current-view) (v/text) (:cursor))

        expected1-preview     (-> expected1 (c/hud) (h/current-view) (v/text) (:lines))
        expected1-cursor      (-> expected1 (c/hud) (h/current-view) (v/text) (:cursor))

        expected2-preview     (-> expected2 (c/hud) (h/current-view) (v/text) (:lines))
        expected2-cursor      (-> expected2 (c/hud) (h/current-view) (v/text) (:cursor))]
    (is (= actual1-preview expected1-preview))
    (is (= actual2-preview expected2-preview))
    (is (= actual1-cursor expected1-cursor))
    (is (= actual2-cursor expected2-cursor))
    (is (= actual1-suggestion expected1-options))
    (is (= actual2-suggestion expected2-options))
    (is (= actual1-documentation nil))
    (is (= actual1-signatures nil))
    (is (= actual2-documentation nil))
    (is (= actual2-signatures nil))))

(deftest keeps-suggestion-upon-input
  (let [context          (-> ["persisted"
                              ---
                              "1|"]
                             (derive-context)
                             (process
                               [e/suggest e/suggest (e/character \a)]
                               {:response (completion-response ["option-1" "option-2"])}))
        expected         (-> ["persisted"
                              ---
                              "option-2a|"]
                             (derive-context))
        actual-options   (-> context (c/docs) (d/suggestions))
        actual-preview   (-> context (c/hud) (h/current-view) (v/text) (:lines))
        actual-cursor    (-> context (c/hud) (h/current-view) (v/text) (:cursor))
        expected-preview (-> expected (c/hud) (h/current-view) (v/text) (:lines))
        expected-cursor  (-> expected (c/hud) (h/current-view) (v/text) (:cursor))]
    (is (= actual-preview expected-preview))
    (is (= actual-cursor expected-cursor))
    (is (= actual-options nil))))

(deftest shows-empty-box-when-no-suggestions
  (let [context          (-> ["persisted"
                              ---
                              "1|"]
                             (derive-context)
                             (process
                               [e/suggest]
                               {:response (completion-response [])}))
        expected         (-> ["persisted"
                              ---
                              "1"
                              "------|"
                              "------"]
                             (derive-context))
        expected-options (-> []
                             (derive-text))
        actual-options   (-> context (c/docs) (d/suggestions) (v/text))
        actual-preview   (-> context (c/hud) (h/current-view) (v/text) (:lines))
        actual-cursor    (-> context (c/hud) (h/current-view) (v/text) (:cursor))
        expected-preview (-> expected (c/hud) (h/current-view) (v/text) (:lines))
        expected-cursor  (-> expected (c/hud) (h/current-view) (v/text) (:cursor))]
    (is (= actual-preview expected-preview))
    (is (= actual-cursor expected-cursor))
    (is (= actual-options expected-options))))

;; VIII. Documentation

(deftest shows-documentation
  (let [context               (-> ["persisted"
                                   ---
                                   "s|ome things are"]
                                  (derive-context))
        expected1             (-> ["persisted"
                                   ---
                                   "some things are"
                                   "------"
                                   " line-1 with text|"
                                   " line-2 with text"
                                   "------"]
                                  (derive-context))
        expected2             (-> ["persisted"
                                   ---
                                   "some things are"
                                   "------"
                                   " line-1 with text"
                                   " line-2 with text|"
                                   "------"]
                                  (derive-context))

        expected1-info        (-> ["line-1 with text|"
                                   "line-2 with text"]
                                  (derive-text))
        expected2-info        (-> ["line-1 with text"
                                   "line-2 with text|"]
                                  (derive-text))

        processed1            (process context
                                       [e/docs]
                                       {:response (doc-response "line-1 with text\nline-2 with text")})
        processed2            (process context
                                       [e/docs e/docs]
                                       {:response (doc-response "line-1 with text\nline-2 with text")})
        actual1-documentation (-> processed1 (c/docs) (d/documentation) (v/text))
        actual1-suggestion    (-> processed1 (c/docs) (d/suggestions))
        actual1-signatures    (-> processed1 (c/docs) (d/signatures))
        actual1-preview       (-> processed1 (c/hud) (h/current-view) (v/text) (:lines))
        actual1-cursor        (-> processed1 (c/hud) (h/current-view) (v/text) (:cursor))

        actual2-documentation (-> processed2 (c/docs) (d/documentation) (v/text))
        actual2-suggestion    (-> processed1 (c/docs) (d/suggestions))
        actual2-signatures    (-> processed1 (c/docs) (d/signatures))
        actual2-preview       (-> processed2 (c/hud) (h/current-view) (v/text) (:lines))
        actual2-cursor        (-> processed2 (c/hud) (h/current-view) (v/text) (:cursor))

        expected1-preview     (-> expected1 (c/hud) (h/current-view) (v/text) (:lines))
        expected1-cursor      (-> expected1 (c/hud) (h/current-view) (v/text) (:cursor))

        expected2-preview     (-> expected2 (c/hud) (h/current-view) (v/text) (:lines))
        expected2-cursor      (-> expected2 (c/hud) (h/current-view) (v/text) (:cursor))]
    (is (= actual1-preview expected1-preview))
    (is (= actual2-preview expected2-preview))
    (is (= actual1-cursor expected1-cursor))
    (is (= actual2-cursor expected2-cursor))
    (is (= actual1-documentation expected1-info))
    (is (= actual2-documentation expected2-info))
    (is (= actual1-suggestion nil))
    (is (= actual1-signatures nil))
    (is (= actual2-suggestion nil))
    (is (= actual2-signatures nil))))

(deftest shows-empty-box-when-no-documentation
  (let [context          (-> ["persisted"
                              ---
                              "1|"]
                             (derive-context)
                             (process
                               [e/docs]
                               {:response (doc-response "")}))
        expected         (-> ["persisted"
                              ---
                              "1"
                              "------|"
                              "------"]
                             (derive-context))
        expected-info    (-> []
                             (derive-text))
        actual-info      (-> context (c/docs) (d/documentation) (v/text))
        actual-preview   (-> context (c/hud) (h/current-view) (v/text) (:lines))
        actual-cursor    (-> context (c/hud) (h/current-view) (v/text) (:cursor))
        expected-preview (-> expected (c/hud) (h/current-view) (v/text) (:lines))
        expected-cursor  (-> expected (c/hud) (h/current-view) (v/text) (:cursor))]
    (is (= actual-preview expected-preview))
    (is (= actual-cursor expected-cursor))
    (is (= actual-info expected-info))))

;; IX. Signatures

(deftest shows-signatures
  (let [context               (-> ["persisted"
                                   ---
                                   "s|ome things are"]
                                  (derive-context))
        expected1             (-> ["persisted"
                                   ---
                                   "some things are"
                                   "------"
                                   " ns/name signature-1|"
                                   " ns/name signature-2"
                                   "------"]
                                  (derive-context))
        expected2             (-> ["persisted"
                                   ---
                                   "some things are"
                                   "------"
                                   " ns/name signature-1"
                                   " ns/name signature-2|"
                                   "------"]
                                  (derive-context))

        expected1-options     (-> ["ns/name signature-1|"
                                   "ns/name signature-2"]
                                  (derive-text))
        expected2-options     (-> ["ns/name signature-1"
                                   "ns/name signature-2|"]
                                  (derive-text))

        processed1            (process context
                                       [e/signature]
                                       {:response (argument-response "ns" "name" ["signature-1" "signature-2"])})
        processed2            (process context
                                       [e/signature e/signature]
                                       {:response (argument-response "ns" "name" ["signature-1" "signature-2"])})
        actual1-signatures    (-> processed1 (c/docs) (d/signatures) (v/text))
        actual1-suggestion    (-> processed1 (c/docs) (d/suggestions))
        actual1-documentation (-> processed1 (c/docs) (d/documentation))
        actual1-preview       (-> processed1 (c/hud) (h/current-view) (v/text) (:lines))
        actual1-cursor        (-> processed1 (c/hud) (h/current-view) (v/text) (:cursor))

        actual2-signatures    (-> processed2 (c/docs) (d/signatures) (v/text))
        actual2-suggestion    (-> processed1 (c/docs) (d/suggestions))
        actual2-documentation (-> processed1 (c/docs) (d/documentation))
        actual2-preview       (-> processed2 (c/hud) (h/current-view) (v/text) (:lines))
        actual2-cursor        (-> processed2 (c/hud) (h/current-view) (v/text) (:cursor))

        expected1-preview     (-> expected1 (c/hud) (h/current-view) (v/text) (:lines))
        expected1-cursor      (-> expected1 (c/hud) (h/current-view) (v/text) (:cursor))

        expected2-preview     (-> expected2 (c/hud) (h/current-view) (v/text) (:lines))
        expected2-cursor      (-> expected2 (c/hud) (h/current-view) (v/text) (:cursor))]
    (is (= actual1-preview expected1-preview))
    (is (= actual2-preview expected2-preview))
    (is (= actual1-cursor expected1-cursor))
    (is (= actual2-cursor expected2-cursor))
    (is (= actual1-signatures expected1-options))
    (is (= actual2-signatures expected2-options))
    (is (= actual1-suggestion nil))
    (is (= actual1-documentation nil))
    (is (= actual2-suggestion nil))
    (is (= actual2-documentation nil))))

(deftest shows-empty-string-when-no-signatures
  (let [context          (-> ["persisted"
                              ---
                              "1|"]
                             (derive-context)
                             (process
                               [e/signature]
                               {:response (argument-response "ns" "name" [])}))
        expected         (-> ["persisted"
                              ---
                              "1"
                              "------|"
                              " ns/name |"
                              "------"]
                             (derive-context))
        expected-options (-> ["ns/name |"]
                             (derive-text))
        actual-options   (-> context (c/docs) (d/signatures) (v/text))
        actual-preview   (-> context (c/hud) (h/current-view) (v/text) (:lines))
        actual-cursor    (-> context (c/hud) (h/current-view) (v/text) (:cursor))
        expected-preview (-> expected (c/hud) (h/current-view) (v/text) (:lines))
        expected-cursor  (-> expected (c/hud) (h/current-view) (v/text) (:cursor))]
    (is (= actual-preview expected-preview))
    (is (= actual-cursor expected-cursor))
    (is (= actual-options expected-options))))


;; VIII. Highlighting

(deftest gc-same-line-selection
  (testing "Moving left"
    (let [result        (-> ["This| is a line"]
                            (derive-context)
                            (process [e/select-left e/select-left e/select-right]))
          highlights    (-> ["Thi⦇s⦈ is a line"]
                            (derive-context)
                            (c/hud)
                            (h/highlights)
                            (:manual)
                            (:region))
          garbage       (-> ["Th⦇is⦈ is a line"]
                            (derive-context)
                            (c/hud)
                            (h/highlights)
                            (:manual)
                            (:region))
          expected-high (-> result (c/hud) (h/highlights) (:selection) (:region))
          expected-gc   (-> result (c/hud) (h/garbage) (:selection) (:region))]
      (is (= expected-high highlights) "Highlights mismatch")
      (is (= expected-gc garbage) "Garbage mismatch")))

  (testing "Moving right"
    (let [result        (-> ["|This is a line"]
                            (derive-context)
                            (process [e/select-right e/select-right e/select-left]))
          highlights    (-> ["⦇T⦈his is a line"]
                            (derive-context)
                            (c/hud)
                            (h/highlights)
                            (:manual)
                            (:region))
          garbage       (-> ["⦇Th⦈is is a line"]
                            (derive-context)
                            (c/hud)
                            (h/highlights)
                            (:manual)
                            (:region))
          expected-high (-> result (c/hud) (h/highlights) (:selection) (:region))
          expected-gc   (-> result (c/hud) (h/garbage) (:selection) (:region))]
      (is (= expected-high highlights) "Highlights mismatch")
      (is (= expected-gc garbage) "Garbage mismatch"))))

(deftest gc-multi-line-selection
  (testing "Moving up"
    (let [result        (-> ["This is"
                             "a |large"
                             "piece of text"]
                            (derive-context)
                            (process [e/select-all e/select-up e/select-up]))
          highlights    (-> ["⦇This is⦈"
                             "a large"
                             "piece of text"]
                            (derive-context)
                            (c/hud)
                            (h/highlights)
                            (:manual)
                            (:region))
          garbage       (-> ["⦇This is"
                             "a large⦈"
                             "piece of text"]
                            (derive-context)
                            (c/hud)
                            (h/highlights)
                            (:manual)
                            (:region))
          expected-high (-> result (c/hud) (h/highlights) (:selection) (:region))
          expected-gc   (-> result (c/hud) (h/garbage) (:selection) (:region))]
      (is (= expected-high highlights) "Highlights mismatch")
      (is (= expected-gc garbage) "Garbage mismatch")))

  (testing "Moving up left"
    (let [result        (-> ["This is"
                             "a |large"
                             "piece of text"]
                            (derive-context)
                            (process [e/select-all e/select-up e/select-up e/select-left]))
          highlights    (-> ["⦇This i⦈s"
                             "a large"
                             "piece of text"]
                            (derive-context)
                            (c/hud)
                            (h/highlights)
                            (:manual)
                            (:region))
          garbage       (-> ["⦇This is⦈"
                             "a large"
                             "piece of text"]
                            (derive-context)
                            (c/hud)
                            (h/highlights)
                            (:manual)
                            (:region))
          expected-high (-> result (c/hud) (h/highlights) (:selection) (:region))
          expected-gc   (-> result (c/hud) (h/garbage) (:selection) (:region))]
      (is (= expected-high highlights) "Highlights mismatch")
      (is (= expected-gc garbage) "Garbage mismatch")))

  (testing "Moving up right"
    (let [result        (-> ["This is"
                             "a |large"
                             "piece of text"]
                            (derive-context)
                            (process [e/select-all e/select-up e/select-up e/select-right]))
          highlights    (-> ["⦇This is"
                             "⦈a large"
                             "piece of text"]
                            (derive-context)
                            (c/hud)
                            (h/highlights)
                            (:manual)
                            (:region))
          garbage       (-> ["⦇This is⦈"
                             "a large"
                             "piece of text"]
                            (derive-context)
                            (c/hud)
                            (h/highlights)
                            (:manual)
                            (:region))
          expected-high (-> result (c/hud) (h/highlights) (:selection) (:region))
          expected-gc   (-> result (c/hud) (h/garbage) (:selection) (:region))]
      (is (= expected-high highlights) "Highlights mismatch")
      (is (= expected-gc garbage) "Garbage mismatch")))

  (testing "Moving down"
    (let [result        (-> ["This is"
                             "a |large"
                             "piece of text"]
                            (derive-context)
                            (process [e/select-all e/select-up e/select-up e/select-down]))
          highlights    (-> ["⦇This is"
                             "a large⦈"
                             "piece of text"]
                            (derive-context)
                            (c/hud)
                            (h/highlights)
                            (:manual)
                            (:region))
          garbage       (-> ["⦇This is⦈"
                             "a large"
                             "piece of text"]
                            (derive-context)
                            (c/hud)
                            (h/highlights)
                            (:manual)
                            (:region))
          expected-high (-> result (c/hud) (h/highlights) (:selection) (:region))
          expected-gc   (-> result (c/hud) (h/garbage) (:selection) (:region))]
      (is (= expected-high highlights) "Highlights mismatch")
      (is (= expected-gc garbage) "Garbage mismatch")))

  (testing "Moving down left"
    (let [result        (-> ["This is"
                             "a |large"
                             "piece of text"]
                            (derive-context)
                            (process [e/select-all e/select-up e/select-up e/select-down e/select-left]))
          highlights    (-> ["⦇This is"
                             "a larg⦈e"
                             "piece of text"]
                            (derive-context)
                            (c/hud)
                            (h/highlights)
                            (:manual)
                            (:region))
          garbage       (-> ["⦇This is"
                             "a large⦈"
                             "piece of text"]
                            (derive-context)
                            (c/hud)
                            (h/highlights)
                            (:manual)
                            (:region))
          expected-high (-> result (c/hud) (h/highlights) (:selection) (:region))
          expected-gc   (-> result (c/hud) (h/garbage) (:selection) (:region))]
      (is (= expected-high highlights) "Highlights mismatch")
      (is (= expected-gc garbage) "Garbage mismatch")))

  (testing "Moving down right"
    (let [result        (-> ["This is"
                             "a |large"
                             "piece of text"]
                            (derive-context)
                            (process [e/select-all e/select-up e/select-up e/select-down e/select-right]))
          highlights    (-> ["⦇This is"
                             "a large"
                             "⦈piece of text"]
                            (derive-context)
                            (c/hud)
                            (h/highlights)
                            (:manual)
                            (:region))
          garbage       (-> ["⦇This is"
                             "a large⦈"
                             "piece of text"]
                            (derive-context)
                            (c/hud)
                            (h/highlights)
                            (:manual)
                            (:region))
          expected-high (-> result (c/hud) (h/highlights) (:selection) (:region))
          expected-gc   (-> result (c/hud) (h/garbage) (:selection) (:region))]
      (is (= expected-high highlights) "Highlights mismatch")
      (is (= expected-gc garbage) "Garbage mismatch"))))

(deftest gc-reset
  (testing "Multiple lines"
    (let [result        (-> ["Some |piece of text"
                             "with lines"]
                            (derive-context)
                            (process [e/select-all e/move-right]))
          garbage       (-> ["⦇Some piece of text"
                             "with lines⦈"]
                            (derive-context)
                            (c/hud)
                            (h/highlights)
                            (:manual)
                            (:region))
          expected-high (-> result (c/hud) (h/highlights) (:selection) (:region))
          expected-gc   (-> result (c/hud) (h/garbage) (:selection) (:region))]
      (is (= expected-high nil) "Highlights mismatch")
      (is (= expected-gc garbage) "Garbage mismatch")))

  (testing "Same line"
    (testing "Multiple lines"
      (let [result        (-> ["Some |piece of text"]
                              (derive-context)
                              (process [e/select-right e/select-right e/move-right]))
            garbage       (-> ["Some ⦇pi⦈ece of text"]
                              (derive-context)
                              (c/hud)
                              (h/highlights)
                              (:manual)
                              (:region))
            expected-high (-> result (c/hud) (h/highlights) (:selection) (:region))
            expected-gc   (-> result (c/hud) (h/garbage) (:selection) (:region))]
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
        actual-open     (-> context (c/hud) (h/highlights) (:open-paren) (:region))
        actual-closed   (-> context (c/hud) (h/highlights) (:closed-paren) (:region))
        expected-open   (-> expected-open (c/hud) (h/highlights) (:manual) (:region))
        expected-closed (-> expected-closed (c/hud) (h/highlights) (:manual) (:region))]
    (is (= actual-open expected-open))
    (is (= actual-closed expected-closed))))

(deftest does-not-highlight-unmatched-parentheses
  (let [context     (-> ["persisted"
                         ---
                         "(|+ 1"]
                        (derive-context)
                        (process [e/move-left]))
        actual-high (-> context (c/hud) (h/highlights))]
    (is (= actual-high {}))))

;; X. Injecting

(deftest injects-code
  (let [context         (-> ["persisted"
                             ---
                             "some text|"]
                            (derive-context)
                            (process
                              [(e/inject "ping")]
                              {:response (value-response "pong")}))
        expected        (-> ["persisted"
                             ---
                             ""
                             "pong"
                             ""
                             "Ω =>"
                             "some text|"]
                            (derive-context))
        expected-view   (-> expected (c/hud) (h/current-view) (v/text) (:lines))
        actual-view     (-> context (c/hud) (h/current-view) (v/text) (:lines))
        expected-cursor (-> expected (c/hud) (h/current-view) (v/text) (:cursor))
        actual-cursor   (-> context (c/hud) (h/current-view) (v/text) (:cursor))]
    (is (= expected-view actual-view))
    (is (= expected-cursor actual-cursor))))

;; XI. Event processing

(deftest processes-every-event
  (testing "Delete Previous"
    (let [actual          (-> ["text|"]
                              (derive-context)
                              (process [e/delete-previous]))
          expected        (-> ["tex|"]
                              (derive-context))
          expected-view   (-> expected (c/hud) (h/current-view) (v/text) (:lines))
          actual-view     (-> actual (c/hud) (h/current-view) (v/text) (:lines))
          expected-cursor (-> expected (c/hud) (h/current-view) (v/text) (:cursor))
          actual-cursor   (-> actual (c/hud) (h/current-view) (v/text) (:cursor))
          expected-undo   [(-> ["text|"] (derive-text))]
          actual-undo     (-> actual (c/store) (st/undo-history) (st/timeframe))
          expected-redo   []
          actual-redo     (-> actual (c/store) (st/redo-history) (st/timeframe))]
      (is (= expected-view actual-view))
      (is (= expected-cursor actual-cursor))
      (is (= expected-undo actual-undo))
      (is (= expected-redo actual-redo))))

  (testing "Delete Current"
    (let [actual          (-> ["tex|t"]
                              (derive-context)
                              (process [e/delete-current]))
          expected        (-> ["tex|"]
                              (derive-context))
          expected-view   (-> expected (c/hud) (h/current-view) (v/text) (:lines))
          actual-view     (-> actual (c/hud) (h/current-view) (v/text) (:lines))
          expected-cursor (-> expected (c/hud) (h/current-view) (v/text) (:cursor))
          actual-cursor   (-> actual (c/hud) (h/current-view) (v/text) (:cursor))
          expected-undo   [(-> ["tex|t"] (derive-text))]
          actual-undo     (-> actual (c/store) (st/undo-history) (st/timeframe))
          expected-redo   []
          actual-redo     (-> actual (c/store) (st/redo-history) (st/timeframe))]
      (is (= expected-view actual-view))
      (is (= expected-cursor actual-cursor))
      (is (= expected-undo actual-undo))
      (is (= expected-redo actual-redo))))

  (testing "Character"
    (let [actual          (-> ["text|"]
                              (derive-context)
                              (process [(e/character \a)]))
          expected        (-> ["texta|"]
                              (derive-context))
          expected-view   (-> expected (c/hud) (h/current-view) (v/text) (:lines))
          actual-view     (-> actual (c/hud) (h/current-view) (v/text) (:lines))
          expected-cursor (-> expected (c/hud) (h/current-view) (v/text) (:cursor))
          actual-cursor   (-> actual (c/hud) (h/current-view) (v/text) (:cursor))
          expected-undo   [(-> ["text|"] (derive-text))]
          actual-undo     (-> actual (c/store) (st/undo-history) (st/timeframe))
          expected-redo   []
          actual-redo     (-> actual (c/store) (st/redo-history) (st/timeframe))]
      (is (= expected-view actual-view))
      (is (= expected-cursor actual-cursor))
      (is (= expected-undo actual-undo))
      (is (= expected-redo actual-redo))))

  (testing "New Line"
    (let [actual          (-> ["text|"]
                              (derive-context)
                              (process [e/new-line]))
          expected        (-> ["text"
                               "|"]
                              (derive-context))
          expected-view   (-> expected (c/hud) (h/current-view) (v/text) (:lines))
          actual-view     (-> actual (c/hud) (h/current-view) (v/text) (:lines))
          expected-cursor (-> expected (c/hud) (h/current-view) (v/text) (:cursor))
          actual-cursor   (-> actual (c/hud) (h/current-view) (v/text) (:cursor))
          expected-undo   [(-> ["text|"] (derive-text))]
          actual-undo     (-> actual (c/store) (st/undo-history) (st/timeframe))
          expected-redo   []
          actual-redo     (-> actual (c/store) (st/redo-history) (st/timeframe))]
      (is (= expected-view actual-view))
      (is (= expected-cursor actual-cursor))
      (is (= expected-undo actual-undo))
      (is (= expected-redo actual-redo))))

  (testing "Cut"
    (let [actual          (-> ["t|ext"]
                              (derive-context)
                              (process [e/jump-select-right e/cut]))
          expected        (-> ["t|"]
                              (derive-context))
          expected-view   (-> expected (c/hud) (h/current-view) (v/text) (:lines))
          actual-view     (-> actual (c/hud) (h/current-view) (v/text) (:lines))
          expected-cursor (-> expected (c/hud) (h/current-view) (v/text) (:cursor))
          actual-cursor   (-> actual (c/hud) (h/current-view) (v/text) (:cursor))
          expected-undo   [(-> ["text|"] (derive-text))]
          actual-undo     (-> actual (c/store) (st/undo-history) (st/timeframe))
          expected-redo   []
          actual-redo     (-> actual (c/store) (st/redo-history) (st/timeframe))]
      (is (= expected-view actual-view))
      (is (= expected-cursor actual-cursor))
      (is (= expected-undo actual-undo))
      (is (= expected-redo actual-redo))))

  (testing "Paste"
    (let [actual          (-> ["t|ext"]
                              (derive-context)
                              (process [e/jump-select-right e/cut e/move-left e/paste]))
          expected        (-> ["ext|t"]
                              (derive-context))
          expected-view   (-> expected (c/hud) (h/current-view) (v/text) (:lines))
          actual-view     (-> actual (c/hud) (h/current-view) (v/text) (:lines))
          expected-cursor (-> expected (c/hud) (h/current-view) (v/text) (:cursor))
          actual-cursor   (-> actual (c/hud) (h/current-view) (v/text) (:cursor))
          expected-undo   [(-> ["|t"] (derive-text)) (-> ["text|"] (derive-text))]
          actual-undo     (-> actual (c/store) (st/undo-history) (st/timeframe))
          expected-redo   []
          actual-redo     (-> actual (c/store) (st/redo-history) (st/timeframe))]
      (is (= expected-view actual-view))
      (is (= expected-cursor actual-cursor))
      (is (= expected-undo actual-undo))
      (is (= expected-redo actual-redo))))

  (testing "Undo"
    (let [actual          (-> ["text|"]
                              (derive-context)
                              (process [e/delete-previous e/delete-previous e/undo]))
          expected        (-> ["tex|"]
                              (derive-context))
          expected-view   (-> expected (c/hud) (h/current-view) (v/text) (:lines))
          actual-view     (-> actual (c/hud) (h/current-view) (v/text) (:lines))
          expected-cursor (-> expected (c/hud) (h/current-view) (v/text) (:cursor))
          actual-cursor   (-> actual (c/hud) (h/current-view) (v/text) (:cursor))
          expected-undo   [(-> ["text|"] (derive-text))]
          actual-undo     (-> actual (c/store) (st/undo-history) (st/timeframe))
          expected-redo   [(-> ["te|"] (derive-text))]
          actual-redo     (-> actual (c/store) (st/redo-history) (st/timeframe))]
      (is (= expected-view actual-view))
      (is (= expected-cursor actual-cursor))
      (is (= expected-undo actual-undo))
      (is (= expected-redo actual-redo))))

  (testing "Empty-Undo"
    (let [actual          (-> ["text|"]
                              (derive-context)
                              (process [e/undo]))
          expected        (-> ["text|"]
                              (derive-context))
          expected-view   (-> expected (c/hud) (h/current-view) (v/text) (:lines))
          actual-view     (-> actual (c/hud) (h/current-view) (v/text) (:lines))
          expected-cursor (-> expected (c/hud) (h/current-view) (v/text) (:cursor))
          actual-cursor   (-> actual (c/hud) (h/current-view) (v/text) (:cursor))
          actual-undo     (-> actual (c/store) (st/undo-history) (st/timeframe))
          actual-redo     (-> actual (c/store) (st/redo-history) (st/timeframe))]
      (is (= expected-view actual-view))
      (is (= expected-cursor actual-cursor))
      (is (= [] actual-undo))
      (is (= [] actual-redo))))

  (testing "Redo"
    (let [actual          (-> ["text|"]
                              (derive-context)
                              (process [e/delete-previous e/delete-previous e/undo e/undo e/redo]))
          expected        (-> ["tex|"]
                              (derive-context))
          expected-view   (-> expected (c/hud) (h/current-view) (v/text) (:lines))
          actual-view     (-> actual (c/hud) (h/current-view) (v/text) (:lines))
          expected-cursor (-> expected (c/hud) (h/current-view) (v/text) (:cursor))
          actual-cursor   (-> actual (c/hud) (h/current-view) (v/text) (:cursor))
          expected-undo   [(-> ["text|"] (derive-text))]
          actual-undo     (-> actual (c/store) (st/undo-history) (st/timeframe))
          expected-redo   [(-> ["te|"] (derive-text))]
          actual-redo     (-> actual (c/store) (st/redo-history) (st/timeframe))]
      (is (= expected-view actual-view))
      (is (= expected-cursor actual-cursor))
      (is (= expected-undo actual-undo))
      (is (= expected-redo actual-redo))))

  (testing "Empty-Redo"
    (let [actual          (-> ["text|"]
                              (derive-context)
                              (process [e/redo]))
          expected        (-> ["text|"]
                              (derive-context))
          expected-view   (-> expected (c/hud) (h/current-view) (v/text) (:lines))
          actual-view     (-> actual (c/hud) (h/current-view) (v/text) (:lines))
          expected-cursor (-> expected (c/hud) (h/current-view) (v/text) (:cursor))
          actual-cursor   (-> actual (c/hud) (h/current-view) (v/text) (:cursor))
          actual-undo     (-> actual (c/store) (st/undo-history) (st/timeframe))
          actual-redo     (-> actual (c/store) (st/redo-history) (st/timeframe))]
      (is (= expected-view actual-view))
      (is (= expected-cursor actual-cursor))
      (is (= [] actual-undo))
      (is (= [] actual-redo))))

  (testing "Balanced Undo-Redos"
    (let [actual          (-> ["text|"]
                              (derive-context)
                              (process [e/delete-previous e/undo e/redo e/undo e/redo]))
          expected        (-> ["tex|"]
                              (derive-context))
          expected-view   (-> expected (c/hud) (h/current-view) (v/text) (:lines))
          actual-view     (-> actual (c/hud) (h/current-view) (v/text) (:lines))
          expected-cursor (-> expected (c/hud) (h/current-view) (v/text) (:cursor))
          actual-cursor   (-> actual (c/hud) (h/current-view) (v/text) (:cursor))
          actual-undo     (-> actual (c/store) (st/undo-history) (st/timeframe))
          expected-undo   [(-> ["text|"] (derive-text))]
          actual-redo     (-> actual (c/store) (st/redo-history) (st/timeframe))
          expected-redo   []]
      (is (= expected-view actual-view))
      (is (= expected-cursor actual-cursor))
      (is (= expected-undo actual-undo))
      (is (= expected-redo actual-redo)))))
