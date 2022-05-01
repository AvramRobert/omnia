(ns omnia.context-test
  (:require [schema.core :as s]
            [omnia.repl.hud :as h]
            [omnia.repl.context :as r]
            [omnia.repl.text :as i]
            [clojure.test :refer [is deftest testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :refer [for-all]]
            [omnia.util.generator :refer [one]]
            [omnia.test-utils :refer :all]
            [omnia.schema.context :refer [Context]]
            [omnia.schema.text :refer [Seeker]]
            [omnia.repl.events :as e]))

;; 0. Manipulation

(deftest replacing-main-hud-refreshes-preview
  (let [context    (-> ["existing input|"]
                       (derive-context))
        enrichment (i/from-tagged-strings ["some input|"])
        actual     (-> context
                       (r/with-hud (-> context
                                       (r/persisted-hud)
                                       (h/enrich-with [enrichment])))
                       (r/preview-hud))
        expected   (-> ["existing input"
                        "some text|"]
                       (derive-context)
                       (r/preview-hud))]
    (= expected actual)))

(deftest clipboard-is-renewed
  (let [actual   (-> ["existing input"]
                     (derive-context)
                     (process [e/select-all e/copy (e/character \a) e/select-all e/copy])
                     (r/input-area)
                     (:clipboard))
        expected (i/from-tagged-strings ["existing inputa"])]
    (is (= expected actual))))

(deftest clipboard-is-propagated
  (let [actual   (-> ["existing |input"]
                     (derive-context)
                     (process [e/select-all e/copy (e/character \a) (e/character \b)])
                     (r/input-area)
                     (:clipboard))
        expected (i/from-tagged-strings ["existing input"])]
    (is (= expected actual))))

;; I. Calibrating

(comment
  {:prefilled-size 5
   :view-size      27
   :text-area      (gen-text-area-of 29)}
  "Context params:
      => cannot change: 14%

         can change: 85%
         can view: 79%
         cannot view: 21%"

  "2 hidden lines => 10 total : 8 viewable : 2 unviewable")

(deftest calibrates-exceeding-upper-view
  (let [context           (-> ["persisted"
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
                              (derive-context))
        expected1         (-> ["persisted"
                               ---
                               "some"
                               -x-
                               "in|put"
                               "of"
                               "mine"
                               "that"
                               "is"
                               "this"
                               -x-
                               "long"]
                              (derive-context))
        expected2         (-> ["persisted"
                               ---
                               -x-
                               "so|me"
                               "input"
                               "of"
                               "mine"
                               "that"
                               "is"
                               -x-
                               "this"
                               "long"]
                              (derive-context))
        processed1        (-> context (process [e/move-up]))
        processed2        (-> context (process [e/move-up e/move-up]))
        processed3        (-> context (process [e/move-up e/move-up e/move-up]))
        actual-preview1   (-> processed1 (r/preview-hud) (h/project-hud) (:lines))
        actual-cursor1    (-> processed1 (r/preview-hud) (h/project-hud) (:cursor))
        actual-offset1    (-> processed1 (r/preview-hud) (h/view-offset))

        expected-preview1 (-> expected1 (r/preview-hud) (h/project-hud) (:lines))
        expected-cursor1  (-> expected1 (r/preview-hud) (h/project-hud) (:cursor))
        expected-offset1  (-> expected1 (r/preview-hud) (h/view-offset))

        actual-preview2   (-> processed2 (r/preview-hud) (h/project-hud) (:lines))
        actual-cursor2    (-> processed2 (r/preview-hud) (h/project-hud) (:cursor))
        actual-offset2    (-> processed2 (r/preview-hud) (h/view-offset))

        expected-preview2 (-> expected2 (r/preview-hud) (h/project-hud) (:lines))
        expected-cursor2  (-> expected2 (r/preview-hud) (h/project-hud) (:cursor))
        expected-offset2  (-> expected2 (r/preview-hud) (h/view-offset))

        actual-preview3   (-> processed3 (r/preview-hud) (h/project-hud) (:lines))
        actual-cursor3    (-> processed3 (r/preview-hud) (h/project-hud) (:cursor))
        actual-offset3    (-> processed3 (r/preview-hud) (h/view-offset))]
    (is (= actual-preview1 expected-preview1))
    (is (= actual-cursor1 expected-cursor1))
    (is (= actual-offset1 expected-offset1))

    (is (= actual-preview2 expected-preview2))
    (is (= actual-cursor2 expected-cursor2))
    (is (= actual-offset2 expected-offset2))

    (is (= actual-preview2 expected-preview2))
    (is (= actual-cursor2 expected-cursor2))
    (is (= actual-offset2 expected-offset2))

    (is (= actual-preview3 expected-preview2))
    (is (= actual-cursor3 expected-cursor2))
    (is (= actual-offset3 expected-offset2))))

(deftest calibrates-exceeding-lower-view
  (let [context           (-> ["persisted"
                               ---
                               -x-
                               "some"
                               "input"
                               "of"
                               "mine"
                               "that"
                               "is|"
                               -x-
                               "this"
                               "long"]
                              (derive-context))
        expected1         (-> ["persisted"
                               ---
                               "some"
                               -x-
                               "input"
                               "of"
                               "mine"
                               "that"
                               "is"
                               "th|is"
                               -x-
                               "long"]
                              (derive-context))
        expected2         (-> ["persisted"
                               ---
                               "some"
                               "input"
                               -x-
                               "of"
                               "mine"
                               "that"
                               "is"
                               "this"
                               "lo|ng"
                               -x-]
                              (derive-context))
        processed1        (-> context (process [e/move-down]))
        processed2        (-> context (process [e/move-down e/move-down]))
        processed3        (-> context (process [e/move-down e/move-down e/move-down]))
        actual-preview1   (-> processed1 (r/preview-hud) (h/project-hud) (:lines))
        actual-cursor1    (-> processed1 (r/preview-hud) (h/project-hud) (:cursor))
        actual-offset1    (-> processed1 (r/preview-hud) (h/view-offset))

        expected-preview1 (-> expected1 (r/preview-hud) (h/project-hud) (:lines))
        expected-cursor1  (-> expected1 (r/preview-hud) (h/project-hud) (:cursor))
        expected-offset1  (-> expected1 (r/preview-hud) (h/view-offset))

        actual-preview2   (-> processed2 (r/preview-hud) (h/project-hud) (:lines))
        actual-cursor2    (-> processed2 (r/preview-hud) (h/project-hud) (:cursor))
        actual-offset2    (-> processed2 (r/preview-hud) (h/view-offset))

        expected-preview2 (-> expected2 (r/preview-hud) (h/project-hud) (:lines))
        expected-cursor2  (-> expected2 (r/preview-hud) (h/project-hud) (:cursor))
        expected-offset2  (-> expected2 (r/preview-hud) (h/view-offset))

        actual-preview3   (-> processed3 (r/preview-hud) (h/project-hud) (:lines))
        actual-cursor3    (-> processed3 (r/preview-hud) (h/project-hud) (:cursor))
        actual-offset3    (-> processed3 (r/preview-hud) (h/view-offset))]
    (is (= actual-preview1 expected-preview1))
    (is (= actual-cursor1 expected-cursor1))
    (is (= actual-offset1 expected-offset1))

    (is (= actual-preview2 expected-preview2))
    (is (= actual-cursor2 expected-cursor2))
    (is (= actual-offset2 expected-offset2))

    (is (= actual-preview2 expected-preview2))
    (is (= actual-cursor2 expected-cursor2))
    (is (= actual-offset2 expected-offset2))

    (is (= actual-preview3 expected-preview2))
    (is (= actual-cursor3 expected-cursor2))
    (is (= actual-offset3 expected-offset2))))

(deftest calibrates-exceeding-lower-view-forcibly
  (let [context           (-> ["persisted"
                               ---
                               -x-
                               "some"
                               "input"
                               "of"
                               "mine"
                               "that"
                               "is|"
                               -x-
                               "this"
                               "long"]
                              (derive-context))
        expected         (-> ["persisted"
                              ---
                              "some"
                              "input"
                              -x-
                              "of"
                              "mine"
                              "that"
                              "is"
                              "this"
                              "lo|ng"
                              -x-]
                             (derive-context))
        processed         (-> context
                              (r/with-input-area (-> context
                                                     (r/input-area)
                                                     (i/move (fn [[x y]] [x (+ y 2)]))))
                              (r/refresh)
                              (r/calibrate))

        actual-preview   (-> processed (r/preview-hud) (h/project-hud) (:lines))
        actual-cursor    (-> processed (r/preview-hud) (h/project-hud) (:cursor))
        actual-offset    (-> processed (r/preview-hud) (h/view-offset))

        expected-preview (-> expected (r/preview-hud) (h/project-hud) (:lines))
        expected-cursor  (-> expected (r/preview-hud) (h/project-hud) (:cursor))
        expected-offset  (-> expected (r/preview-hud) (h/view-offset))]
    (is (= actual-preview expected-preview))
    (is (= actual-cursor expected-cursor))
    (is (= actual-offset expected-offset))))

(deftest calibrates-exceeding-upper-view-forcibly
  (let [context           (-> ["persisted"
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
                              (derive-context))
        expected         (-> ["persisted"
                              ---
                              -x-
                              "so|me"
                              "input"
                              "of"
                              "mine"
                              "that"
                              "is"
                              -x-
                              "this"
                              "long"]
                             (derive-context))
        processed         (-> context
                              (r/with-input-area (-> context
                                                     (r/input-area)
                                                     (i/move (fn [[x y]] [x (- y 2)]))))
                              (r/refresh)
                              (r/calibrate))

        actual-preview   (-> processed (r/preview-hud) (h/project-hud) (:lines))
        actual-cursor    (-> processed (r/preview-hud) (h/project-hud) (:cursor))
        actual-offset    (-> processed (r/preview-hud) (h/view-offset))

        expected-preview (-> expected (r/preview-hud) (h/project-hud) (:lines))
        expected-cursor  (-> expected (r/preview-hud) (h/project-hud) (:cursor))
        expected-offset  (-> expected (r/preview-hud) (h/view-offset))]
    (is (= actual-preview expected-preview))
    (is (= actual-cursor expected-cursor))
    (is (= actual-offset expected-offset))))

(deftest keeps-overview-when-moving-within-view
  (let [context (-> ["persisted"
                     ---
                     "some"
                     -x-
                     "input"
                     "of|"
                     "mine"
                     "that"
                     "is"
                     "this"
                     -x-
                     "long"]
                    (derive-context))
        expected1 (-> ["persisted"
                       ---
                       "some"
                       -x-
                       "input"
                       "of"
                       "mine"
                       "th|at"
                       "is"
                       "this"
                       -x-
                       "long"]
                      (derive-context))
        processed1 (-> context (process [e/move-down e/move-down]))
        processed2 (-> context (process [e/move-down e/move-down e/move-up e/move-up]))

        actual-preview1   (-> processed1 (r/preview-hud) (h/project-hud) (:lines))
        actual-cursor1    (-> processed1 (r/preview-hud) (h/project-hud) (:cursor))
        actual-offset1    (-> processed1 (r/preview-hud) (h/view-offset))

        expected-preview1 (-> expected1 (r/preview-hud) (h/project-hud) (:lines))
        expected-cursor1  (-> expected1 (r/preview-hud) (h/project-hud) (:cursor))
        expected-offset1  (-> expected1 (r/preview-hud) (h/view-offset))

        actual-preview2   (-> processed2 (r/preview-hud) (h/project-hud) (:lines))
        actual-cursor2    (-> processed2 (r/preview-hud) (h/project-hud) (:cursor))
        actual-offset2    (-> processed2 (r/preview-hud) (h/view-offset))

        expected-preview2 (-> context (r/preview-hud) (h/project-hud) (:lines))
        expected-cursor2  (-> context (r/preview-hud) (h/project-hud) (:cursor))
        expected-offset2  (-> context (r/preview-hud) (h/view-offset))]
    (is (= actual-preview1 expected-preview1))
    (is (= actual-cursor1 expected-cursor1))
    (is (= actual-offset1 expected-offset1))

    (is (= actual-preview2 expected-preview2))
    (is (= actual-cursor2 expected-cursor2))
    (is (= actual-offset2 expected-offset2))

    (is (= actual-preview2 expected-preview2))
    (is (= actual-cursor2 expected-cursor2))
    (is (= actual-offset2 expected-offset2))))

(defn correct-under-deletion-top [ctx]
  (-> ctx
      (at-main-view-start)
      (process [up up])
      (should-be #(-> % (process [select-down delete-previous]) (overview) (= 1))
                 #(-> % (process [select-down select-down delete-previous]) (overview) (= 0))
                 #(-> % (process [select-down select-down select-down select-down delete-previous]) (overview) (= 0)))))

(defn correct-under-deletion-end [ctx]
  (-> ctx
      (at-input-end)
      (at-line-start)
      (should-be #(= (overview %) 0)
                 #(-> % (process [up select-down delete-previous]) (overview) (= 0))
                 #(-> % (process [up select-down delete-previous]) (overview) (= 0)))))

(defn correct-under-deletion-bottom [ctx]
  (-> ctx
      (at-main-view-start)
      (process [up up])
      (at-view-bottom)
      (should-be #(= (overview %) 2)
                 #(-> % (process [up select-down delete-previous]) (overview) (= 1))
                 #(-> % (process [select-up delete-previous]) (overview) (= 1))
                 #(-> % (process [up up select-down select-down delete-previous]) (overview) (= 0))
                 #(-> % (process [select-down delete-previous]) (overview) (= 0)))))

(defn correct-under-deletion-in-multi-line [ctx]
  (-> ctx
      (at-main-view-start)
      (process [up up down down down down])
      (should-be #(-> % (process [select-down delete-previous]) (overview) (= 1))
                 #(-> % (process [select-down select-down delete-previous]) (overview) (= 0)))))

(defn correct-under-insertion-top [ctx]
  (-> ctx
      (at-main-view-start)
      (should-be #(-> % (process [enter]) (overview) (= 1))
                 #(-> % (process [enter enter]) (overview) (= 2)))))

(defn correct-under-insertion-bottom [ctx]
  (-> ctx
      (at-main-view-start)
      (process [up up])
      (at-view-bottom)
      (process [enter enter enter enter])
      (should-be #(= (overview %) 2))))

(defn correct-under-insertion-end [ctx]
  (-> ctx
      (at-input-end)
      (at-line-start)
      (process [enter enter enter enter])
      (should-be #(= (overview %) 0))))

(defn correct-under-insertion-in-multi-line [ctx]
  (-> ctx
      (at-main-view-start)
      (process [up up down down down])
      (should-be #(= (overview %) 2)
                 #(-> % (process [enter]) (overview) (= 3))
                 #(-> % (process [enter enter]) (overview) (= 4)))))

(defn correct-under-multi-copied-insertion [ctx]
  (-> ctx
      (at-main-view-start)
      (process [up up])
      (at-view-bottom)
      (process [select-up select-up copy])
      (should-be #(-> % (process [paste]) (overview) (= 4))
                 #(-> % (process [down down paste]) (overview) (= 2)))))

(defn correct-under-multi-selected-deletion [ctx]
  (-> ctx
      (at-main-view-start)
      (process [up up down down down select-up select-up delete-previous])
      (should-be #(= (overview %) 0))))

(defn correct-under-change-variance [ctx]
  (-> ctx
      (at-main-view-start)
      (process [up up down down down enter select-down select-down delete-previous])
      (should-be #(= (overview %) 1)
                 #(-> % (process [enter enter enter select-up select-up select-up delete-previous]) (overview) (= 1))
                 #(-> % (process [select-down select-down delete-previous]) (overview) (= 0))
                 #(-> % (process [select-up select-up delete-previous]) (overview) (= 0))
                 #(-> % (process [enter select-up delete-previous]) (overview) (= 1)))))

(defn correct-under-rebounded-deletion [ctx]
  (-> ctx
      (at-main-view-start)
      (process [up up select-down select-down select-down delete-previous])
      (should-be #(= (overview %) 0))))


(defn correct-under-hud-enlargement [ctx]
  (-> ctx
      (at-main-view-start)
      (should-be #(-> % (process [up up]) (resize-view-by 1) (overview) (= 1))
                 #(-> % (process [up up]) (resize-view-by 2) (overview) (= 0))
                 #(-> % (process [select-all delete-previous]) (resize-view-by 2) (overview) (= 0)))))

(defn correct-under-hud-shrinking [ctx]
  (-> ctx
      (at-main-view-start)
      (should-be #(-> % (process [up up]) (resize-view-by -1) (overview) (= 3))
                 #(-> % (process [up up]) (resize-view-by -2) (overview) (= 4))
                 #(-> % (process [up up]) (resize-view-by -3) (overview) (= 5))
                 #(-> % (process [select-all delete-previous]) (resize-view-by -3) (overview) (= 0)))))

(defn correct-under-hud-size-variance [ctx]
  (-> ctx
      (at-main-view-start)
      (should-be #(-> % (process [up up]) (resize-view-by 2) (resize-view-by -2) (overview) (= 2))
                 #(-> % (process [up up]) (resize-view-by 2) (resize-view-by -1) (overview) (= 1))
                 #(-> % (process [up up]) (resize-view-by -2) (resize-view-by 1) (overview) (= 3))
                 #(-> % (process [up up]) (resize-view-by -4) (resize-view-by 2) (overview) (= 4)))))

(defn calibrating [ctx]
  (correct-under-deletion-top ctx)
  (correct-under-deletion-bottom ctx)
  (correct-under-deletion-end ctx)
  (correct-under-deletion-in-multi-line ctx)
  (correct-under-multi-selected-deletion ctx)
  (correct-under-change-variance ctx)
  (correct-under-rebounded-deletion ctx)

  (correct-under-insertion-top ctx)
  (correct-under-insertion-bottom ctx)
  (correct-under-insertion-end ctx)
  (correct-under-insertion-in-multi-line ctx)
  (correct-under-multi-copied-insertion ctx)
  (correct-under-hud-enlargement ctx)
  (correct-under-hud-shrinking ctx)
  (correct-under-hud-size-variance ctx))

(defspec calibrating-test 100
  (for-all [tctx (gen-context {:prefilled-size 5
                               :view-size      27
                               :text-area      (gen-text-area-of 29)})]
    (calibrating tctx)))

;; II. Scrolling

(deftest scrolls-up
  (let [context          (-> ["some"
                              "persisted"
                              "area"
                              ---
                              -x-
                              "existing"
                              "input|"
                              -x-
                              "area"]
                             (derive-context)
                             (process [e/scroll-up e/scroll-up e/scroll-up]))
        expected         (-> [-x-
                              "some"
                              "persisted"
                              -x-
                              "area"
                              ---
                              "existing"
                              "input|"
                              "area"]
                             (derive-context))
        actual-preview   (-> context (r/preview-hud) (h/project-hud) (:lines))
        expected-preview (-> expected (r/preview-hud) (h/project-hud) (:lines))
        expected-cursor  (-> context (r/preview-hud) (h/text) (:cursor))
        actual-cursor    (-> expected (r/preview-hud) (h/text) (:cursor))
        actual-offset    (-> context (r/preview-hud) (h/scroll-offset))]
    (is (= actual-offset 3))
    (is (= actual-preview expected-preview))
    (is (= actual-cursor expected-cursor))))

(deftest scrolls-down
  (let [context          (-> ["some"
                              "persisted"
                              "area"
                              ---
                              -x-
                              "existing"
                              "input|"
                              -x-
                              "area"]
                             (derive-context)
                             (process [e/scroll-up
                                       e/scroll-up
                                       e/scroll-up
                                       e/scroll-up]))
        expected         (-> ["some"
                              -x-
                              "persisted"
                              "area"
                              -x-
                              ---
                              "existing"
                              "input|"
                              "area"]
                             (derive-context))
        processed        (process context [e/scroll-down e/scroll-down])
        actual-preview   (-> processed (r/preview-hud) (h/project-hud) (:lines))
        expected-preview (-> expected (r/preview-hud) (h/project-hud) (:lines))
        actual-cursor    (-> processed (r/preview-hud) (h/text) (:cursor))
        expected-cursor  (-> expected (r/preview-hud) (h/text) (:cursor))
        actual-offset    (-> processed (r/preview-hud) (h/scroll-offset))]
    (is (= actual-offset 2))
    (is (= actual-preview expected-preview))
    (is (= actual-cursor expected-cursor))))

(deftest stops-scrolling-up-at-bounds
  (let [context         (-> ["some"
                             "persisted"
                             "area"
                             ---
                             -x-
                             "existing"
                             "input|"
                             -x-
                             "area"]
                            (derive-context))
        scrolled        (process context (repeat 100 e/scroll-up))
        init-offset     (-> context (r/preview-hud) (h/scroll-offset))
        scrolled-offset (-> scrolled (r/preview-hud) (h/scroll-offset))]
    (is (= init-offset 0))
    (is (= scrolled-offset 12))))

(deftest stops-scrolling-down-at-bounds
  (let [context            (-> ["some"
                                "persisted"
                                "area"
                                ---
                                -x-
                                "existing"
                                "input|"
                                -x-
                                "area"]
                               (derive-context))
        scrolled-up        (process context (repeat 100 e/scroll-up))
        scrolled-down      (process context (repeat 100 e/scroll-down))
        actual-up-offset   (-> scrolled-up (r/preview-hud) (h/scroll-offset))
        actual-down-offset (-> scrolled-down (r/preview-hud) (h/scroll-offset))]
    (is (= actual-up-offset 12))
    (is (= actual-down-offset 0))))

(deftest resets-scrolling
  (let [context             (-> ["some"
                                 "persisted"
                                 "area"
                                 ---
                                 -x-
                                 "existing"
                                 "input|"
                                 -x-
                                 "area"]
                                (derive-context))
        scrolled            (process context [e/scroll-up e/scroll-up])
        reset               (r/scroll-stop scrolled)
        init-scroll-offset  (-> scrolled (r/preview-hud) (h/scroll-offset))
        reset-scroll-offset (-> reset (r/preview-hud) (h/scroll-offset))]
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
        actual-cursor     (-> processed (r/preview-hud) (h/text) (:cursor))
        actual-preview    (-> processed (r/preview-hud) (h/text) (:lines))
        actual-previous   (-> processed (r/previous-hud) (h/text) (:lines))
        expected-cursor   (-> expected (r/preview-hud) (h/text) (:cursor))
        expected-preview  (-> expected (r/preview-hud) (h/text) (:lines))
        expected-previous (-> context (r/preview-hud) (h/text) (:lines))]
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
        actual-cursor     (-> processed (r/preview-hud) (h/text) (:cursor))
        actual-preview    (-> processed (r/preview-hud) (h/text) (:lines))
        actual-previous   (-> processed (r/previous-hud) (h/text) (:lines))
        expected-cursor   (-> expected (r/preview-hud) (h/text) (:cursor))
        expected-preview  (-> expected (r/preview-hud) (h/text) (:lines))
        expected-previous (-> context (r/preview-hud) (h/text) (:lines))]
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
        expected-previous (-> context (r/preview-hud) (h/text) (:lines))
        expected-preview  (-> expected (r/preview-hud) (h/text) (:lines))
        expected-cursor   (-> expected (r/preview-hud) (h/text) (:cursor))
        actual-preview    (-> processed (r/preview-hud) (h/text) (:lines))
        actual-previous   (-> processed (r/previous-hud) (h/text) (:lines))
        actual-cursor     (-> processed (r/preview-hud) (h/text) (:cursor))]
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
        actual-preview1    (-> processed1 (r/preview-hud) (h/text) (:lines))
        actual-cursor1     (-> processed1 (r/preview-hud) (h/text) (:cursor))
        expected-preview1  (-> expected1 (r/preview-hud) (h/text) (:lines))
        expected-cursor1   (-> expected1 (r/preview-hud) (h/text) (:cursor))

        actual-preview2    (-> processed2 (r/preview-hud) (h/text) (:lines))
        actual-cursor2     (-> processed2 (r/preview-hud) (h/text) (:cursor))
        expected-preview2  (-> expected2 (r/preview-hud) (h/text) (:lines))
        expected-cursor2   (-> expected2 (r/preview-hud) (h/text) (:cursor))

        actual-preview3    (-> processed3 (r/preview-hud) (h/text) (:lines))
        actual-cursor3     (-> processed3 (r/preview-hud) (h/text) (:cursor))]
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
        actual-preview   (-> context (r/preview-hud) (h/text) (:lines))
        actual-cursor    (-> context (r/preview-hud) (h/text) (:cursor))
        expected-preview (-> expected (r/preview-hud) (h/text) (:lines))
        expected-cursor  (-> expected (r/preview-hud) (h/text) (:cursor))]
    (is (= actual-preview expected-preview))
    (is (= actual-cursor expected-cursor))))

;; VII. Suggesting

(deftest shows-suggestions
  (let [context           (-> ["persisted"
                               ---
                               "1|"]
                              (derive-context {:response
                                               (completion-response ["option-1"
                                                                     "option-2"])}))
        expected1         (-> ["persisted"
                               ---
                               "option-1"
                               "------"
                               " option-1|"
                               " option-2"
                               "------"]
                              (derive-context))
        expected2         (-> ["persisted"
                               ---
                               "option-2"
                               "------"
                               " option-1"
                               " option-2|"
                               "------"]
                              (derive-context))
        processed1        (process context [e/suggest])
        processed2        (process context [e/suggest e/suggest])

        actual1-preview   (-> processed1 (r/preview-hud) (h/text) (:lines))
        actual1-cursor    (-> processed1 (r/preview-hud) (h/text) (:cursor))

        actual2-preview   (-> processed2 (r/preview-hud) (h/text) (:lines))
        actual2-cursor    (-> processed2 (r/preview-hud) (h/text) (:cursor))

        expected1-preview (-> expected1 (r/preview-hud) (h/text) (:lines))
        expected1-cursor  (-> expected1 (r/preview-hud) (h/text) (:cursor))

        expected2-preview (-> expected2 (r/preview-hud) (h/text) (:lines))
        expected2-cursor  (-> expected2 (r/preview-hud) (h/text) (:cursor))]
    (is (= actual1-preview expected1-preview))
    (is (= actual2-preview expected2-preview))
    (is (= actual1-cursor expected1-cursor))
    (is (= actual2-cursor expected2-cursor))))

(deftest keeps-suggestion-upon-input
  (let [context          (-> ["persisted"
                              ---
                              "1|"]
                             (derive-context {:response (completion-response ["option-1"
                                                                                    "option-2"])})
                             (process [e/suggest e/suggest (e/character \a)]))
        expected         (-> ["persisted"
                              ---
                              "option-2a|"]
                             (derive-context))
        actual-preview   (-> context (r/preview-hud) (h/text) (:lines))
        actual-cursor    (-> context (r/preview-hud) (h/text) (:cursor))
        expected-preview (-> expected (r/preview-hud) (h/text) (:lines))
        expected-cursor  (-> expected (r/preview-hud) (h/text) (:cursor))]
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
                      "------"
                      "|------"]
                     (derive-context))
        actual-preview   (-> context (r/preview-hud) (h/text) (:lines))
        actual-cursor    (-> context (r/preview-hud) (h/text) (:cursor))
        expected-preview (-> expected (r/preview-hud) (h/text) (:lines))
        expected-cursor  (-> expected (r/preview-hud) (h/text) (:cursor))]
    (is (= actual-preview expected-preview))
    (is (= actual-cursor expected-cursor))))

;; IX. Highlighting

(deftest gc-same-line-selection
  (testing "Moving left"
    (let [result     (-> ["This| is a line"]
                         (derive-context)
                         (process [select-left select-left select-right]))
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
                         (process [select-right select-right select-left]))
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
                            (process [select-all select-up select-up]))
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
                            (process [select-all select-up select-up select-left]))
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
                            (process [select-all select-up select-up select-right]))
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
                            (process [select-all select-up select-up select-down]))
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
                            (process [select-all select-up select-up select-down select-left]))
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
                            (process [select-all select-up select-up select-down select-right]))
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
                            (process [select-all right]))
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
                              (process [select-right select-right right]))
            garbage       (-> ["Some ⦇co⦈ntext"]
                              (derive-context)
                              (r/highlights)
                              (:manual)
                              (:region))
            expected-high (-> result (r/highlights) (:selection) (:region))
            expected-gc   (-> result (r/garbage) (:selection) (:region))]
        (is (= expected-high nil) "Highlights mismatch")
        (is (= expected-gc garbage) "Garbage mismatch")))))

;; X. Parenthesis matching

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