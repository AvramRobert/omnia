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

(def ^:const NR-OF-TESTS 100)

(deftest reads-context-from-description
  (testing "Filled sections"
    (let [context   (-> ["persisted"
                         "area"
                         --input-area
                         "input"
                         "area"
                         --viewable-area
                         "viewable"
                         "input|"]
                        (derive-context))
          header    (:lines default-header)
          persisted (->> context (r/persisted-hud) (h/text) (:lines))
          input     (-> context (r/input-area) (:lines))
          viewable  (-> context (r/preview-hud) (h/project-hud) (:lines))]
      (is (= persisted (concat header [[\p \e \r \s \i \s \t \e \d] [\a \r \e \a]])))
      (is (= input [[\i \n \p \u \t] [\a \r \e \a] [\v \i \e \w \a \b \l \e] [\i \n \p \u \t]]))
      (is (= viewable [[\v \i \e \w \a \b \l \e] [\i \n \p \u \t]])))

    (testing "Empty persisted area"
      (let [context   (-> [--input-area
                           "input"
                           "area"
                           --viewable-area
                           "viewable"
                           "input|"]
                          (derive-context))
            header    (:lines default-header)
            persisted (->> context (r/persisted-hud) (h/text) (:lines))
            input     (-> context (r/input-area) (:lines))
            viewable  (-> context (r/preview-hud) (h/project-hud) (:lines))]
        (is (= persisted header))
        (is (= input [[\i \n \p \u \t] [\a \r \e \a] [\v \i \e \w \a \b \l \e] [\i \n \p \u \t]]))
        (is (= viewable [[\v \i \e \w \a \b \l \e] [\i \n \p \u \t]]))))

    (testing "No persisted area and entire input area viewable"
      (let [context   (-> [--input-area
                           --viewable-area
                           "input"
                           "area"
                           "viewable"
                           "input|"]
                          (derive-context))
            header    (:lines default-header)
            persisted (->> context (r/persisted-hud) (h/text) (:lines))
            input     (-> context (r/input-area) (:lines))
            viewable  (-> context (r/preview-hud) (h/project-hud) (:lines))]
        (is (= persisted header))
        (is (= input [[\i \n \p \u \t] [\a \r \e \a] [\v \i \e \w \a \b \l \e] [\i \n \p \u \t]]))
        (is (= viewable [[\i \n \p \u \t] [\a \r \e \a] [\v \i \e \w \a \b \l \e] [\i \n \p \u \t]]))))

    (testing "With persisted area and entire input area viewable"
      (let [context   (-> ["persisted"
                           "area"
                           --input-area
                           --viewable-area
                           "input"
                           "area"
                           "viewable"
                           "input|"]
                          (derive-context))
            header    (:lines default-header)
            persisted (->> context (r/persisted-hud) (h/text) (:lines))
            input     (-> context (r/input-area) (:lines))
            viewable  (-> context (r/preview-hud) (h/project-hud) (:lines))]
        (is (= persisted (concat header [[\p \e \r \s \i \s \t \e \d] [\a \r \e \a]])))
        (is (= input [[\i \n \p \u \t] [\a \r \e \a] [\v \i \e \w \a \b \l \e] [\i \n \p \u \t]]))
        (is (= viewable [[\i \n \p \u \t] [\a \r \e \a] [\v \i \e \w \a \b \l \e] [\i \n \p \u \t]]))))))

;; 0. Manipulation

(deftest replacing-main-hud-refreshes-preview
  (let [context    (-> [--input-area
                        --viewable-area
                        "existing input|"]
                       (derive-context))
        enrichment (i/from-tagged-strings ["some input|"])
        actual     (-> context
                       (r/with-hud (-> context
                                       (r/persisted-hud)
                                       (h/enrich-with [enrichment])))
                       (r/preview-hud))
        expected   (-> [--input-area
                        --viewable-area
                        "existing input"
                        "some text|"]
                       (derive-context)
                       (r/preview-hud))]
    (= expected actual)))

(deftest clipboard-is-renewed
  (let [actual   (-> [--input-area
                      --viewable-area
                      "existing input"]
                     (derive-context)
                     (process [e/select-all e/copy (e/character \a) e/select-all e/copy])
                     (r/input-area)
                     (:clipboard))
        expected (i/from-tagged-strings ["existing inputa"])]
    (is (= expected actual))))

(deftest clipboard-is-propagated
  (let [actual   (-> [--input-area
                      --viewable-area
                      "existing |input"]
                     (derive-context)
                     (process [e/select-all e/copy (e/character \a) (e/character \b)])
                     (r/input-area)
                     (:clipboard))
        expected (i/from-tagged-strings ["existing input"])]
    (is (= expected actual))))

;; I. Calibrating

(defn exceed-upper-bound [ctx]
  (-> ctx
      (at-main-view-start)
      (should-be #(-> % (process [up]) (overview) (= 1))
                 #(-> % (process [up up]) (overview) (= 2))
                 #(-> % (process [up up up]) (overview) (= 2)))))

(defn exceed-lower-bound [ctx]
  (-> ctx
      (at-main-view-start)
      (process [up up])
      (at-view-bottom)
      (should-be #(= (overview %) 2)
                 #(-> % (process [down]) (overview) (= 1))
                 #(-> % (process [down down]) (overview) (= 0))
                 #(-> % (process [down down down]) (overview) (= 0)))))

(defn exceed-lower-bound-non-incrementally [ctx]
  (let [initial-view (-> ctx
                         (at-main-view-start)
                         (process [up up])
                         (at-view-bottom))
        new-text     (-> initial-view (r/input-area) (i/move-y #(+ % 2)))
        new-view     (-> initial-view
                         (r/with-input-area new-text)
                         (r/refresh)
                         (r/calibrate))]
    (is (= 0 (overview new-view)))))

(defn exceed-upper-bound-non-incrementally [ctx]
  (let [initial-view (-> ctx (at-main-view-start))
        new-text     (-> initial-view (r/input-area) (i/move-y #(- % 2)))
        new-view     (-> initial-view
                         (r/with-input-area new-text)
                         (r/refresh)
                         (r/calibrate))]
    (is (= 2 (overview new-view)))))

(defn scroll-upper-bound [ctx]
  (-> ctx
      (at-main-view-start)
      (process [up down down down])
      (should-be #(= (overview %) 1))))

(defn scroll-lower-bound [ctx]
  (-> ctx
      (at-main-view-start)
      (process [up])
      (at-view-bottom)
      (process [up up up up])
      (should-be #(= (overview %) 1))))

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
  (exceed-upper-bound ctx)
  (exceed-upper-bound-non-incrementally ctx)
  (exceed-lower-bound ctx)
  (exceed-lower-bound-non-incrementally ctx)
  (scroll-upper-bound ctx)
  (scroll-lower-bound ctx)

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
  (let [offset (-> ["some"
                    "persisted"
                    "area"
                    --input-area
                    --viewable-area
                    "existing"
                    "input"
                    "area|"]
                   (derive-context)
                   (process [e/scroll-up e/scroll-up e/scroll-up e/scroll-up])
                   (r/preview-hud)
                   (h/scroll-offset))]
    (is (= 4 offset))))

(deftest scrolls-down
  (let [offset (-> ["some"
                    "persisted"
                    "area"
                    --input-area
                    --viewable-area
                    "existing"
                    "input"
                    "area|"]
                   (derive-context)
                   (process [e/scroll-up
                             e/scroll-up
                             e/scroll-up
                             e/scroll-up
                             e/scroll-down
                             e/scroll-down])
                   (r/preview-hud)
                   (h/scroll-offset))]
    (is (= 2 offset))))

(deftest stops-scrolling-up-at-bounds
  (let [hud      (-> ["some"
                      "persisted"
                      "area"
                      --input-area
                      --viewable-area
                      "existing"
                      "input"
                      "area|"]
                     (derive-context)
                     (process (repeat 100 e/scroll-up))
                     (r/preview-hud))
        actual   (h/scroll-offset hud)
        expected (-> hud (h/text) (:size))]
    (is (= expected actual))))

(deftest stops-scrolling-down-at-bounds
  (let [hud      (-> ["some"
                      "persisted"
                      "area"
                      --input-area
                      --viewable-area
                      "existing"
                      "input"
                      "area|"]
                     (derive-context)
                     (process (repeat 100 e/scroll-up))
                     (process (repeat 100 e/scroll-down))
                     (r/preview-hud))
        actual   (h/scroll-offset hud)
        expected 0]
    (is (= expected actual))))

(deftest resets-scrolling
  (let [offset (-> ["some"
                    "persisted"
                    "area"
                    --input-area
                    --viewable-area
                    "existing"
                    "input"
                    "area|"]
                   (derive-context)
                   (process [e/scroll-up
                             e/scroll-up])
                   (r/scroll-stop)
                   (r/preview-hud)
                   (h/scroll-offset))]
    (is (= 0 offset))))

;; III. Capturing

(defn capture-and-remember [ctx]
  (-> ctx
      (at-input-end)
      (at-line-start)
      (process [(character \a)])
      (should-be #(= \a (-> % (r/preview-hud) (h/text) (i/previous-char)))
                 #(= \a (-> % (r/input-area) (i/previous-char)))
                 #(h/equivalent? (r/previous-hud %) (r/preview-hud ctx)))))

(defn capturing [ctx]
  (capture-and-remember ctx))

(defspec capturing-test
         NR-OF-TESTS
  (for-all [tctx (gen-context {:prefilled-size 20
                               :view-size      7
                               :text-area      (gen-text-area-of 10)})]
           (capturing tctx)))

;; IV. Clearing

(defn clear-remember-persist [ctx]
  (let [current-input      (r/input-area ctx)
        expected-persisted (r/init-hud (r/view-size ctx) (r/nrepl-client ctx))
        expected-complete  (h/enrich-with expected-persisted [current-input])]
    (-> ctx
        (process [clear])
        (should-be #(h/equivalent? (r/preview-hud %) expected-complete)
                   #(h/equivalent? (r/persisted-hud %) expected-persisted)
                   #(h/equivalent? (r/previous-hud %) (r/preview-hud ctx))))))

(defn clearing [ctx]
  (clear-remember-persist ctx))

(defspec clearing-test
         NR-OF-TESTS
  (for-all [tctx (gen-context {:prefilled-size 20
                               :view-size      7
                               :text-area      (gen-text-area-of 10)})]
           (clearing tctx)))

;; V. Evaluating

(defn remember-preserve-persist [ctx eval-result]
  (let [current-input      (r/input-area ctx)
        result             (i/from-string eval-result)
        expected-previous  (r/preview-hud ctx)
        expected-persisted (-> ctx
                               (r/persisted-hud)
                               (h/enrich-with [current-input
                                               i/empty-line
                                               result
                                               i/empty-line
                                               r/caret]))
        expected-complete  (h/enrich-with expected-persisted [i/empty-line])]
    (-> ctx
        (process [evaluate])
        (should-be #(i/equivalent? (r/input-area %) i/empty-line)
                   #(h/equivalent? (r/previous-hud %) expected-previous)
                   #(h/equivalent? (r/persisted-hud %) expected-persisted)
                   #(h/equivalent? (r/preview-hud %) expected-complete)))))

(defn evaluating [ctx eval-result]
  (remember-preserve-persist ctx eval-result))

(defspec evaluating-test
         NR-OF-TESTS
  (let [eval-result "result"]
    (for-all [tctx (gen-context {:prefilled-size 20
                                 :view-size      7
                                 :text-area      (gen-text-area-of 10)
                                 :receive        (gen-evaluation eval-result)})]
             (evaluating tctx eval-result))))

;; VI. Rolling back

(defn roll-rebase-remember-back [ctx]
  (let [hist (server-history ctx)]
    (-> ctx
        (process [prev-eval])
        (should-be #(h/equivalent? (r/previous-hud %) (r/preview-hud ctx))
                   #(i/equivalent? (r/input-area %) (first hist))))))

(defn roll-keep-clipboard [ctx]
  (let [processed (process ctx [select-all
                                copy
                                prev-eval
                                prev-eval
                                select-all
                                delete-previous
                                paste])]
    (is (h/equivalent? (r/preview-hud processed) (r/preview-hud ctx)))))

(defn rolling-back [ctx]
  (roll-keep-clipboard ctx)
  (roll-rebase-remember-back ctx))

(defspec rolling-back-test
         NR-OF-TESTS
  (for-all [tctx (gen-context {:prefilled-size 5
                               :view-size      27
                               :text-area      (gen-text-area-of 29)
                               :history        (gen-history {:prefilled-size 1
                                                             :element-size   10})})]
           (rolling-back tctx)))

;; VII. Rolling forward

(defn roll-rebase-remember-forward [ctx]
  (let [processed (process ctx [evaluate
                                prev-eval
                                prev-eval
                                next-eval])]
    (should-be processed
               #(i/equivalent? (r/input-area %) (r/input-area ctx))
               #(h/equivalent? (-> % (process [next-eval]) (r/previous-hud))
                               (r/preview-hud %)))))

(defn rolling-forward [ctx]
  (roll-rebase-remember-forward ctx))

(defspec rolling-forward-test
         NR-OF-TESTS
  (for-all [tctx (gen-context {:prefilled-size 5
                               :view-size      27
                               :text-area      (gen-text-area-of 29)
                               :history        (gen-history {:prefilled-size 1
                                                             :element-size   12})})]
           (rolling-forward tctx)))

;; VIII. Suggesting

(defn suggestion-continuation [ctx]
  (let [suggestions        (suggestions ctx)
        suggestion-size    (:size suggestions)
        end                (-> ctx (at-input-end) (at-line-start))
        replaced-point     (cursor end)
        process-suggestion (fn [ctx n]
                             (-> ctx
                                 (process (repeat n suggest))
                                 (r/preview-hud)
                                 (h/text)
                                 (i/reset-to replaced-point)
                                 (i/current-line)))]
    (should-be end
               #(= (process-suggestion % 1) (suggestion-at % 0))
               #(= (process-suggestion % 2) (suggestion-at % 1))
               #(= (process-suggestion % 3) (suggestion-at % 2))
               #(= (process-suggestion % (inc suggestion-size)) (suggestion-at % 0)))))

(defn suggestion-override [ctx]
  (let [input    \a
        actual   (-> ctx
                     (at-input-end)
                     (at-line-start)
                     (process [suggest suggest (character input)])
                     (r/preview-hud)
                     (h/text)
                     (i/current-line))
        expected (-> ctx (suggestion-at 1) (conj input))]
    (is (= expected actual))))

(defn suggesting [ctx]
  (suggestion-continuation ctx)
  (suggestion-override ctx))

(defspec suggesting-test
         NR-OF-TESTS
  (for-all [tctx (gen-context {:prefilled-size 20
                               :view-size      15
                               :receive        (gen-completion 12)
                               :text-area      (gen-text-area-of 17)})]
           (suggesting tctx)))

(defn no-override [ctx]
  (let [end            (-> ctx (at-input-end) (at-line-start))
        current-line   (-> end (r/preview-hud) (h/text) (i/current-line))
        current-cursor (-> end (r/preview-hud) (h/text) (:cursor))
        actual-line    (-> end
                           (process [suggest])
                           (r/preview-hud)
                           (h/text)
                           (i/reset-to current-cursor)
                           (i/current-line))]
    (is (= current-line actual-line))))

(defn empty-suggesting [ctx]
  (no-override ctx))

(defspec empty-suggesting-test
         NR-OF-TESTS
  (for-all [tctx (gen-context {:prefilled-size 20
                               :view-size      15
                               :text-area      (gen-text-area-of 17)})]
           (empty-suggesting tctx)))

;; IX. Highlighting

(deftest gc-same-line-selection
  (testing "Moving left"
    (let [result     (-> ["This| is a line"]
                         (i/from-tagged-strings)
                         (context-from)
                         (process [select-left select-left select-right]))
          highlights (-> ["Thi<s> is a line"]
                         (i/from-tagged-strings)
                         (:selection))
          garbage    (-> ["Th<is> is a line"]
                         (i/from-tagged-strings)
                         (:selection))]
      (is (= (-> result (r/highlights) (:selection) (:region)) highlights) "Highlights mismatch")
      (is (= (-> result (r/garbage) (:selection) (:region)) garbage) "Garbage mismatch")))

  (testing "Moving right"
    (let [result     (-> ["|This is a line"]
                         (i/from-tagged-strings)
                         (context-from)
                         (process [select-right select-right select-left]))
          highlights (-> ["<T>his is a line"]
                         (i/from-tagged-strings)
                         (:selection))
          garbage    (-> ["<Th>is is a line"]
                         (i/from-tagged-strings)
                         (:selection))]
      (is (= (-> result (r/highlights) (:selection) (:region)) highlights) "Highlights mismatch")
      (is (= (-> result (r/garbage) (:selection) (:region)) garbage) "Garbage mismatch"))))

(deftest gc-multi-line-selection
  (testing "Moving up"
    (let [result     (-> ["This is"
                          "a |large"
                          "context"]
                         (i/from-tagged-strings)
                         (context-from)
                         (process [select-all select-up select-up]))
          highlights (-> ["<This is>"
                          "a large"
                          "context"]
                         (i/from-tagged-strings)
                         (:selection))
          garbage    (-> ["<This is"
                          "a large>"
                          "context"]
                         (i/from-tagged-strings)
                         (:selection))]
      (is (= (-> result (r/highlights) (:selection) (:region)) highlights) "Highlights mismatch")
      (is (= (-> result (r/garbage) (:selection) (:region)) garbage) "Garbage mismatch")))

  (testing "Moving up left"
    (let [result     (-> ["This is"
                          "a |large"
                          "context"]
                         (i/from-tagged-strings)
                         (context-from)
                         (process [select-all select-up select-up select-left]))
          highlights (-> ["<This i>s"
                          "a large"
                          "context"]
                         (i/from-tagged-strings)
                         (:selection))
          garbage    (-> ["<This is>"
                          "a large"
                          "context"]
                         (i/from-tagged-strings)
                         (:selection))]
      (is (= (-> result (r/highlights) (:selection) (:region)) highlights) "Highlights mismatch")
      (is (= (-> result (r/garbage) (:selection) (:region)) garbage) "Garbage mismatch")))

  (testing "Moving up right"
    (let [result     (-> ["This is"
                          "a |large"
                          "context"]
                         (i/from-tagged-strings)
                         (context-from)
                         (process [select-all select-up select-up select-right]))
          highlights (-> ["<This is"
                          ">a large"
                          "context"]
                         (i/from-tagged-strings)
                         (:selection))
          garbage    (-> ["<This is>"
                          "a large"
                          "context"]
                         (i/from-tagged-strings)
                         (:selection))]
      (is (= (-> result (r/highlights) (:selection) (:region)) highlights) "Highlights mismatch")
      (is (= (-> result (r/garbage) (:selection) (:region)) garbage) "Garbage mismatch")))

  (testing "Moving down"
    (let [result     (-> ["This is"
                          "a |large"
                          "context"]
                         (i/from-tagged-strings)
                         (context-from)
                         (process [select-all select-up select-up select-down]))
          highlights (-> ["<This is"
                          "a large>"
                          "context"]
                         (i/from-tagged-strings)
                         (:selection))
          garbage    (-> ["<This is>"
                          "a large"
                          "context"]
                         (i/from-tagged-strings)
                         (:selection))]
      (is (= (-> result (r/highlights) (:selection) (:region)) highlights) "Highlights mismatch")
      (is (= (-> result (r/garbage) (:selection) (:region)) garbage) "Garbage mismatch")))

  (testing "Moving down left"
    (let [result     (-> ["This is"
                          "a |large"
                          "context"]
                         (i/from-tagged-strings)
                         (context-from)
                         (process [select-all select-up select-up select-down select-left]))
          highlights (-> ["<This is"
                          "a larg>e"
                          "context"]
                         (i/from-tagged-strings)
                         (:selection))
          garbage    (-> ["<This is"
                          "a large>"
                          "context"]
                         (i/from-tagged-strings)
                         (:selection))]
      (is (= (-> result (r/highlights) (:selection) (:region)) highlights) "Highlights mismatch")
      (is (= (-> result (r/garbage) (:selection) (:region)) garbage) "Garbage mismatch")))

  (testing "Moving down right"
    (let [result     (-> ["This is"
                          "a |large"
                          "context"]
                         (i/from-tagged-strings)
                         (context-from)
                         (process [select-all select-up select-up select-down select-right]))
          highlights (-> ["<This is"
                          "a large"
                          ">context"]
                         (i/from-tagged-strings)
                         (:selection))
          garbage    (-> ["<This is"
                          "a large>"
                          "context"]
                         (i/from-tagged-strings)
                         (:selection))]
      (is (= (-> result (r/highlights) (:selection) (:region)) highlights) "Highlights mismatch")
      (is (= (-> result (r/garbage) (:selection) (:region)) garbage) "Garbage mismatch"))))

(deftest gc-reset
  (testing "Multiple lines"
    (let [result  (-> ["Some |context"
                       "with lines"]
                      (i/from-tagged-strings)
                      (context-from)
                      (process [select-all right]))
          garbage (-> ["<Some context"
                       "with lines>"]
                      (i/from-tagged-strings)
                      (:selection))]
      (is (= (-> result (r/highlights) (:selection) (:region)) nil) "Highlights mismatch")
      (is (= (-> result (r/garbage) (:selection) (:region)) garbage) "Garbage mismatch")))

  (testing "Same line"
    (testing "Multiple lines"
      (let [result  (-> ["Some |context"]
                        (i/from-tagged-strings)
                        (context-from)
                        (process [select-right select-right right]))
            garbage (-> ["Some <co>ntext"]
                        (i/from-tagged-strings)
                        (:selection))]
        (is (= (-> result (r/highlights) (:selection) (:region)) nil) "Highlights mismatch")
        (is (= (-> result (r/garbage) (:selection) (:region)) garbage) "Garbage mismatch")))))

;; X. Parenthesis matching
(defn highlight-matched [ctx]
  (let [[x y] (-> ctx (r/preview-hud) (h/text) (:cursor))
        scheme     (fn [region]
                     {:region region
                      :scheme (-> default-config (:syntax) (:clean-up))
                      :styles [:underline]})
        highlights (-> ctx
                       (process [(character \()
                                 (character \a)
                                 (character \a)
                                 left
                                 left
                                 left])
                       (r/highlights))]
    (is (= (:open-paren highlights)
           (scheme {:start [x y]
                    :end   [(inc x) y]})))
    (is (= (:closed-paren highlights)
           (scheme {:start [(+ x 3) y]
                    :end   [(+ x 4) y]})))))

(defn dont-highlight-unmatched [ctx]
  (let [processed (process ctx [(character \()
                                (character \a)
                                (character \a)
                                select-right
                                delete-previous
                                left
                                left
                                left])]
    (is (= (r/highlights processed) {}))))

(defn parens-matching [ctx]
  (highlight-matched ctx)
  (dont-highlight-unmatched ctx))

(defspec parens-matching-test
         NR-OF-TESTS
  (for-all [tctx (gen-context {:prefilled-size 20
                               :view-size      7
                               :text-area      (gen-text-area-of 10)})]
           (parens-matching tctx)))

;; XI. Pop-ups

(defn pop-up-with-calibration [ctx content]
  (let [window  (->> content (:size) (h/riffle-window content))
        context (-> ctx (at-main-view-start) (process [up up]))]
    (is (= 2 (-> context (pop-up window) (h/view-offset))))
    (is (= 2 (-> context (process [down down]) (pop-up window) (h/view-offset))))
    (is (= 2 (-> context (at-view-bottom) (pop-up window) (h/view-offset))))
    (is (= 1 (-> context (at-view-bottom) (process [down]) (pop-up window) (h/view-offset))))))


(defn pop-up-window [ctx content]
  (let [content-size (:size content)
        window       (h/riffle-window content content-size)
        text         (r/input-area ctx)
        pop-up-size  (+ content-size 2)
        expected     (-> (i/join-many text h/delimiter (i/indent content 1) h/delimiter)
                         (i/rebase #(take-last pop-up-size %)))
        actual       (-> ctx
                         (at-input-end)
                         (at-line-start)
                         (pop-up window)
                         (h/text)
                         (i/rebase #(take-last pop-up-size %)))]
    (is (= (-> actual (i/start) (i/current-line)) (i/current-line h/delimiter)))
    (is (= (-> actual (i/end) (i/current-line)) (i/current-line h/delimiter)))
    (is (i/equivalent? expected actual))))

(defn empty-pop-up-window [ctx]
  (pop-up-window ctx i/empty-seeker))

(defn framed-pop-up [ctx]
  (pop-up-window ctx (one (gen-text-area-of 5))))

(defn pop-up-riffled [ctx content]
  (let [content-size (:size content)
        window       (h/riffle-window content content-size)
        preview      (r/preview-hud ctx)
        line         #(-> window (h/text) (i/reset-y %) (i/indent 1) (i/current-line))
        riffled-line #(->> window
                           (iterate h/riffle)
                           (take (inc %))
                           (last)
                           (h/pop-up preview)
                           (h/text)
                           (i/current-line))]
    (is (= (line 0) (riffled-line 0)))
    (is (= (line 1) (riffled-line 1)))
    (is (= (line 4) (riffled-line 4)))
    (is (= (line 0) (riffled-line 5)))))

(defn pop-ups [ctx]
  (empty-pop-up-window ctx)
  (framed-pop-up ctx)
  (pop-up-with-calibration ctx (one (gen-text-area-of 5)))
  (pop-up-riffled ctx (one (gen-text-area-of 5))))

(defspec pop-up-test
         NR-OF-TESTS
  (for-all [tctx (gen-context {:prefilled-size 20
                               :view-size      15
                               :text-area      (gen-text-area-of 17)})]
           (pop-ups tctx)))


(defn unchanged-highlights [ctx]
  (let [processed (process ctx [(character \() right ignore])]
    (is (not (nil? (-> processed (r/highlights) (:open-paren)))))))

(defn unchanged-selection [ctx]
  (let [processed (process ctx [select-right
                                select-right
                                select-right
                                ignore])]
    (is (not (nil? (-> processed (r/highlights) (:selection)))))))

(defn unchanged-scrolling [ctx]
  (let [actual-offset (-> ctx
                          (at-input-end)
                          (process [scroll-up scroll-up ignore])
                          (scroll-offset))]
    (is (= 2 actual-offset))))

(defn ignores [ctx]
  (unchanged-highlights ctx)
  (unchanged-selection ctx)
  (unchanged-scrolling ctx))

(defspec ignoring-test
         NR-OF-TESTS
  (for-all [tctx (gen-context {:view-size 10
                               :text-area (gen-text-area-of 17)})]
           (ignores tctx)))
