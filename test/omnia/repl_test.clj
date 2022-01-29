(ns omnia.repl-test
  (:require [clojure.test :refer [is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :refer [for-all]]
            [omnia.util.generator :refer [one]]
            [omnia.test-utils :refer :all]
            [schema.core :as s]
            [omnia.util.arithmetic :as a]
            [omnia.repl.hud :as h]
            [omnia.repl.context :as r]
            [omnia.text.core :as i]))

(def ^:const NR-OF-TESTS 1)

;; 0. Manipulation

(defn automatic-preview-refresh [ctx text]
  (let [some-input       (i/from-string "input")
        new-hud          (-> ctx (r/persisted-hud) (h/enrich-with [text]))
        expected-preview (-> new-hud (h/enrich-with [some-input]))
        actual-preview   (-> ctx
                             (r/with-input-area some-input)
                             (r/with-hud new-hud)
                             (r/preview-hud))]
    (is (= expected-preview actual-preview))))

(defn clipboard-propagation [ctx text]
  (let [expected-clipboard (i/from-string "content")
        text-full-clip     (assoc text :clipboard expected-clipboard)
        actual-clipboard   (-> ctx
                               (process [select-all copy])
                               (r/with-input-area text-full-clip)
                               (r/input-area)
                               (:clipboard))]
    (is (i/equivalent? expected-clipboard actual-clipboard))))

(defn clipboard-preservation [ctx text]
  (let [text-empty-clip    (assoc text :clipboard nil)
        expected-clipboard (r/input-area ctx)
        actual-clipboard   (-> ctx
                               (process [select-all copy])
                               (r/with-input-area text-empty-clip)
                               (r/input-area)
                               (:clipboard))]
    (is (i/equivalent? expected-clipboard actual-clipboard))))

(s/defn manipulations [ctx :- r/Context, seeker :- i/Seeker]
        (automatic-preview-refresh ctx seeker)
        (clipboard-propagation ctx seeker)
        (clipboard-preservation ctx seeker))

(defspec manipulating-test
         NR-OF-TESTS
  (for-all [seeker  (gen-text-area-of 10)
            context (gen-context {:view-size 7
                                  :text-area (gen-text-area-of 5)})]
           (manipulations context seeker)))

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
      (should-be #(-> % (process [select-down backspace]) (overview) (= 1))
                 #(-> % (process [select-down select-down backspace]) (overview) (= 0))
                 #(-> % (process [select-down select-down select-down select-down backspace]) (overview) (= 0)))))

(defn correct-under-deletion-end [ctx]
  (-> ctx
      (at-input-end)
      (at-line-start)
      (should-be #(= (overview %) 0)
                 #(-> % (process [up select-down backspace]) (overview) (= 0))
                 #(-> % (process [up select-down backspace]) (overview) (= 0)))))

(defn correct-under-deletion-bottom [ctx]
  (-> ctx
      (at-main-view-start)
      (process [up up])
      (at-view-bottom)
      (should-be #(= (overview %) 2)
                 #(-> % (process [up select-down backspace]) (overview) (= 1))
                 #(-> % (process [select-up backspace]) (overview) (= 1))
                 #(-> % (process [up up select-down select-down backspace]) (overview) (= 0))
                 #(-> % (process [select-down backspace]) (overview) (= 0)))))

(defn correct-under-deletion-in-multi-line [ctx]
  (-> ctx
      (at-main-view-start)
      (process [up up down down down down])
      (should-be #(-> % (process [select-down backspace]) (overview) (= 1))
                 #(-> % (process [select-down select-down backspace]) (overview) (= 0)))))

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
      (process [up up down down down select-up select-up backspace])
      (should-be #(= (overview %) 0))))

(defn correct-under-change-variance [ctx]
  (-> ctx
      (at-main-view-start)
      (process [up up down down down enter select-down select-down backspace])
      (should-be #(= (overview %) 1)
                 #(-> % (process [enter enter enter select-up select-up select-up backspace]) (overview) (= 1))
                 #(-> % (process [select-down select-down backspace]) (overview) (= 0))
                 #(-> % (process [select-up select-up backspace]) (overview) (= 0))
                 #(-> % (process [enter select-up backspace]) (overview) (= 1)))))

(defn correct-under-rebounded-deletion [ctx]
  (-> ctx
      (at-main-view-start)
      (process [up up select-down select-down select-down backspace])
      (should-be #(= (overview %) 0))))


(defn correct-under-hud-enlargement [ctx]
  (-> ctx
      (at-main-view-start)
      (should-be #(-> % (process [up up]) (resize-view-by 1) (overview) (= 1))
                 #(-> % (process [up up]) (resize-view-by 2) (overview) (= 0))
                 #(-> % (process [select-all backspace]) (resize-view-by 2) (overview) (= 0)))))

(defn correct-under-hud-shrinking [ctx]
  (-> ctx
      (at-main-view-start)
      (should-be #(-> % (process [up up]) (resize-view-by -1) (overview) (= 3))
                 #(-> % (process [up up]) (resize-view-by -2) (overview) (= 4))
                 #(-> % (process [up up]) (resize-view-by -3) (overview) (= 5))
                 #(-> % (process [select-all backspace]) (resize-view-by -3) (overview) (= 0)))))

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
  (correct-under-insertion-top ctx)
  (correct-under-insertion-bottom ctx)
  (correct-under-insertion-end ctx)
  (correct-under-insertion-in-multi-line ctx)
  (correct-under-multi-selected-deletion ctx)
  (correct-under-multi-copied-insertion ctx)
  (correct-under-change-variance ctx)
  (correct-under-rebounded-deletion ctx)
  (correct-under-hud-enlargement ctx)
  (correct-under-hud-shrinking ctx)
  (correct-under-hud-size-variance ctx))

(defspec calibrating-test
         NR-OF-TESTS
  (for-all [tctx (gen-context {:prefilled-size 5
                               :view-size      27
                               :text-area      (gen-text-area-of 29)})]
           (calibrating tctx)))

;; II. Scrolling

(defn scroll-upwards [ctx]
  (let [offset        4
        actual-offset (-> ctx (process (repeat offset scroll-up)) (scroll-offset))]
    (is (= offset actual-offset))))

(defn scroll-downwards [ctx]
  (let [actual-offset (-> ctx
                          (process (repeat 5 scroll-up))
                          (process (scroll-down 5 scroll-down))
                          (scroll-offset))]
    (is (= 0 actual-offset))))

(defn stop-upward-scroll [ctx]
  (let [text-size (-> ctx (r/preview-hud) (h/text) (:size))
        offset    (-> ctx (process (repeat 100 scroll-up)) (scroll-offset))]
    (is (= text-size offset))))

(defn stop-downward-scroll [ctx]
  (let [offset (-> ctx
                   (process (repeat 10 scroll-up))
                   (process (repeat 100 scroll-down))
                   (scroll-offset))]
    (is (= 0 offset))))

(defn scroll-reset [ctx]
  (let [scroll-offset (-> ctx
                          (at-input-end)
                          (at-line-start)
                          (process [scroll-up
                                    scroll-up
                                    (character \a)])
                          (scroll-offset))]
    (is (= 0 scroll-offset))))

(defn scrolling [ctx]
  (scroll-upwards ctx)
  (scroll-downwards ctx)
  (stop-upward-scroll ctx)
  (stop-downward-scroll ctx)
  (scroll-reset ctx))

(defspec scrolling-test
         NR-OF-TESTS
  (for-all [tctx (gen-context {:prefilled-size 20
                               :view-size      7
                               :text-area      (gen-text-area-of 10)})]
           (scrolling tctx)))

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
        expected-persisted (r/init-hud (r/view-size ctx) (r/client ctx))
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
                                backspace
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

(defn gc-same-line-moving-left [ctx]
  (let [test-ctx  (-> ctx (at-main-view-end) (at-line-start))
        text      (-> test-ctx (r/preview-hud) (h/text))
        y         (-> text (:size) (dec))
        x         0
        line-size (-> text (i/line-at y) (count))
        gc-end-x  (-> x (a/inc< line-size) (a/inc< line-size))
        hl-end-x  (-> x (a/inc< line-size) (a/inc< line-size) (a/dec< 0))]

    (let [result    (process test-ctx [select-right select-right select-left])
          hl-region (-> result (r/highlights) (:selection) (:region))
          gc-region (-> result (r/garbage) (:selection) (:region))]
      (is (= (:start gc-region) [x y]) "GC start mismatch")
      (is (= (:end gc-region) [gc-end-x y]) "GC end mismatch")

      (is (= (:start hl-region) [x y]) "HL start mismatch")
      (is (= (:end hl-region) [hl-end-x y]) "HL end mismatch"))))

;;FIXME
(defn gc-same-line-moving-right [ctx]
  "Interesting behaviour:
  Given text: 1
  Perform:
    right -> {:hl {}, :gc {}}
    right -> {:hl {}, :gc {}}
    select-left -> {:hl {:start [0, 6] :end [1, 6]} :gc {}}
    select-left -> {:hl {:start [0, 6] :end [1, 6]} :gc {:start [0, 6] :end [1, 6]}}}
    select-right -> {:hl {:start [1, 6] :end [1, 6]} :gc {:start [0, 6] :end [0, 6]}}}
    "
  (let [test-ctx   (-> ctx (at-main-view-end) (at-line-start))
        text       (-> test-ctx (r/preview-hud) (h/text))
        x          0
        y          (-> text (:size) (dec))
        line-size  (-> text (i/line-at y) (count))
        gc-start-x (-> x (a/inc< line-size) (a/inc< line-size) (a/dec< 0) (a/dec< 0))
        gc-end-x   (-> x (a/inc< line-size) (a/inc< line-size))
        hl-start-x (-> x (a/inc< line-size) (a/inc< line-size) (a/dec< 0))
        hl-end-x   (-> x (a/inc< line-size) (a/inc< line-size))]

    (let [result    (process test-ctx [right right select-left select-left select-right])
          hl-region (-> result (r/highlights) (:selection) (:region))
          gc-region (-> result (r/garbage) (:selection) (:region))]
      (is (= (:start gc-region) [gc-start-x y]) "GC start mismatch")
      (is (= (:end gc-region) [gc-end-x y]) "GC end mismatch")

      (is (= (:start hl-region) [hl-start-x y]) "HL start mismatch")
      (is (= (:end hl-region) [hl-end-x y]) "HL end mismatch"))))

(defn gc-same-line-complete-selection-moving-left [ctx]
  (let [test-ctx (-> ctx (at-main-view-end) (at-line-end))
        init-y   (-> test-ctx (r/persisted-hud) (h/text) (:size))
        [end-x end-y] (-> test-ctx (r/preview-hud) (h/text) (:cursor))]

    (let [result    (process test-ctx [select-all select-left])
          hl-region (-> result (r/highlights) (:selection) (:region))
          gc-region (-> result (r/garbage) (:selection) (:region))]
      (is (= (:start gc-region) [0 init-y]) "GC start mismatch")
      (is (= (:end gc-region) [end-x end-y]) "GC end mismatch")

      (is (= (:start hl-region) [0 init-y]) "HL start mismatch")
      (is (= (:end hl-region) [(- end-x 1) end-y]) "HL end mismatch"))))

(defn gc-same-line-complete-selection-moving-right [ctx]
  (let [test-ctx  (-> ctx (at-main-view-end) (at-line-end))
        text      (-> test-ctx (r/preview-hud) (h/text))
        init-y    (-> test-ctx (r/persisted-hud) (h/text) (:size))
        [end-x end-y] (:cursor text)]

    (let [result    (process test-ctx [select-all select-left select-left select-right])
          hl-region (-> result (r/highlights) (:selection) (:region))
          gc-region (-> result (r/garbage) (:selection) (:region))]
      (is (= (:start gc-region) [0 init-y]) "GC start mismatch")
      (is (= (:end gc-region) [(- end-x 2) end-y]) "GC end mismatch")

      (is (= (:start hl-region) [0 init-y]) "HL start mismatch")
      (is (= (:end hl-region) [(- end-x 1) end-y]) "HL end mismatch"))))

(defn gc-multi-line-complete-selection-moving-up [ctx]
  (let [test-ctx (-> ctx (at-main-view-end) (at-line-end))
        text     (-> test-ctx (r/preview-hud) (h/text))
        init-y   (-> test-ctx (r/persisted-hud) (h/text) (:size))
        [_ end-y] (:cursor text)
        exp-hl-x (count (i/line-at text (- end-y 2)))
        exp-gc-x (count (i/line-at text (- end-y 1)))]

    (let [result    (process test-ctx [select-all select-up select-up])
          hl-region (-> result (r/highlights) (:selection) (:region))
          gc-region (-> result (r/garbage) (:selection) (:region))]
      (is (= (:start gc-region) [0 init-y]) "GC start mismatch")
      (is (= (:end gc-region) [exp-gc-x (- end-y 1)]) "GC end mismatch")
      (println (:end (i/line-at text (-> gc-region :end :y)))
               (:end (i/line-at text (- end-y 1))))
      (is (= (:start hl-region) [0 init-y]) "HL start mismatch")
      (is (= (:end hl-region) [exp-hl-x (- end-y 2)]) "HL end mismatch"))))

(defn gc-multi-line-complete-selection-moving-down [ctx]
  (let [bottom (-> ctx (at-main-view-end) (at-line-end))
        [x y] (-> bottom (r/preview-hud) (h/text) (:cursor))
        region (-> bottom
                   (process [select-all select-up select-up select-down])
                   (r/garbage)
                   (:selection)
                   (:region))]
    (is (:start region) [x y])
    (is (:end region) [x (+ y 1)])))

(defn gc-multi-line-complete-selection-moving-up-left [ctx]
  (let [bottom (-> ctx (at-main-view-end) (at-line-end))
        [x y] (-> bottom (r/preview-hud) (h/text) (:cursor))
        region (-> bottom
                   (process [select-all select-up select-left])
                   (r/garbage)
                   (:selection)
                   (:region))]
    (is (:start region) [(- x 1) y])
    (is (:end region) [x (+ y 1)])))

(defn gc-multi-line-complete-selection-moving-up-right [ctx]
  (let [bottom (-> ctx (at-main-view-end) (at-line-end))
        [x y] (-> bottom (r/preview-hud) (h/text) (:cursor))
        region (-> bottom
                   (process [select-all select-up select-left select-left select-right])
                   (r/garbage)
                   (:selection)
                   (:region))]
    (is (:start region) [(- x 1) y])
    (is (:end region) [x (+ y 1)])))

(defn gc-multi-line-complete-selection-moving-down-left [ctx]
  (let [bottom (-> ctx (at-main-view-end) (at-line-end))
        [x y] (-> bottom (r/preview-hud) (h/text) (:cursor))
        region (-> bottom
                   (process [select-all select-up select-up select-down select-left])
                   (r/garbage)
                   (:selection)
                   (:region))]
    (is (:start region) [(- x 1) y])
    (is (:end region) [x (+ y 1)])))

(defn gc-multi-line-complete-selection-moving-down-right [ctx]
  (let [bottom (-> ctx (at-main-view-end) (at-line-end))
        [x y] (-> bottom (r/preview-hud) (h/text) (:cursor))
        region (-> bottom
                   (process [select-all select-up select-up select-down select-left select-left select-right])
                   (r/garbage)
                   (:selection)
                   (:region))]
    (is (:start region) [(- x 1) y])
    (is (:end region) [x (+ y 1)])))

(defn gc-reset [ctx]
  (let [bottom (-> ctx (at-main-view-end) (at-line-end))
        [x y] (-> bottom (r/preview-hud) (h/text) (:cursor))
        region (-> bottom
                   (process [select-all left select-all])
                   (r/garbage))]
    (is (empty? region))))

(defn highlighting [ctx]
  #_(gc-same-line-moving-left ctx)
  (gc-same-line-moving-right ctx)
  ;(gc-same-line-complete-selection-moving-left ctx)
  ;(gc-same-line-complete-selection-moving-right ctx)
  ;(gc-multi-line-complete-selection-moving-up ctx)
  ;(gc-multi-line-complete-selection-moving-down ctx)
  ;(gc-multi-line-complete-selection-moving-up-left ctx)
  ;(gc-multi-line-complete-selection-moving-up-right ctx)
  ;(gc-multi-line-complete-selection-moving-down-left ctx)
  ;(gc-multi-line-complete-selection-moving-down-right ctx)
  ;(gc-reset ctx)
  ;
  )

(defspec highlighting-test
         20
  (for-all [tctx (gen-context {:prefilled-size 20
                               :view-size      7
                               :text-area      (gen-text-area-of 10)})]
           (highlighting tctx)))

;; X. Parenthesis matching

(defn highlight-matched [ctx]
  (let [[x y] (-> ctx (r/preview-hud) (h/text) (:cursor))
        scheme            (fn [region]
                            {:region region
                             :scheme (-> ctx (r/configuration) (:syntax) (:clean-up))
                             :styles [:underline]})
        actual-highlights (-> ctx
                              (process [(character \()
                                        (character \a)
                                        (character \a)
                                        left
                                        left
                                        left
                                        parens-match])
                              (r/highlights))]
    (is (= (:open-paren actual-highlights)
           (scheme {:start [x y]
                    :end   [(inc x) y]})))
    (is (= (:closed-paren actual-highlights)
           (scheme {:start [(+ x 3) y]
                    :end   [(+ x 4) y]})))))

(defn dont-highlight-unmatched [ctx]
  (let [processed (process ctx [(character \()
                                (character \a)
                                (character \a)
                                select-right
                                backspace
                                left
                                left
                                left
                                parens-match])]
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
        expected     (-> (i/conjoin text h/delimiter (i/indent content 1) h/delimiter)
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