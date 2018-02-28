(ns omnia.hud-test
  (:require [clojure.test :refer [is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :refer [for-all]]
            [clojure.test.check.generators :as gen]
            [omnia.test-utils :refer :all]
            [omnia.hud :as h]
            [omnia.input :as i]))

;; I. Calibrating

(defn exceed-upper-bound [ctx]
  (-> (move-top-fov ctx)
      (can-be #(-> % (process up) (ov) (= 1))
              #(-> % (process up 2) (ov) (= 2))
              #(-> % (process up 3) (ov) (= 2)))))

(defn exceed-lower-bound [ctx]
  (-> (move-top-fov ctx)
      (process up 2)
      (move-bottom-fov)
      (can-be #(= (ov %) 2)
              #(-> % (process down) (ov) (= 1))
              #(-> % (process down 2) (ov) (= 0))
              #(-> % (process down 3) (ov) (= 0)))))

(defn exceed-lower-bound-non-incrementally [ctx]
  (-> (move-top-fov ctx)
      (process up 2)
      (move-bottom-fov)
      (update :seeker #(i/move % (fn [[x y]] [x (+ 2 y)])))
      (h/preserve)
      (h/rebase)
      (h/calibrate)
      (can-be #(= (ov %) 0))))

(defn exceed-upper-bound-non-incrementally [ctx]
  (-> (move-top-fov ctx)
      (update :seeker #(i/move % (fn [[x y]] [x (- 2 y)])))
      (h/preserve)
      (h/rebase)
      (h/calibrate)
      (can-be #(= (ov %) 2))))

(defn scroll-upper-bound [ctx]
  (-> (move-top-fov ctx)
      (process up)
      (process down 6)
      (can-be #(= (ov %) 1))))

(defn scroll-lower-bound [ctx]
  (-> (move-top-fov ctx)
      (process up)
      (move-bottom-fov)
      (process up 6)
      (can-be #(= (ov %) 1))))


(defn correct-under-deletion-top [ctx]
  (-> (from-start ctx)
      (move-top-fov)
      (process up 2)
      (can-be #(-> % (process select-down) (process backspace) (ov) (= 1))
              #(-> % (process select-down 2) (process backspace) (ov) (= 0))
              #(-> % (process select-down 3) (process backspace) (ov) (= 0)))))

(defn correct-under-deletion-end [ctx]
  (-> (from-start ctx)
      (move-end-fov)
      (can-be #(= (ov %) 0)
              #(-> % (process up) (process select-down) (process backspace) (ov) (= 0))
              #(-> % (process up 2) (process select-down) (process backspace) (ov) (= 0)))))

(defn correct-under-deletion-bottom [ctx]
  (-> (from-start ctx)
      (move-top-fov)
      (process up 2)
      (move-bottom-fov)
      (can-be #(= (ov %) 2)
              #(-> % (process up) (process select-down) (process backspace) (ov) (= 1))
              #(-> % (process select-up) (process backspace) (ov) (= 1))
              #(-> % (process up 2) (process select-down 2) (process backspace) (ov) (= 0))
              #(-> % (process select-down) (process backspace) (ov) (= 0)))))

(defn correct-under-deletion-in-multi-line [ctx]
  (-> (from-start ctx)
      (move-top-fov)
      (process up 2)
      (process down 4)
      (can-be #(-> % (process select-down) (process backspace) (ov) (= 1))
              #(-> % (process select-down 2) (process backspace) (ov) (= 0)))))

(defn correct-under-insertion-top [ctx]
  (-> (move-top-fov ctx)
      (can-be #(-> % (process enter) (ov) (= 1))
              #(-> % (process enter) (process enter) (ov) (= 2)))))

(defn correct-under-insertion-bottom [ctx]
  (-> (from-end ctx)
      (move-top-fov)
      (process up 2)
      (move-bottom-fov)
      (process enter 10)
      (can-be #(= (ov %) 2))))

(defn correct-under-insertion-end [ctx]
  (-> (move-end-fov ctx)
      (process enter 10)
      (can-be #(= (ov %) 0))))

(defn correct-under-insertion-in-multi-line [ctx]
  (-> (move-top-fov ctx)
      (process up 2)
      (process down 3)
      (can-be #(= (ov %) 2)
              #(-> % (process enter) (ov) (= 3))
              #(-> % (process enter 2) (ov) (= 4)))))

(defn correct-under-multi-copied-insertion [ctx]
  (-> (from-end ctx)
      (move-top-fov)
      (process up 2)
      (move-bottom-fov)
      (process select-up 2)
      (process copy)
      (can-be #(-> % (process paste) (ov) (= 4))
              #(-> % (process down 2) (process paste) (ov) (= 2)))))

(defn correct-under-multi-selected-deletion [ctx]
  (-> (from-start ctx)
      (move-top-fov)
      (process up 2)
      (process down 3)
      (process select-up 2)
      (process backspace)
      (can-be #(= (ov %) 0))))

(defn correct-under-change-variance [ctx]
  (-> (from-end ctx)
      (move-top-fov)
      (process up 2)
      (process down 3)
      (process enter)
      (process select-down 2)
      (process backspace)
      (can-be #(= (ov %) 1)
              #(-> % (process enter 3) (process select-up 3) (process backspace) (ov) (= 1))
              #(-> % (process select-down 2) (process backspace) (ov) (= 0))
              #(-> % (process select-up 2) (process backspace) (ov) (= 0))
              #(-> % (process enter) (process select-up) (process backspace) (ov) (= 1)))))

(defn correct-under-rebounded-deletion [ctx]
  (-> (from-end ctx)
      (move-top-fov)
      (process up 2)
      (process select-down 10)
      (process backspace)
      (can-be #(= (ov %) 0))))


(defn correct-under-hud-enlargement [ctx]
  (-> (move-top-fov ctx)

      (can-be #(-> % (process up 2) (enlarge-by 1) (process down) (ov) (= 1))
              #(-> % (process up 2) (enlarge-by 2) (process down) (ov) (= 0))
              #(-> % (process select-all) (process backspace) (enlarge-by 2) (process down) (ov) (= 0)))))

(defn correct-under-hud-shrinking [ctx]
  (-> (move-top-fov ctx)
      (can-be #(-> % (process up 2) (shrink-by 1) (process down) (ov) (= 3))
              #(-> % (process up 2) (shrink-by 2) (process down) (ov) (= 4))
              #(-> % (process up 2) (shrink-by 3) (process down) (ov) (= 5))
              #(-> % (process select-all) (process backspace) (shrink-by 3) (process down) (ov) (= 0)))))

(defn correct-under-hud-size-variance [ctx]
  (-> (move-top-fov ctx)
      (can-be #(-> % (process up 2) (enlarge-by 2) (shrink-by 2) (ov) (= 2))
              #(-> % (process up 2) (enlarge-by 2) (shrink-by 1) (process down) (ov) (= 1))
              #(-> % (process up 2) (shrink-by 2) (enlarge-by 1) (process down) (ov) (= 3))
              #(-> % (process up 2) (shrink-by 4) (enlarge-by 2) (process down) (ov) (= 4)))))

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
         100
         (for-all [tctx (gen-context {:size 5
                                      :fov 27
                                      :seeker (one (gen-seeker-of 29))})]
                  (calibrating tctx)))

;; II. Scrolling

(defn scroll-upwards [ctx]
  (let [fov (get-in ctx [:complete-hud :fov])]
    (-> ctx
        (process scroll-up 4)
        (lor)
        (= (+ fov 4))
        (is))))

(defn scroll-downwards [ctx n]
  (-> ctx
      (process scroll-up n)
      (process scroll-down n)
      (can-be #(= (lor %) (lor ctx)))))

(defn stop-upward-scroll [ctx]
  (let [h (get-in ctx [:complete-hud :height])]
    (-> ctx
        (process scroll-up 100)
        (lor)
        (= h)
        (is))))

(defn stop-downward-scroll [ctx]
  (-> ctx
      (process scroll-up 10)
      (process scroll-down 100)
      (lor)
      (= (lor ctx))
      (is)))

(defn scroll-ending-with-ov [ctx]
  (let [offset (-> (move-top-fov ctx) (process up 2))
        ov  (get-in offset [:complete-hud :ov])
        fov (get-in offset [:complete-hud :fov])]
    (-> offset
        (process scroll-up 3)
        (process scroll-down 5)
        (lor)
        (= (+ ov fov))
        (is))))

(defn scroll-reset [ctx]
  (let [lor' (get-in ctx [:complete-hud :lor])]
    (-> (move-end-fov ctx)
        (process scroll-up 5)
        (process (char-key \a))
        (lor)
        (= lor')
        (is))))

(defn scroll-reset-with-ov [ctx]
  (let [fov (get-in ctx [:complete-hud :fov])]
    (-> (move-top-fov ctx)
        (process up)
        (process scroll-up 5)
        (process (char-key \a))
        (lor)
        (= (+ fov 1))
        (is))))

(defn scrolling [ctx]
  (scroll-upwards ctx)
  (scroll-downwards ctx (one gen/pos-int))
  (stop-upward-scroll ctx)
  (stop-downward-scroll ctx)
  (scroll-ending-with-ov ctx)
  (scroll-reset ctx)
  (scroll-reset-with-ov ctx))

(defspec scrolling-test
         100
         (for-all [tctx (gen-context {:size 20
                                      :fov 7
                                      :seeker (one (gen-seeker-of 10))})]
                  (scrolling tctx)))

;; III. Capturing

(defn capture-and-remember [ctx]
  (-> (move-end-fov ctx)
      (process (char-key \a))
      (can-be #(-> (:complete-hud %) (i/left) (= \a))
              #(-> (:seeker %) (i/left) (= \a))
              #(-> (:previous-hud %) (<=> (:complete-hud ctx))))))

(defn capturing [ctx]
  (capture-and-remember ctx))

(defspec capturing-test
         100
         (for-all [tctx (gen-context {:size 20
                                      :fov 7
                                      :seeker (one (gen-seeker-of 10))})]
                  (capturing tctx)))

;; IV. Clearing

(defn clear-remember-persist [ctx]
  (let [expected-persisted (h/init-hud ctx)
        expected-complete (i/join expected-persisted (:seeker ctx))]
    (-> ctx
        (process clear)
        (can-be #(<=> (:complete-hud %) expected-complete)
                #(<=> (:persisted-hud %) expected-persisted)
                #(<=> (:previous-hud %) (:complete-hud ctx))))))

(defn clearing [ctx]
  (clear-remember-persist ctx))

(defspec clearing-test
         100
         (for-all [tctx (gen-context {:size 20
                                      :fov 7
                                      :seeker (one (gen-seeker-of 10))})]
                  (clearing tctx)))

;; V. Evaluating

(defn remember-preserve-persist [ctx]
  (let [expected-hud (i/join-many (:complete-hud ctx) h/empty-line h/caret)]
    (-> ctx
        (process evaluate)
        (can-be #(<=> (:seeker %) i/empty-seeker)
                #(<=> (:previous-hud %) (:complete-hud ctx))
                #(<=> (:complete-hud %) expected-hud)
                #(<=> (:persisted-hud %) expected-hud)))))

(defn evaluating [ctx]
  (remember-preserve-persist ctx))

(defspec evaluating-test
         100
         (let [seeker (one (gen-seeker-of 10))]
           (for-all [tctx (gen-context {:size 20
                                        :fov 7
                                        :seeker seeker
                                        :receive (evaluation seeker)})]
                    (evaluating tctx))))

;; VI. Rolling back

(defn roll-rebase-remember-back [ctx]
  (let [hist (history ctx)]
    (-> ctx
        (process prev-eval)
        (can-be #(<=> (:previous-hud %) (:complete-hud ctx))
                #(<=> (:seeker %) (first hist))))))

(defn roll-keep-clipboard [ctx]
  (-> ctx
      (process select-all)
      (process copy)
      (process prev-eval)
      (process prev-eval)
      (process select-all)
      (process backspace)
      (process paste)
      (:complete-hud)
      (<=> (:complete-hud ctx))
      (is)))

(defn rolling-back [ctx]
  (roll-keep-clipboard ctx)
  (roll-rebase-remember-back ctx))

(defspec rolling-back-test
         100
         (for-all [tctx (gen-context {:size 5
                                      :fov 27
                                      :history [(one (gen-seeker-of 32))]
                                      :seeker (one (gen-seeker-of 29))})]
                  (rolling-back tctx)))

;; VII. Rolling forward

(defn roll-rebase-remember-forward [ctx]
  (-> ctx
      (process evaluate)
      (process prev-eval)
      (process prev-eval)
      (process next-eval)
      (can-be
        #(<=> (:seeker %) (:seeker ctx))
        #(-> % (process next-eval) (:previous-hud) (<=> (:complete-hud %))))))

(defn rolling-forward [ctx]
  (roll-rebase-remember-forward ctx))

(defspec rolling-forward-test
         100
         (for-all [tctx (gen-context {:size 5
                                      :fov 27
                                      :history [(one (gen-seeker-of 32))]
                                      :seeker (one (gen-seeker-of 29))})]
                  (rolling-forward tctx)))

;; VIII. Suggesting

(defn suggestion-override [ctx]
  (let [suggestions (suggestions ctx)
        suggestion-at (fn [th] (-> (i/reset-y suggestions th) (i/line)))
        end (move-end-fov ctx)
        replaced-point (cursor end)]
    (can-be end
            #(-> % (process suggest) (:complete-hud) (i/move (fn [_] replaced-point)) (i/line) (= (suggestion-at 0)))
            #(-> % (process suggest 4) (:complete-hud) (i/move (fn [_] replaced-point)) (i/line) (= (suggestion-at 3)))
            #(-> % (process suggest 11) (:complete-hud) (i/move (fn [_] replaced-point)) (i/line) (= (suggestion-at 10)))
            #(-> % (process suggest 13) (:complete-hud) (i/move (fn [_] replaced-point)) (i/line) (= (suggestion-at 0)))
            #(-> % (process suggest 14) (:complete-hud) (i/move (fn [_] replaced-point)) (i/line) (= (suggestion-at 1))))))

(defn suggesting [ctx]
  (suggestion-override ctx))

(defspec suggesting-test
         100
         (for-all [tctx (gen-context {:size 20
                                      :fov 15
                                      :receive (one (gen-suggestions 12))
                                      :seeker (one (gen-seeker-of 17))})]
                  (suggesting tctx)))

;; IX. Highlighting

(defn queue-highlights [ctx]
  (let [top (-> (from-start ctx) (move-top-fov))
        bottom (-> (from-start ctx) (move-top-fov) (move-bottom-fov))
        [xt yt] (get-in top [:complete-hud :cursor])
        [xb yb] (get-in bottom [:complete-hud :cursor])]
    (can-be top
            #(-> (process % select-down 4)
                 (:highlights)
                 (contains? {:start [xt yt]
                             :end   [xt (+ yt 4)]}))
            #(-> (move-bottom-fov %)
                 (process select-up 4)
                 (:highlights)
                 (contains? {:start [xb (- yb 4)]
                             :end   [xb yb]})))))

(defn garbage-collect-highlights [ctx]
  (let [top (-> (from-start ctx) (move-top-fov))
        bottom (-> (from-start ctx) (move-top-fov) (move-bottom-fov))
        [xt yt] (get-in top [:complete-hud :cursor])
        [xb yb] (get-in bottom [:complete-hud :cursor])]
    (can-be top
            #(-> (process % select-down 4)
                 (process enter)
                 (:garbage)
                 (contains? {:start [xt yt]
                             :end   [xt (+ yt 4)]}))
            #(-> (move-bottom-fov %)
                 (process select-up 4)
                 (process enter)
                 (:garbage)
                 (contains? {:start [xb (- yb 4)]
                             :end   [xb yb]}))
            #(-> (process % select-down 4)
                 (process enter)
                 (:highlights)
                 (= h/empty-set))
            #(-> (move-bottom-fov %)
                 (process select-up 4)
                 (process enter)
                 (:highlights)
                 (= h/empty-set)))))

(defn highlighting [ctx]
  (queue-highlights ctx)
  (garbage-collect-highlights ctx))

(defspec highlighting-test
         100
         (for-all [tctx (gen-context {:size 20
                                      :fov 7
                                      :seeker (one (gen-seeker-of 10))})]
                  (highlighting tctx)))

;; X. Parenthesis matching

(defn highlight-matched [ctx]
  (let [[x y] (get-in ctx [:complete-hud :cursor])]
    (-> ctx
        (process (char-key \())
        (process (char-key \a) 4)
        (process left 5)
        (process parens-match)
        (:highlights)
        (can-be #(contains? % {:start [x y]
                               :end   [(inc x) y]})
                #(contains? % {:start [(+ x 5) y]
                               :end   [(+ x 6) y]})))))

(defn dont-highlight-unmatched [ctx]
  (-> ctx
      (process (char-key \())
      (process (char-key \a) 4)
      (process select-right)
      (process backspace)
      (process left 5)
      (process parens-match)
      (:highlights)
      (= h/empty-set)
      (is)))

(defn parens-matching [ctx]
  (highlight-matched ctx)
  (dont-highlight-unmatched ctx))

(defspec parens-matching-test
         100
         (for-all [tctx (gen-context {:size 20
                                      :fov 7
                                      :seeker (one (gen-seeker-of 10))})]
                  (parens-matching tctx)))

;; XI. Pop-ups

(defn pop-up-with-calibration [ctx content]
  (let [window  (->> content (i/height) (h/window content))
        context (-> ctx (move-top-fov) (process up 2))]
    (is (= 2 (-> context (h/pop-up-static window) (ov))))
    (is (= 2 (-> context (process down 2) (h/pop-up-static window) (ov))))
    (is (= 2 (-> context (move-bottom-fov) (h/pop-up-static window) (ov))))
    (is (= 1 (-> context (move-bottom-fov) (process down) (h/pop-up-static window) (ov))))))

(defn empty-pop-up-window [ctx]
  (let [window (h/window i/empty-seeker 10)
        expected (i/join-many h/delimiter i/empty-seeker h/delimiter)
        actual (-> ctx
                   (move-end-fov)
                   (h/pop-up-static window)
                   (:complete-hud)
                   (i/rebase #(take-last 2 %)))]
    (<=> expected actual)))

(defn pop-up-window [ctx content]
  (let [size (i/height content)
        expected (i/join-many h/delimiter (h/indent content 1) h/delimiter)
        actual (-> ctx
                   (move-end-fov)
                   (h/pop-up-static (h/window content size))
                   (:complete-hud)
                   (i/rebase #(take-last (+ size 2) %)))]
    (<=> expected actual)))

(defn pop-up-statically [ctx content]
  (let [window (h/window content (i/height content))
        expected-cursor (-> ctx :complete-hud :cursor)]
    (is (= expected-cursor
           (-> ctx
               (h/pop-up-static window)
               (:complete-hud)
               (:cursor))))
    (is (= expected-cursor
           (->> window
                (h/riffle)
                (h/pop-up-static ctx)
                (:complete-hud)
                (:cursor))))))

(defn pop-up-riffled [ctx content]
  (let [window (h/window content (- (i/height content) 2))
        [x _] (-> content (i/start) (i/end-x) (:cursor))
        [_ y] (-> ctx :complete-hud :cursor)
        line #(-> window (i/reset-y %) (h/indent 1) (i/line))]
    (is (= [(+ x 1) (+ y 2)]          ;; + x 1 because indentation
           (-> ctx
               (h/pop-up-riffle window)
               (:complete-hud)
               (:cursor))))
    (is (= (line 1)
           (->> window
                (h/riffle)
                (h/pop-up-riffle ctx)
                (:complete-hud)
                (i/line))))
    (is (= (line 4)
           (->> window
                (h/riffle)
                (h/riffle)
                (h/riffle)
                (h/riffle)
                (h/pop-up-riffle ctx)
                (:complete-hud)
                (i/line))))
    (is (= (line 0)
           (->> window
                (h/riffle)
                (h/riffle)
                (h/riffle)
                (h/riffle)
                (h/riffle)
                (h/pop-up-riffle ctx)
                (:complete-hud)
                (i/line))))))


(defn pop-ups [ctx]
  (empty-pop-up-window ctx)
  (pop-up-window ctx (one (gen-seeker-of 5)))
  (pop-up-with-calibration ctx (one (gen-seeker-of 5)))
  (pop-up-statically ctx (one (gen-seeker-of 5)))
  (pop-up-riffled ctx (one (gen-seeker-of 5))))

(defspec pop-up-test
         100
         (for-all [tctx (gen-context {:size 20
                                      :fov 15
                                      :seeker (one (gen-seeker-of 17))})]
                  (pop-ups tctx)))