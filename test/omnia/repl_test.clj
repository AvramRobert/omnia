(ns omnia.repl-test
  (:require [clojure.test :refer [is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :refer [for-all]]
            [clojure.test.check.generators :as gen]
            [omnia.test-utils :refer :all]
            [omnia.hud :as h]
            [omnia.repl :as r]
            [omnia.input :as i]
            [schema.core :as s]))

;; 0. Manipulation

(defn automatic-preview-refresh [ctx text]
  (let [some-input       (i/from-string "input")
        new-hud          (-> ctx (r/persisted-hud) (h/enrich-with [text]))
        expected-preview (-> new-hud (h/enrich-with [some-input]))
        actual-preview   (-> ctx
                             (r/with-text some-input)
                             (r/with-hud new-hud)
                             (r/preview-hud))]
    (is (= expected-preview actual-preview))))

(defn clipboard-propagation [ctx text]
  (let [expected-clipboard  (i/from-string "content")
        text-full-clip      (assoc text :clipboard expected-clipboard)
        actual-clipboard    (-> ctx
                                (process select-all)
                                (process copy)
                                (r/with-text text-full-clip)
                                (r/input-area)
                                (:clipboard))]
    (<=>seeker expected-clipboard actual-clipboard)))

(defn clipboard-preservation [ctx text]
  (let [text-empty-clip    (assoc text :clipboard nil)
        expected-clipboard (r/input-area ctx)
        actual-clipboard    (-> ctx
                               (process select-all)
                               (process copy)
                               (r/with-text text-empty-clip)
                               (r/input-area)
                               (:clipboard))]
    (<=>seeker expected-clipboard actual-clipboard)))

(s/defn manipulations [ctx :- r/Context, seeker :- i/Seeker]
  (automatic-preview-refresh ctx seeker)
  (clipboard-propagation ctx seeker)
  (clipboard-preservation ctx seeker))

(defspec manipulating-test 100
  (for-all [seeker  (gen-seeker-of 10)
            context (gen-context {:size   0
                                  :fov    7
                                  :seeker (one (gen-seeker-of 5))})]
           (manipulations context seeker)))

;; I. Calibrating

(defn exceed-upper-bound [ctx]
  (-> (at-main-view-start ctx)
      (can-be #(-> % (process up) (ov) (= 1))
              #(-> % (process up 2) (ov) (= 2))
              #(-> % (process up 3) (ov) (= 2)))))

(defn exceed-lower-bound [ctx]
  (-> (at-main-view-start ctx)
      (process up 2)
      (at-view-bottom)
      (can-be #(= (ov %) 2)
              #(-> % (process down) (ov) (= 1))
              #(-> % (process down 2) (ov) (= 0))
              #(-> % (process down 3) (ov) (= 0)))))

(defn exceed-lower-bound-non-incrementally [ctx]
  (-> (at-main-view-start ctx)
      (process up 2)
      (at-view-bottom)
      (update :seeker #(i/move % (fn [[x y]] [x (+ 2 y)])))
      (r/preserve)
      (r/rebase)
      (r/calibrate)
      (can-be #(= (ov %) 0))))

(defn exceed-upper-bound-non-incrementally [ctx]
  (-> (at-main-view-start ctx)
      (update :seeker #(i/move % (fn [[x y]] [x (- 2 y)])))
      (r/preserve)
      (r/rebase)
      (r/calibrate)
      (can-be #(= (ov %) 2))))

(defn scroll-upper-bound [ctx]
  (-> (at-main-view-start ctx)
      (process up)
      (process down 6)
      (can-be #(= (ov %) 1))))

(defn scroll-lower-bound [ctx]
  (-> (at-main-view-start ctx)
      (process up)
      (at-view-bottom)
      (process up 6)
      (can-be #(= (ov %) 1))))


(defn correct-under-deletion-top [ctx]
  (-> ctx
      (at-main-view-start)
      (process up 2)
      (can-be #(-> % (process select-down) (process backspace) (ov) (= 1))
              #(-> % (process select-down 2) (process backspace) (ov) (= 0))
              #(-> % (process select-down 3) (process backspace) (ov) (= 0)))))

(defn correct-under-deletion-end [ctx]
  (-> ctx
      (at-input-end)
      (at-line-start)
      (can-be #(= (ov %) 0)
              #(-> % (process up) (process select-down) (process backspace) (ov) (= 0))
              #(-> % (process up 2) (process select-down) (process backspace) (ov) (= 0)))))

(defn correct-under-deletion-bottom [ctx]
  (-> ctx
      (at-main-view-start)
      (process up 2)
      (at-view-bottom)
      (can-be #(= (ov %) 2)
              #(-> % (process up) (process select-down) (process backspace) (ov) (= 1))
              #(-> % (process select-up) (process backspace) (ov) (= 1))
              #(-> % (process up 2) (process select-down 2) (process backspace) (ov) (= 0))
              #(-> % (process select-down) (process backspace) (ov) (= 0)))))

(defn correct-under-deletion-in-multi-line [ctx]
  (-> ctx
      (at-main-view-start)
      (process up 2)
      (process down 4)
      (can-be #(-> % (process select-down) (process backspace) (ov) (= 1))
              #(-> % (process select-down 2) (process backspace) (ov) (= 0)))))

(defn correct-under-insertion-top [ctx]
  (-> ctx
      (at-main-view-start)
      (can-be #(-> % (process enter) (ov) (= 1))
              #(-> % (process enter) (process enter) (ov) (= 2)))))

(defn correct-under-insertion-bottom [ctx]
  (-> ctx
      (at-main-view-start)
      (process up 2)
      (at-view-bottom)
      (process enter 10)
      (can-be #(= (ov %) 2))))

(defn correct-under-insertion-end [ctx]
  (-> ctx
      (at-input-end)
      (at-line-start)
      (process enter 10)
      (can-be #(= (ov %) 0))))

(defn correct-under-insertion-in-multi-line [ctx]
  (-> (at-main-view-start ctx)
      (process up 2)
      (process down 3)
      (can-be #(= (ov %) 2)
              #(-> % (process enter) (ov) (= 3))
              #(-> % (process enter 2) (ov) (= 4)))))

(defn correct-under-multi-copied-insertion [ctx]
  (-> ctx
      (at-main-view-start)
      (process up 2)
      (at-view-bottom)
      (process select-up 2)
      (process copy)
      (can-be #(-> % (process paste) (ov) (= 4))
              #(-> % (process down 2) (process paste) (ov) (= 2)))))

(defn correct-under-multi-selected-deletion [ctx]
  (-> ctx
      (at-main-view-start)
      (process up 2)
      (process down 3)
      (process select-up 2)
      (process backspace)
      (can-be #(= (ov %) 0))))

(defn correct-under-change-variance [ctx]
  (-> ctx
      (at-main-view-start)
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
  (-> ctx
      (at-main-view-start)
      (process up 2)
      (process select-down 10)
      (process backspace)
      (can-be #(= (ov %) 0))))


(defn correct-under-hud-enlargement [ctx]
  (-> ctx
      (at-main-view-start)
      (can-be #(-> % (process up 2) (enlarge-by 1) (process down) (ov) (= 1))
              #(-> % (process up 2) (enlarge-by 2) (process down) (ov) (= 0))
              #(-> % (process select-all) (process backspace) (enlarge-by 2) (process down) (ov) (= 0)))))

(defn correct-under-hud-shrinking [ctx]
  (-> ctx
      (at-main-view-start)
      (can-be #(-> % (process up 2) (shrink-by 1) (process down) (ov) (= 3))
              #(-> % (process up 2) (shrink-by 2) (process down) (ov) (= 4))
              #(-> % (process up 2) (shrink-by 3) (process down) (ov) (= 5))
              #(-> % (process select-all) (process backspace) (shrink-by 3) (process down) (ov) (= 0)))))

(defn correct-under-hud-size-variance [ctx]
  (-> ctx
      (at-main-view-start)
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

(defspec calibrating-test 100
  (for-all [tctx (gen-context {:size   5
                               :fov    27
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
  (let [h (get-in ctx [:complete-hud :seeker :height])]
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
  (let [offset (-> (at-main-view-start ctx) (process up 2))
        ov     (get-in offset [:complete-hud :ov])
        fov    (get-in offset [:complete-hud :fov])]
    (-> offset
        (process scroll-up 3)
        (process scroll-down 5)
        (lor)
        (= (+ ov fov))
        (is))))

(defn scroll-reset [ctx]
  (let [expected-lor (get-in ctx [:complete-hud :lor])
        actual-lor (-> ctx
                       (at-input-end)
                       (at-line-start)
                       (process scroll-up 5)
                       (process (char-key \a))
                       (lor))]
    (is (= expected-lor actual-lor))))

(defn scroll-reset-with-ov [ctx]
  (let [fov (get-in ctx [:complete-hud :fov])]
    (-> (at-main-view-start ctx)
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
  (for-all [tctx (gen-context {:size   20
                               :fov    7
                               :seeker (one (gen-seeker-of 10))})]
           (scrolling tctx)))

;; III. Capturing

(defn capture-and-remember [ctx]
  (-> ctx
      (at-input-end)
      (at-line-start)
      (process (char-key \a))
      (can-be #(-> (:complete-hud %) (:seeker) (i/left) (= \a))
              #(-> (:seeker %) (i/left) (= \a))
              #(-> (:previous-hud %) (<=>hud (:complete-hud ctx))))))

(defn capturing [ctx]
  (capture-and-remember ctx))

(defspec capturing-test
         100
  (for-all [tctx (gen-context {:size   20
                               :fov    7
                               :seeker (one (gen-seeker-of 10))})]
           (capturing tctx)))

;; IV. Clearing

(defn clear-remember-persist [ctx]
  (let [expected-persisted (r/init-hud (:terminal ctx) (:repl ctx))
        expected-complete  (update expected-persisted :seeker #(i/conjoin % (:seeker ctx)))]
    (-> ctx
        (process clear)
        (can-be #(<=>hud (:complete-hud %) expected-complete)
                #(<=>hud (:persisted-hud %) expected-persisted)
                #(<=>hud (:previous-hud %) (:complete-hud ctx))))))

(defn clearing [ctx]
  (clear-remember-persist ctx))

(defspec clearing-test
         100
  (for-all [tctx (gen-context {:size   20
                               :fov    7
                               :seeker (one (gen-seeker-of 10))})]
           (clearing tctx)))

;; V. Evaluating

(defn remember-preserve-persist [ctx]
  (let [current-input      (r/input-area ctx)
        expected-previous  (-> ctx (r/preview-hud) (h/text))
        expected-persisted (-> ctx
                               (r/persisted-hud)
                               (h/text)
                               (i/conjoin current-input i/empty-line h/caret))
        expected-complete  (-> expected-persisted (i/conjoin i/empty-line))]
    (-> ctx
        (process evaluate)
        (can-be #(-> % (h/text) (= i/empty-line))
                #(-> % (r/previous-hud) (h/text) (= expected-previous))
                #(-> % (r/persisted-hud) (h/text) (= expected-persisted))
                #(-> % (r/preview-hud) (h/text) (= expected-complete))))))

(s/defn evaluating [ctx :- r/Context]
  (remember-preserve-persist ctx))

(defspec evaluating-test
         100
  (let [seeker (one (gen-seeker-of 10))]
    (for-all [tctx (gen-context {:size    20
                                 :fov     7
                                 :seeker  seeker
                                 :receive (evaluation seeker)})]
             (evaluating tctx))))

;; VI. Rolling back

(defn roll-rebase-remember-back [ctx]
  (let [hist (history ctx)]
    (-> ctx
        (process prev-eval)
        (can-be #(<=>hud (:previous-hud %) (:complete-hud ctx))
                #(<=>seeker (:seeker %) (first hist))))))

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
      (<=>hud (:complete-hud ctx))
      (is)))

(defn rolling-back [ctx]
  (roll-keep-clipboard ctx)
  (roll-rebase-remember-back ctx))

(defspec rolling-back-test
         100
  (for-all [tctx (gen-context {:size    5
                               :fov     27
                               :history [(one (gen-seeker-of 32))]
                               :seeker  (one (gen-seeker-of 29))})]
           (rolling-back tctx)))

;; VII. Rolling forward

(defn roll-rebase-remember-forward [ctx]
  (-> ctx
      (process evaluate)
      (process prev-eval)
      (process prev-eval)
      (process next-eval)
      (can-be
        #(<=>seeker (:seeker %) (:seeker ctx))
        #(-> % (process next-eval) (:previous-hud) (<=>hud (:complete-hud %))))))

(defn rolling-forward [ctx]
  (roll-rebase-remember-forward ctx))

(defspec rolling-forward-test
         100
  (for-all [tctx (gen-context {:size    5
                               :fov     27
                               :history [(one (gen-seeker-of 32))]
                               :seeker  (one (gen-seeker-of 29))})]
           (rolling-forward tctx)))

;; VIII. Suggesting

(defn suggestion-override [ctx]
  (let [suggestions    (suggestions ctx)
        suggestion-at  (fn [th] (-> (i/reset-y suggestions th) (i/line)))
        end            (-> ctx (at-input-end) (at-line-start))
        replaced-point (cursor end)]
    (can-be end
            #(-> % (process suggest) (:complete-hud) (:seeker) (i/reset-to replaced-point) (i/line) (= (suggestion-at 0)))
            #(-> % (process suggest 4) (:complete-hud) (:seeker) (i/reset-to replaced-point) (i/line) (= (suggestion-at 3)))
            #(-> % (process suggest 11) (:complete-hud) (:seeker) (i/reset-to replaced-point) (i/line) (= (suggestion-at 10)))
            #(-> % (process suggest 13) (:complete-hud) (:seeker) (i/reset-to replaced-point) (i/line) (= (suggestion-at 0)))
            #(-> % (process suggest 14) (:complete-hud) (:seeker) (i/reset-to replaced-point) (i/line) (= (suggestion-at 1))))))

(defn suggesting [ctx]
  (suggestion-override ctx))

(defspec suggesting-test
         100
  (for-all [tctx (gen-context {:size    20
                               :fov     15
                               :receive (one (gen-suggestions 12))
                               :seeker  (one (gen-seeker-of 17))})]
           (suggesting tctx)))

(defn no-override [ctx]
  (let [end            (-> ctx (at-input-end) (at-line-start))
        current-line   (-> end (:complete-hud) (:seeker) (i/line))
        current-cursor (-> end (:complete-hud) (:seeker) (:cursor))]
    (-> end
        (process suggest)
        (:complete-hud)
        (:seeker)
        (i/reset-to current-cursor)
        (i/line)
        (= current-line))))

(defn empty-suggesting [ctx]
  (no-override ctx))

(defspec empty-suggesting-test
         100
  (for-all [tctx (gen-context {:size    20
                               :fov     15
                               :receive i/empty-seeker
                               :seeker  (one (gen-seeker-of 17))})]
           (empty-suggesting tctx)))

;; IX. Highlighting

(defn queue-highlights [ctx]
  (let [top    (-> ctx (at-main-view-start))
        bottom (-> ctx (at-main-view-end) (at-line-start))
        [xt yt] (get-in top [:complete-hud :seeker :cursor])
        [xb yb] (get-in bottom [:complete-hud :seeker :cursor])]
    (can-be top
            #(-> (process % select-down 4)
                 (:highlights)
                 (:selection)
                 (highlights? {:start [xt yt]
                               :end   [xt (+ yt 4)]}))
            #(-> (at-view-bottom %)
                 (process select-up 4)
                 (:highlights)
                 (:selection)
                 (highlights? {:start [xb (- yb 4)]
                               :end   [xb yb]})))))

(defn garbage-collect-highlights [ctx]
  (let [top    (-> ctx (at-main-view-start))
        bottom (-> ctx (at-main-view-end) (at-line-start))
        [xt yt] (get-in top [:complete-hud :seeker :cursor])
        [xb yb] (get-in bottom [:complete-hud :seeker :cursor])]
    (can-be top
            #(-> (process % select-down 4)
                 (process enter)
                 (:garbage)
                 (:selection)
                 (highlights? {:start [xt yt]
                               :end   [xt (+ yt 4)]}))
            #(-> (at-view-bottom %)
                 (process select-up 4)
                 (process enter)
                 (:garbage)
                 (:selection)
                 (highlights? {:start [xb (- yb 4)]
                               :end   [xb yb]}))
            #(-> (process % select-down 4)
                 (process enter)
                 (:highlights)
                 (= i/empty-map))
            #(-> (at-view-bottom %)
                 (process select-up 4)
                 (process enter)
                 (:highlights)
                 (= i/empty-map)))))

(defn highlighting [ctx]
  (queue-highlights ctx)
  (garbage-collect-highlights ctx))

(defspec highlighting-test
         100
  (for-all [tctx (gen-context {:size   20
                               :fov    7
                               :seeker (one (gen-seeker-of 10))})]
           (highlighting tctx)))

;; X. Parenthesis matching

(defn highlight-matched [ctx]
  (let [[x y] (get-in ctx [:complete-hud :seeker :cursor])
        scheme            (fn [region]
                            {:region region
                             :scheme (-> ctx (:config) (:syntax) (:clean-up))
                             :styles [:underline]})
        actual-highlights (-> ctx
                              (process (char-key \())
                              (process (char-key \a) 4)
                              (process left 5)
                              (process parens-match)
                              (:highlights))]
    (is (= (:open-paren actual-highlights)
           (scheme {:start [x y]
                    :end   [(inc x) y]})))
    (is (= (:closed-paren actual-highlights)
           (scheme {:start [(+ x 5) y]
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
      (= i/empty-map)
      (is)))

(defn parens-matching [ctx]
  (highlight-matched ctx)
  (dont-highlight-unmatched ctx))

(defspec parens-matching-test
         100
  (for-all [tctx (gen-context {:size   20
                               :fov    7
                               :seeker (one (gen-seeker-of 10))})]
           (parens-matching tctx)))

;; XI. Pop-ups

(defn pop-up-with-calibration [ctx content]
  (let [window  (->> content (:height) (h/riffle-window content))
        context (-> ctx (at-main-view-start) (process up 2))]
    (is (= 2 (-> context (pop-up window) (:ov))))
    (is (= 2 (-> context (process down 2) (pop-up window) (h/overview))))
    (is (= 2 (-> context (at-view-bottom) (pop-up window) (h/overview))))
    (is (= 1 (-> context (at-view-bottom) (process down) (pop-up window) (h/overview))))))


(defn pop-up-window [ctx content]
  (let [content-size (:height content)
        window       (h/riffle-window content content-size)
        text         (r/input-area ctx)
        pop-up-size  (+ content-size 2)
        expected     (-> (i/conjoin text r/delimiter (i/indent content 1) r/delimiter)
                         (i/rebase #(take-last pop-up-size %)))
        actual       (-> ctx
                         (at-input-end)
                         (at-line-start)
                         (pop-up window)
                         (h/text)
                         (i/rebase #(take-last pop-up-size %)))]
    (is (= (-> actual (i/start) (i/line)) (i/line r/delimiter)))
    (is (= (-> actual (i/end) (i/line)) (i/line r/delimiter)))
    (<=>seeker expected actual)))

(defn empty-pop-up-window [ctx]
  (pop-up-window ctx i/empty-seeker))

(defn framed-pop-up [ctx]
  (pop-up-window ctx (one (gen-seeker-of 5))))

(defn pop-up-riffled [ctx content]
  (let [content-size (:height content)
        window       (h/riffle-window content content-size)
        preview      (r/preview-hud ctx)
        line         #(-> window (h/text) (i/reset-y %) (i/indent 1) (i/line))
        riffled-line #(->> window
                           (iterate h/riffle)
                           (take (inc %))
                           (last)
                           (h/pop-up preview)
                           (h/text)
                           (i/line))]
    (is (= (line 0) (riffled-line 0)))
    (is (= (line 1) (riffled-line 1)))
    (is (= (line 4) (riffled-line 4)))
    (is (= (line 0) (riffled-line 5)))))

(defn pop-ups [ctx]
  (empty-pop-up-window ctx)
  (framed-pop-up ctx)
  (pop-up-with-calibration ctx (one (gen-seeker-of 5)))
  (pop-up-riffled ctx (one (gen-seeker-of 5))))

(defspec pop-up-test
         100
  (for-all [tctx (gen-context {:size   20
                               :fov    15
                               :seeker (one (gen-seeker-of 17))})]
           (pop-ups tctx)))


(defn unchanged-highlights [ctx]
  (-> ctx
      (process (char-key \())
      (process right)
      (process ignore)
      (:highlights)
      (can-be #(not (nil? (:open-paren %))))))

(defn unchanged-selection [ctx]
  (-> ctx
      (at-input-start)
      (process select-right 12)
      (process ignore)
      (:highlights)
      (can-be #(not (nil? (:selection %))))))

(defn unchanged-scrolling [ctx]
  (let [offset       2
        initial-lor  (-> ctx (:complete-hud) (:lor))
        expected-lor (+ offset initial-lor)
        actual-lor   (-> ctx
                         (at-input-end)
                         (process scroll-up offset)
                         (process ignore)
                         (lor))]
    (is (= expected-lor actual-lor))))

(defn ignores [ctx]
  (unchanged-highlights ctx)
  (unchanged-selection ctx)
  (unchanged-scrolling ctx))

(defspec ignoring-test
         100
  (for-all [tctx (gen-context {:fov    10
                               :seeker (one (gen-seeker-of 17))})]
           (ignores tctx)))