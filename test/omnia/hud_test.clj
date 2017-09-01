(ns omnia.hud-test
  (require [clojure.test :refer [is]]
           [clojure.test.check.clojure-test :refer [defspec]]
           [clojure.test.check.properties :refer [for-all]]
           [omnia.test-utils :refer :all]
           [clojure.test.check.generators :as gen]
           [omnia.config :refer [default-keymap]]
           [omnia.highlight :refer [default-cs]]
           [omnia.more :refer [--]]
           [omnia.hud :as h]
           [omnia.rendering :as rd]
           [omnia.repl :as r]
           [omnia.input :as i]))

(defprotocol TRowed
  (getRows [_]))

(defprotocol TSized
  (getTerminalSize [_]))

(defn test-terminal [size]
  (reify TSized
    (getTerminalSize [_]
      (reify TRowed
        (getRows [_] size)))))

(def ctx (h/context {:terminal nil
                     :repl nil
                     :keymap default-keymap
                     :colourscheme default-cs}))

(defn gen-context [{:keys [size fov seeker suggestions history]
                    :or {fov 10
                         suggestions i/empty-seeker
                         history []
                         seeker (:seeker ctx)}}]
  (assert (not (nil? size)) "A context must always have a hud size")
  (gen/fmap
    (fn [hud-seeker]
      (let [hud (h/hud fov hud-seeker)]
        (-> ctx
            (assoc :terminal (test-terminal fov)
                   :repl (-> (r/repl {:kind :identity
                                      :history history})
                             (assoc :complete-f (constantly suggestions)))
                   :seeker seeker
                   :complete-hud hud
                   :persisted-hud hud
                   :previous-hud hud)
            (h/rebase seeker)
            (h/remember)))) (gen-seeker-of size)))

;; I. Calibrating

(defn event [action key]
  (i/->Event action key))

(def up (event :up :up))
(def down (event :down :down))
(def left (event :left :left))
(def right (event :right :right))
(def select-all (event :select-all \a))
(def select-down (event :select-down :down))
(def select-up (event :select-up :up))
(def select-right (event :select-right :right))
(def select-left (event :select-left :left))
(def copy (event :copy \c))
(def paste (event :paste \v))
(def backspace (event :backspace :backspace))
(def enter (event :enter :enter))
(def scroll-up (event :scroll-up :page-up))
(def scroll-down (event :scroll-down :page-down))
(defn char-key [k] (event :char k))
(def clear (event :clear \r))
(def evaluate (event :eval \e))
(def prev-eval (event :prev-eval :up))
(def next-eval (event :next-eval :down))
(def parens-match (event :match \p))
(def suggest (event :suggest :tab))

(defn process
  ([ctx event]
    (process ctx event 1))
  ([ctx event n]
   (->> (range 0 n)
        (reduce (fn [nctx _] (second (h/process nctx event))) ctx))))

(defn fov [ctx]
  (get-in ctx [:complete-hud :fov]))

(defn ov [ctx]
  (get-in ctx [:complete-hud :ov]))

(defn lor [ctx]
  (get-in ctx [:complete-hud :lor]))

(defn project-hud [ctx]
  (rd/project-hud (:complete-hud ctx)))

(defn project-cursor [ctx]
  (rd/project-cursor (:complete-hud ctx)))

(defn project-selection [ctx]
  (let [fov (get-in ctx [:complete-hud :fov])
        selection (first (:highlights ctx))]
    (rd/project-selection selection fov)))

(defn make-total [ctx]
  (let [h (get-in ctx [:complete-hud :height])]
    (-> ctx
        (assoc :terminal (test-terminal h))
        (assoc-in [:persisted-hud :fov] h)
        (assoc-in [:persisted-hud :lor] h)
        (h/rebase)
        (h/remember))))

(defn cursor [ctx]
  (get-in ctx [:complete-hud :cursor]))

(defn suggestions [ctx]
  ((get-in ctx [:repl :complete-f])))

(defn history [ctx]
  (get-in ctx [:repl :history]))

(defn move-end-fov [ctx]
  (->> (update ctx :seeker (comp i/start-x i/end))
       (h/rebase)
       (h/remember)))

(defn move-top-fov [ctx]
  (let [fov (get-in ctx [:complete-hud :fov])
        top #(-- % (dec fov))]                              ;; (dec) because you want to land on the fov'th line
    (-> (move-end-fov ctx)
        (update :seeker #(i/move-y % top))
        (h/rebase)
        (h/remember))))

(defn move-bottom-fov [ctx]
  (let [fov (get-in ctx [:complete-hud :fov])
        bottom #(+ % (dec fov))]
    (-> (update ctx :seeker #(i/move-y % bottom))
        (h/rebase)
        (h/remember))))

(defn from-start [ctx]
  (-> ctx
      (update :persisted-hud i/start-x)
      (update :seeker i/start-x)
      (h/rebase)
      (h/remember)))


(defn from-end [ctx]
  (-> ctx
      (update :persisted-hud i/end-x)
      (update :seeker i/end-x)
      (h/rebase)
      (h/remember)))

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
  (correct-under-rebounded-deletion ctx))

(defspec calibrating-test
         100
         (for-all [tctx (gen-context {:size 5
                                      :fov 27
                                      :seeker (just-one (gen-seeker-of 29))})]
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
  (scroll-downwards ctx (just-one gen/pos-int))
  (stop-upward-scroll ctx)
  (stop-downward-scroll ctx)
  (scroll-ending-with-ov ctx)
  (scroll-reset ctx)
  (scroll-reset-with-ov ctx))

(defspec scrolling-test
         100
         (for-all [tctx (gen-context {:size 20
                                      :fov 7
                                      :seeker (just-one (gen-seeker-of 10))})]
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
                                      :seeker (just-one (gen-seeker-of 10))})]
                  (capturing tctx)))

;; IV. Clearing

(defn clear-remember-persist [some-ctx]
  (-> some-ctx
      (process clear)
      (<=> ctx)
      (is)))

(defn clearing [ctx]
  (clear-remember-persist ctx))

(defspec clearing-test
         100
         (for-all [tctx (gen-context {:size 20
                                      :fov 7
                                      :seeker (just-one (gen-seeker-of 10))})]
                  (clearing tctx)))

;; V. Evaluating

(defn remember-preserve-persist [ctx]
  (let [expected-hud (-> (:complete-hud ctx)
                         (i/join-many (:seeker ctx) h/caret i/empty-seeker))]
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
         (for-all [tctx (gen-context {:size 20
                                      :fov 7
                                      :seeker (just-one (gen-seeker-of 10))})]
                  (evaluating tctx)))

;; VI. Rolling back

(defn roll-rebase-remember-back [ctx]
  (let [hist (history ctx)]
    (-> ctx
        (process prev-eval)
        (can-be #(<=> (:previous-hud %) (:complete-hud ctx))
                #(<=> (:seeker %) (first hist))))))

(defn rolling-back [ctx]
  (roll-rebase-remember-back ctx))

(defspec rolling-back-test
         100
         (for-all [tctx (gen-context {:size 5
                                      :fov 27
                                      :history [(just-one (gen-seeker-of 32))]
                                      :seeker (just-one (gen-seeker-of 29))})]
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
                                      :history [(just-one (gen-seeker-of 32))]
                                      :seeker (just-one (gen-seeker-of 29))})]
                  (rolling-forward tctx)))

;; VIII. Suggesting

(defn suggestion-page [ctx]
  (let [suggestions (-> (suggestions ctx)
                        (i/rebase #(take 10 %))
                        (i/join h/continuation)
                        (i/rebase #(mapv (partial cons \space) %))
                        (i/join-many h/delimiter)
                        (->> (i/join h/delimiter)))]
    (-> (move-end-fov ctx)
        (process suggest)
        (:complete-hud)
        (i/rebase #(take-last 13 %))
        (<=> suggestions))))

(defn suggestion-riffle [ctx]
  (let [suggestions (suggestions ctx)
        suggestion-at (fn [th] (-> (i/reset-y suggestions th)
                                   (i/start-x)
                                   (i/slicer #(cons \space %))
                                   (i/line)))]
    (-> (move-end-fov ctx)
        (can-be #(-> % (process suggest) (:complete-hud) (i/line) (= (suggestion-at 0))) ;; starts from 0
                #(-> % (process suggest 4) (:complete-hud) (i/line) (= (suggestion-at 3)))
                #(-> % (process suggest 11) (:complete-hud) (i/line) (= (suggestion-at 10)))
                #(-> % (process suggest 13) (:complete-hud) (i/line) (= (suggestion-at 0)))
                #(-> % (process suggest 14) (:complete-hud) (i/line) (= (suggestion-at 1)))))))

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

(defn suggestion-with-calibration [ctx]
  (-> (move-top-fov ctx)
      (process up 2)
      (can-be #(-> % (process suggest) (ov) (= 2))
              #(-> % (process down 3) (process suggest) (ov) (= 2))
              #(-> (move-bottom-fov %) (process suggest) (ov) (= 2))
              #(-> (move-bottom-fov %) (process down) (process suggest) (ov) (= 1)))))

;; FIXME: Add some test and handler for when the suggestion window > fov?
(defn suggesting [ctx]
  (suggestion-page ctx)
  (suggestion-riffle ctx)
  (suggestion-override ctx)
  (suggestion-with-calibration ctx))

(defspec suggesting-test
         100
         (for-all [tctx (gen-context {:size 20
                                      :fov 15
                                      :suggestions (just-one (gen-seeker-of 12))
                                      :seeker (just-one (gen-seeker-of 17))})]
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
                                      :seeker (just-one (gen-seeker-of 10))})]
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
                                      :seeker (just-one (gen-seeker-of 10))})]
                  (parens-matching tctx)))

;; XI. Hud projection

(defn move-with [ctx {:keys [scroll ov]
                      :or {scroll 0
                           ov     0}}]
  (let [fov (get-in ctx [:complete-hud :fov])]
    (i/rebase (:complete-hud ctx) #(->> (take-last (+ fov scroll ov) %)
                                        (take fov)))))

(defn total-hud-projection [ctx]
  (let [total (make-total ctx)]
    (-> total
        (process scroll-up 10)
        (project-hud)
        (<=> (:complete-hud total)))))

(defn scrolled-hud-projection [ctx]
  (can-be ctx
          #(-> % (process scroll-up 1) (project-hud) (<=> (move-with % {:scroll 1})))
          #(-> % (process scroll-up 4) (project-hud) (<=> (move-with % {:scroll 4})))
          #(-> % (process scroll-up 4) (process scroll-down) (project-hud) (<=> (move-with % {:scroll 3})))))

(defn paged-hud-projection [ctx]
  (-> (move-top-fov ctx)
      (process up 2)
      (can-be
        #(-> % (process scroll-up 1) (project-hud) (<=> (move-with % {:scroll 1 :ov 2})))
        #(-> % (process scroll-up 4) (project-hud) (<=> (move-with % {:scroll 4 :ov 2})))
        #(-> % (process scroll-up 4) (process scroll-down) (project-hud) (<=> (move-with % {:scroll 3 :ov 2}))))))

(defn hud-projection [ctx]
  (total-hud-projection ctx)
  (scrolled-hud-projection ctx)
  (paged-hud-projection ctx))

(defspec hud-projection-test
         100
         (for-all [tctx (gen-context {:size 20
                                      :fov 7
                                      :seeker (just-one (gen-seeker-of 10))})]
                  (hud-projection tctx)))

;; XII. Cursor projection

(defn total-cursor-projection [ctx]
  (let [total (move-end-fov (make-total ctx))
        [x y] (cursor total)
        hp (get-in total [:persisted-hud :height])
        hc (get-in total [:complete-hud :height])]
    (can-be total
            #(-> % (process up) (project-cursor) (= [x (dec y)]))
            #(-> % (process up 4) (project-cursor) (= [x (- y 4)]))
            #(-> % (process up 100) (project-cursor) (= [x hp]))
            #(-> % (process down 100) (project-cursor) (= [x (dec hc)]))))) ;; starts with 0

(defn paged-cursor-projection [ctx]
  (let [end-y (dec (fov ctx))]                              ;; starts from 0
    (-> (move-top-fov ctx)
        (from-start)
        (can-be
          #(-> % (process up) (project-cursor) (= [0 0]))
          #(-> % (process up 2) (project-cursor) (= [0 0]))
          #(-> % (process down 1) (project-cursor) (= [0 1]))
          #(-> % (process down 2) (project-cursor) (= [0 2]))
          #(-> % (move-bottom-fov) (process down) (project-cursor) (= [0 end-y]))
          #(-> % (move-bottom-fov) (process down 2) (project-cursor) (= [0 end-y]))))))

(defn cursor-projection [ctx]
  (total-cursor-projection ctx)
  (paged-cursor-projection ctx))

(defspec cursor-projection-test
         100
         (for-all [tctx (gen-context {:size 20
                                      :fov 7
                                      :seeker (just-one (gen-seeker-of 10))})]
                  (cursor-projection tctx)))

;; XIII. Selection projection

(defn total-selection-projection [ctx]
  (let [total (move-end-fov (make-total ctx))
        [x y] (cursor total)]
    (-> total
        (process select-up 5)
        (:highlights)
        (contains? {:start [x (- y 5)]
                    :end   [x y]})
        (is))))

(defn paged-selection-projection [ctx]
  (let [start (move-top-fov ctx)
        [_ y-start] (cursor start)
        end-y (+ y-start (-> start (fov) (dec)))            ;; starts from 0
        [end-x _] (-> (move-end-fov ctx) (:complete-hud) (i/end-x) (:cursor))]
    (can-be start
            #(-> % (process select-all) (project-selection) (= {:start [0 y-start] :end [end-x end-y]}))
            #(-> (process % up 2)
                 (process select-down 100)
                 (process select-right 10)
                 (project-selection)
                 (= {:start [0 y-start]
                     :end   [end-x end-y]})))))

; selection is currently reset when scrolling
;(defn scrolled-selection-projection [ctx] true)

(defn selection-projection [ctx]
  (total-selection-projection ctx)
  (paged-selection-projection ctx))

(defspec selection-projection-test
         100
         (for-all [tctx (gen-context {:size 20
                                      :fov 7
                                      :seeker (just-one (gen-seeker-of 10))})]
                  (selection-projection tctx)))

;; ---- STATIC DATA ----

(def static-seeker (i/end
                     (i/seeker [[\a]
                                [\b]
                                [\c]
                                [\d]
                                [\e]
                                [\f]
                                [\g]
                                [\h]
                                [\i]
                                [\j]
                                [\k]
                                [\l]
                                [\m]
                                [\n]
                                [\o]
                                [\p]
                                [\q]
                                [\r]
                                [\s]
                                [\t]
                                [\u]
                                [\v]
                                [\w]
                                [\x]
                                [\y]
                                [\z]
                                [\i \t \s \y]
                                [\b \i \t \s \y]
                                [\s \p \i \d \e \r]])))

(def static-ctx (-> ctx
                    (assoc :terminal (test-terminal 27))
                    (update :complete-hud #(-> (i/join % static-seeker)
                                               (assoc :fov 27
                                                      :lor 27)))
                    (update :previous-hud #(-> (i/join % static-seeker)
                                               (assoc :fov 27
                                                      :lor 27)))
                    (update :persisted-hud #(-> (assoc % :fov 27
                                                         :lor 27)))
                    (assoc :seeker static-seeker)))