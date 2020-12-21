(ns omnia.render-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :refer [for-all]]
            [clojure.test.check.generators :as gen]
            [omnia.test-utils :refer :all]
            [omnia.render :refer :all]
            [omnia.more :refer [map-vals reduce-idx]]
            [omnia.context :as r]
            [omnia.hud :as h]
            [omnia.terminal :as t]
            [omnia.input :as i]
            [schema.core :as s])
  (:import (clojure.lang Atom)))

(def State
  {:chars   Atom
   :cursors Atom
   :bgs     Atom
   :fgs     Atom
   :stls    Atom})

(def Accumulate
  {:ctx r/Context
   :state State})

(s/defn accumulative :- Accumulate
  [ctx :- r/Context]
  (let [chars   (atom [])
        cursors (atom [])
        bgs     (atom [])
        fgs     (atom [])
        stls    (atom [])
        acc     (fn [atm val] (swap! atm #(conj % val)))
        size    (t/size (:terminal ctx))]
    {:ctx (assoc ctx
            :terminal (test-terminal {:put! (fn [ch x y fg bg stl]
                                              (acc bgs bg)
                                              (acc fgs fg)
                                              (run! #(acc stls %) stl)
                                              (acc chars ch)
                                              (acc cursors [x y]))
                                      :size (fn [] size)}))
     :state {:chars   chars
             :cursors cursors
             :bgs     bgs
             :fgs     fgs
             :stls    stls}}))

(s/defn inspect
  [acc :- Accumulate p]
  (->> acc (:state) (map-vals deref) (p)))

(s/defn execute :- Accumulate
  [acc :- Accumulate f]
  (f (:ctx acc))
  acc)

(defn index [ctx]
  (-> (project-preview ctx)
      (i/rebase #(map-indexed
                   (fn [y line]
                     (map-indexed
                       (fn [x c]
                         {:cursor [x y] :char c}) line)) %))))

(defn aggregate [traversal]
  (->> (:lines traversal)
       (reduce concat)
       (reduce
         (fn [res item]
           (-> res
               (update :cursors #(conj % (:cursor item)))
               (update :chars #(conj % (:char item))))) {:cursors [] :chars []})))

(defn discretise [ctx]
  "Discretises the `complete-hud` into a vector of chars and a vector of cursors,
   each being the ones the `terminal` prints to the screen."
  (-> (index ctx)
      (aggregate)))

(defn discretise-h [{:keys [complete-hud highlights] :as ctx}]
  "Discretises the `complete-hud` into a vector of chars and a vector of cursors,
   each being the ones the `terminal` highlights on the screen."
  (->> (prioritise highlights)
       (vals)
       (mapv #(let [{start :start
                     end   :end} (-> complete-hud
                                   (h/project-selection (:region %))
                                   (update :start (fn [[x y]] [x (h/project-y complete-hud y)]))
                                   (update :end (fn [[x y]] [x (h/project-y complete-hud y)])))]
                (-> (index ctx)
                    (assoc :cursor start
                           :selection end)
                    (i/extract)
                    (aggregate))))
       (apply (partial merge-with concat))))

;; I. Diffed rendering

(defn padded-diff-render [ctx]
  (let [selected         (-> ctx
                             (at-input-end)
                             (at-line-start)
                             (process select-right 100))
        processed        (process selected backspace)
        n-last           (-> ctx (r/preview-hud) (h/text) (:lines) (last) (count))
        expected-chars   (repeat n-last \space)
        expected-cursors (->> (discretise selected)
                              (:cursors)
                              (take-last n-last))]
    (-> processed
        (accumulative)
        (execute render-diff!)
        (inspect
          (fn [{:keys [chars cursors fgs bgs stls]}]
            (is (not (empty? bgs)))
            (is (not (empty? fgs)))
            (is (empty? stls))
            (is (= expected-cursors cursors))
            (is (= expected-chars chars)))))))

(defn projected-diff-render [ctx k]
  (let [processed        (-> ctx
                             (at-input-end)
                             (at-line-start)
                             (process (char-key k) 10))
        last-n           (-> processed (r/preview-hud) (h/text) (:lines) (last) (count))
        {chars   :chars
         cursors :cursors} (discretise processed)
        expected-chars   (take-last last-n chars)
        expected-cursors (take-last last-n cursors)]
    (-> (accumulative processed)
        (execute render-diff!)
        (inspect
          (fn [{:keys [chars cursors fgs bgs stls]}]
            (is (not (empty? bgs)))
            (is (not (empty? fgs)))
            (is (empty? stls))
            (is (= expected-cursors cursors))
            (is (= expected-chars chars)))))))

(defn no-change-diff-render [ctx]
  (-> ctx
      (at-input-end)
      (at-line-start)
      (process up)
      (accumulative)
      (execute render-diff!)
      (inspect
        (fn [{:keys [chars cursors fgs bgs stls]}]
          (is (empty? chars))
          (is (empty? cursors))
          (is (empty? fgs))
          (is (empty? bgs))
          (is (empty? stls))))))

(defn reset-diff-render [ctx]
  (let [processed (-> ctx
                      (at-main-view-start)
                      (process down)
                      (process select-up 3)
                      (process backspace))
        {expected-chars   :chars
         expected-cursors :cursors} (discretise processed)]
    (-> processed
        (accumulative)
        (execute render-diff!)
        (inspect
          (fn [{:keys [chars cursors _ _ _ _]}]
            (->> (map vector chars cursors)
                 (filter (fn [[char cursor]] (not= \space char)))
                 (map
                   (fn [e-char e-cursor [char cursor]]
                     [e-char e-cursor char cursor]) expected-chars expected-cursors)
                 (reduce
                   (fn [_ [e-char e-cursor char cursor]]
                     (is (= e-char char))
                     (is (= e-cursor cursor))) nil)))))))

(defn diff-render [ctx]
  (projected-diff-render ctx (one gen/char-alpha))
  (padded-diff-render ctx)
  (no-change-diff-render ctx)
  (reset-diff-render ctx))

(defspec diff-render-test
         100
         (for-all [ctx (gen-context {:size   5
                                     :fov    27
                                     :seeker (one (gen-seeker-of 29))})]
                  (diff-render ctx)))

;; II. No rendering

(defn projected-no-render [ctx]
  (-> (accumulative ctx)
      (execute nothing!)
      (inspect
        (fn [{:keys [chars cursors fgs bgs stls]}]
          (is (empty? bgs))
          (is (empty? fgs))
          (is (empty? chars))
          (is (empty? cursors))
          (is (empty? stls))))))

(defn nothing-to-diff-render [ctx]
  (let [processed (-> ctx
                      (at-main-view-start)
                      (process up))]
    (-> (accumulative processed)
        (execute nothing!)
        (inspect
          (fn [{:keys [chars cursors fgs bgs stls]}]
            (is (not (empty? bgs)))
            (is (not (empty? fgs)))
            (is (empty? stls))
            (is (not (empty? chars)))
            (is (not (empty? cursors))))))))

(defn no-render [ctx]
  (projected-no-render ctx)
  (nothing-to-diff-render ctx))

(defspec no-render-test
         100
         (for-all [ctx (gen-context {:size   5
                                     :fov    27
                                     :seeker (one (gen-seeker-of 29))})]
                  (no-render ctx)))

;; III. Selection highlighting

(defn total-selection-render-right [ctx]
  (let [processed (-> ctx
                      (at-input-end)
                      (at-line-start)
                      (process select-right))
        {expected-chars   :chars
         expected-cursors :cursors} (discretise-h processed)]
    (-> (accumulative processed)
        (execute render-highlights!)
        (inspect
          (fn [{:keys [chars cursors bgs fgs stls]}]
            (is (not (empty? bgs)))
            (is (not (empty? fgs)))
            (is (empty? stls))
            (is (= chars expected-chars))
            (is (= cursors expected-cursors)))))))

(defn total-selection-render-left [ctx]
  (let [processed (-> ctx
                      (at-input-end)
                      (at-line-start)
                      (process right)
                      (process select-left))
        {expected-chars   :chars
         expected-cursors :cursors} (discretise-h processed)]
    (-> (accumulative processed)
        (execute render-highlights!)
        (inspect
          (fn [{:keys [chars cursors bgs fgs stls]}]
            (is (not (empty? bgs)))
            (is (not (empty? fgs)))
            (is (empty? stls))
            (is (= chars expected-chars))
            (is (= cursors expected-cursors)))))))

(defn projected-selection-render [ctx]
  (let [processed (process ctx select-all)
        {expected-chars   :chars
         expected-cursors :cursors} (discretise-h processed)]
    (-> (accumulative processed)
        (execute render-highlights!)
        (inspect
          (fn [{:keys [chars cursors bgs fgs stls]}]
            (is (not (empty? bgs)))
            (is (not (empty? fgs)))
            (is (empty? stls))
            (is (= expected-chars chars))
            (is (= expected-cursors cursors)))))))

(defn projected-stylelised-render [ctx]
  (let [processed (-> ctx (process (char-key \()) (process left))
        {expected-chars :chars
         expected-cursors :cursors} (discretise-h processed)]
    (-> (accumulative processed)
        (execute render-highlights!)
        (inspect
          (fn [{:keys [chars cursors bgs fgs stls]}]
            (is (not (empty? bgs)))
            (is (not (empty? fgs)))
            (is (not (empty? stls)))
            (is (= expected-chars chars))
            (is (= expected-cursors cursors)))))))

(defn projected-prioritised-render [ctx]
  (let [processed (-> ctx
                      (process (char-key \())
                      (process (char-key \a))
                      (process left)
                      (process left)
                      (process expand))
        {expected-chars :chars
         expected-cursors :cursors} (discretise-h processed)]
    (-> (accumulative processed)
        (execute render-highlights!)
        (inspect
          (fn [{:keys [chars cursors bgs fgs stls]}]
            (is (not (empty? bgs)))
            (is (not (empty? fgs)))
            (is (empty? stls))
            (is (= expected-chars chars))
            (is (= expected-cursors cursors)))))))

(defn selection-render [ctx]
  (total-selection-render-right ctx)
  (total-selection-render-left ctx)
  (projected-selection-render ctx)
  (projected-stylelised-render ctx)
  (projected-prioritised-render ctx))

(defspec selection-render-test
         100
         (for-all [ctx (gen-context {:size   5
                                     :fov    27
                                     :seeker (one (gen-seeker-of 29))})]
                  (selection-render ctx)))

;; IV. Clean-up highlighting

(defn arbitrary-line-clean-up [ctx]
  (let [selected  (-> ctx
                      (at-main-view-start)
                      (process right)
                      (process select-right 2))
        {expected-chars :chars
         expected-cursors :cursors} (discretise-h selected)]
    (-> selected
        (process left)
        (accumulative)
        (execute clean-highlights!)
        (inspect
          (fn [{:keys [chars cursors bgs fgs]}]
            (is (= (flatten expected-chars) chars))
            (is (= expected-cursors cursors))
            (is (= :default (first (distinct bgs))))
            (is (not (empty? bgs)))
            (is (not (empty? fgs))))))))

(defn clean-up-render [ctx]
  (arbitrary-line-clean-up ctx))

(defspec clean-up-render-test
         100
         (for-all [ctx (gen-context {:size   5
                                     :fov    27
                                     :seeker (one (gen-seeker-of 29))})]
                  (clean-up-render ctx)))

;; V. Hud projection

(defn move-with [ctx {:keys [scroll ov]
                      :or   {scroll 0
                             ov     0}}]
  (let [fov  (-> ctx (r/preview-hud) (h/field-of-view))
        text (-> ctx (r/preview-hud) (h/text))]
    (i/rebase text #(->> % (take-last (+ fov scroll ov)) (take fov)))))

(defn total-hud-projection [ctx]
  (let [complete-view       (maximise-view ctx)
        expected-projection (-> ctx (r/preview-hud) (h/text))]
    (-> complete-view
        (process scroll-up 10)
        (project-preview)
        (<=>seeker expected-projection))))

(defn fov-hud-projection [ctx]
  (let [fov      (field-of-view ctx)
        actual   (-> ctx
                     (at-main-view-end)
                     (r/preview-hud)
                     (h/project-hud))
        expected (-> ctx
                     (r/preview-hud)
                     (h/text)
                     (i/rebase #(take-last fov %)))]
    (<=>seeker expected actual)))

(defn fov-with-overview-hud-projection [ctx]
  (let [offset   2
        fov      (field-of-view ctx)
        actual   (-> ctx
                     (at-main-view-start)
                     (process up offset)
                     (r/preview-hud)
                     (h/project-hud))
        expected (-> ctx
                     (r/preview-hud)
                     (h/text)
                     (i/rebase #(->> % (drop-last offset) (take-last fov))))]
    (<=>seeker expected actual)))

(defn scrolled-hud-projection [ctx]
  (can-be ctx
          #(-> % (process scroll-up 1) (project-preview) (<=>seeker (move-with % {:scroll 1})))
          #(-> % (process scroll-up 4) (project-preview) (<=>seeker (move-with % {:scroll 4})))
          #(-> % (process scroll-up 4) (process scroll-down) (project-preview) (<=>seeker (move-with % {:scroll 3})))))

(defn paged-hud-projection [ctx]
  (-> ctx
      (at-main-view-start)
      (process up 2)
      (can-be
        #(-> % (process scroll-up 1) (project-preview) (<=>seeker (move-with % {:scroll 1 :ov 2})))
        #(-> % (process scroll-up 4) (project-preview) (<=>seeker (move-with % {:scroll 4 :ov 2})))
        #(-> % (process scroll-up 4) (process scroll-down) (project-preview) (<=>seeker (move-with % {:scroll 3 :ov 2}))))))

(defn hud-projection [ctx]
  (total-hud-projection ctx)
  (fov-hud-projection ctx)
  (fov-with-overview-hud-projection ctx)
  (scrolled-hud-projection ctx)
  (paged-hud-projection ctx))

(defspec hud-projection-test
         100
         (for-all [ctx (gen-context {:size   20
                                     :fov    7
                                     :seeker (one (gen-seeker-of 10))})]
                  (hud-projection ctx)))

;; VI. Selection projection

(defn total-selection-projection [ctx]
  (let [total (-> ctx (at-input-end) (at-line-start) (maximise-view))
        [x y] (cursor total)]
    (can-be total
            #(-> (process % select-up 5)
                 (:highlights)
                 (:selection)
                 (highlights? {:start [x (- y 5)]
                               :end   [x y]})))))

(defn paged-selection-extension [ctx]
  (let [from-top-start    (-> ctx (at-main-view-start) (:complete-hud) (:seeker) (:cursor))
        top-clip-end      (-> ctx (at-main-view-end) (:complete-hud) (:seeker) (:cursor))
        bottom-clip-start (-> ctx (at-input-start) (:complete-hud) (:seeker) (:cursor))
        bottom-clip-end   (-> ctx (at-input-start) (at-view-bottom) (at-line-end) (:complete-hud) (:seeker) (:cursor))]
    (can-be ctx
            #(-> %
                 (at-main-view-start)
                 (process select-all)
                 (project-highlight :selection)
                 (= {:start from-top-start
                     :end   top-clip-end}))
            #(-> %
                 (at-input-start)
                 (process select-down 100)
                 (process select-right 100)
                 (project-highlight :selection)
                 (= {:start from-top-start
                     :end   top-clip-end}))
            #(-> %
                 (at-input-end)
                 (process select-up 100)
                 (process select-left 100)
                 (project-highlight :selection)
                 (= {:start bottom-clip-start
                     :end   bottom-clip-end})))))

(defn paged-selection-lower-clip [ctx]
  (let [fov (-> ctx :complete-hud :fov)]
    (-> ctx
        (at-main-view-start)
        (process up 2)
        (process select-right)
        (update-in [:highlights :selection :region]
                   #(let [{[xs ys] :start
                           [xe ye] :end} %]
                      {:start [xs (+ ys fov)]
                       :end   [xe (+ ye fov)]}))
        (can-be #(-> % (project-highlight :selection) (= (no-projection %)))))))

(defn paged-selection-upper-clip [ctx]
  (let [fov (-> ctx :complete-hud :fov)]
    (-> ctx
        (at-input-end)
        (at-line-start)
        (process select-right)
        (update-in [:highlights :selection :region]
                   #(let [{[xs ys] :start
                           [xe ye] :end} %]
                      {:start [xs (- ys fov)]
                       :end   [xe (- ye fov)]}))
        (can-be #(-> % (project-highlight :selection) (= (no-projection %)))))))

; selection is currently reset when scrolling
;(defn scrolled-selection-projection [ctx] true)

(defn selection-projection [ctx]
  (total-selection-projection ctx)
  (paged-selection-extension ctx)
  (paged-selection-lower-clip ctx)
  (paged-selection-upper-clip ctx))

(defspec selection-projection-test
         100
         (for-all [ctx (gen-context {:size   20
                                     :fov    7
                                     :seeker (one (gen-seeker-of 10))})]
                  (selection-projection ctx)))

;; VII. Cursor projection

(defn total-cursor-projection [ctx]
  (let [total (-> ctx (at-input-end) (at-line-start) (maximise-view))
        [x y] (cursor total)
        hp    (get-in total [:persisted-hud :seeker :height])
        hc    (get-in total [:complete-hud :seeker :height])]
    (can-be total
            #(-> % (process up) (project-cursor) (= [x (dec y)]))
            #(-> % (process up 4) (project-cursor) (= [x (- y 4)]))
            #(-> % (process up 100) (project-cursor) (= [x hp]))
            #(-> % (process down 100) (project-cursor) (= [x (dec hc)]))))) ;; starts with 0

(defn paged-cursor-projection [ctx]
  (let [end-y (dec (field-of-view ctx))]                              ;; starts from 0
    (-> ctx
        (at-main-view-start)
        (can-be
          #(-> % (process up) (project-cursor) (= [0 0]))
          #(-> % (process up 2) (project-cursor) (= [0 0]))
          #(-> % (process down 1) (project-cursor) (= [0 1]))
          #(-> % (process down 2) (project-cursor) (= [0 2]))
          #(-> % (at-view-bottom) (process down) (project-cursor) (= [0 end-y]))
          #(-> % (at-view-bottom) (process down 2) (project-cursor) (= [0 end-y]))))))

(defn cursor-projection [ctx]
  (total-cursor-projection ctx)
  (paged-cursor-projection ctx))

(defspec cursor-projection-test
         100
         (for-all [ctx (gen-context {:size   20
                                     :fov    7
                                     :seeker (one (gen-seeker-of 10))})]
                  (cursor-projection ctx)))

;; VIII. Y Projection

(defn- bounded? [ctx y]
  (<= 0 y (field-of-view ctx)))

(defn top-bounded-y-projection [ctx]
  (-> ctx
      (at-input-end)
      (at-line-start)
      (can-be #(bounded? % (-> % (process up 5) (project-y)))
              #(= 0 (-> % (process up 15) (project-y)))
              #(= 0 (-> % (process up 100) (project-y))))))

(defn bottom-bounded-y-projection [ctx]
  (let [end-y (dec (field-of-view ctx))]                              ;; starts from 0
    (-> ctx
        (at-main-view-start)
        (process up 5)
        (can-be #(bounded? % (-> % (process down 10) (project-y)))
                #(= end-y (-> % (process down 15) (project-y)))
                #(= end-y (-> % (process down 100) (project-y)))))))

(defn y-projection [ctx]
  (top-bounded-y-projection ctx)
  (bottom-bounded-y-projection ctx))

(defspec y-projection-test
         100
         (for-all [ctx (gen-context {:size   30
                                     :fov    10
                                     :seeker (one (gen-seeker-of 20))})]
                  (y-projection ctx)))


;; IX. Region diff

(defn- check-diff [{:keys [now then expected]}]
  (let [current {:region now}
        former {:region then}
        result  (if expected {:region expected} expected)]
    (is (= result (additive-diff current former)))))

(defn upper-x-diff []
  (let [a {:start [4 1] :end [4 3]}
        b {:start [0 1] :end [4 3]}
        r {:start [0 1] :end [4 1]}]
    (check-diff {:now a :then b :expected nil})
    (check-diff {:now b :then a :expected r})))

(defn lower-x-diff []
  (let [a {:start [2 1] :end [2 4]}
        b {:start [2 1] :end [5 4]}
        r {:start [2 4] :end [5 4]}]
    (check-diff {:now a :then b :expected nil})
    (check-diff {:now b :then a :expected r})))

(defn upper-y-diff []
  (let [a {:start [4 4] :end [7 6]}
        b {:start [2 1] :end [7 6]}
        r {:start [2 1] :end [4 4]}]
    (check-diff {:now a :then b :expected nil})
    (check-diff {:now b :then a :expected r})))

(defn lower-y-diff []
  (let [a {:start [2 1] :end [4 2]}
        b {:start [2 1] :end [6 5]}
        r {:start [4 2] :end [6 5]}]
    (check-diff {:now a :then b :expected nil})
    (check-diff {:now b :then a :expected r})))


(defn scissor-upper-y []
  (let [a  {:start [2 1] :end [4 1]}
        a' {:start [2 0] :end [4 1]}
        b  {:start [4 1] :end [2 2]}
        r  {:start [2 0] :end [2 1]}]
    (check-diff {:now a :then b :expected a})
    (check-diff {:now a' :then a :expected r})))

(defn scissor-lower-y []
  (let [a  {:start [2 2] :end [4 2]}
        a' {:start [2 2] :end [4 3]}
        b  {:start [4 1] :end [2 2]}
        r  {:start [4 2] :end [4 3]}]
    (check-diff {:now a :then b :expected a})
    (check-diff {:now a' :then a :expected r})))

(defn keep-diff []
  (let [a {:start [2 3] :end [4 5]}
        b {:start [2 3] :end [4 5]}]
    (check-diff {:now a :then b :expected nil})))

(defn no-diff []
  (let [a {:start [2 3] :end [4 5]}
        b {:start [4 5] :end [4 6]}]
    (check-diff {:now a :then b :expected a})
    (check-diff {:now b :then a :expected b})))

(clojure.test/deftest region-diff
  (upper-x-diff)
  (lower-x-diff)
  (upper-y-diff)
  (lower-y-diff)
  (scissor-upper-y)
  (scissor-lower-y)
  (keep-diff)
  (no-diff))