(ns omnia.rendering-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :refer [for-all]]
            [clojure.test.check.generators :as gen]
            [omnia.test-utils :refer :all]
            [omnia.more :refer [map-vals reduce-idx]]
            [omnia.rendering :as r]
            [omnia.terminal :as t]
            [omnia.input :as i]))

(defn stateful [ctx]
  (let [chars   (atom [])
        cursors (atom [])
        bgs     (atom [])
        fgs     (atom [])
        clears  (atom 0)
        acc     (fn [atm val] (swap! atm #(conj % val)))
        cnt     (fn [atm] (swap! atm inc))
        size    (t/size (:terminal ctx))]
    (assoc ctx
      :terminal (test-terminal {:background! (fn [colour] (acc bgs colour))
                                :foreground! (fn [colour] (acc fgs colour))
                                :clear!      (fn [] (cnt clears))
                                :put!        (fn [ch x y]
                                               (acc chars ch)
                                               (acc cursors [x y]))
                                :size        size})
      :state {:chars   chars
              :cursors cursors
              :bgs     bgs
              :fgs     fgs
              :clears  clears})))

(defn inspect [ctx p]
  (when-let [state (:state ctx)]
    (p (map-vals deref state))))

(defn execute [ctx f]
  (f ctx)
  ctx)

(defn hud-traversal [ctx]
  (-> (project-hud ctx)
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
  (-> (hud-traversal ctx)
      (aggregate)))

(defn discretise-h [{:keys [complete-hud highlights] :as ctx}]
  (let [fov (:fov complete-hud)
        {start :start
         end   :end} (-> (first highlights)
                         (update :start (fn [[x y]] [x (r/project-y complete-hud y)]))
                         (update :end (fn [[x y]] [x (r/project-y complete-hud y)]))
                         (r/project-selection fov))]
    (-> (hud-traversal ctx)
        (assoc :cursor start
               :selection end)
        (i/extract)
        (aggregate))))

;; I. Total rendering

(defn projected-total-render [ctx]
  (let [processed (-> (move-top-fov ctx)
                      (process up 2))
        {expected-chars   :chars
         expected-cursors :cursors} (discretise processed)]
    (-> (stateful processed)
        (execute r/total!)
        (inspect
          (fn [{:keys [chars cursors clears fgs bgs]}]
            (is (not (empty? bgs)))
            (is (not (empty? fgs)))
            (is (= 1 clears))
            (is (= expected-chars chars))
            (is (= expected-cursors cursors)))))))

(defn total-render [ctx]
  (projected-total-render ctx))

(defspec total-render-test
         100
         (for-all [ctx (gen-context {:size   5
                                     :fov    27
                                     :seeker (one (gen-seeker-of 29))})]
                  (total-render ctx)))

;; II. Diffed rendering

(defn padded-diff-render [ctx]
  (let [selected         (-> (move-end-fov ctx)
                             (process select-right 100))
        processed        (process selected backspace)
        n-last           (-> (:complete-hud ctx) (:lines) (last) (count))
        expected-chars   (repeat n-last \space)
        expected-cursors (->> (discretise selected)
                              (:cursors)
                              (take-last n-last))]
    (-> (stateful processed)
        (execute r/diff!)
        (inspect
          (fn [{:keys [chars cursors fgs bgs]}]
            (is (not (empty? bgs)))
            (is (not (empty? fgs)))
            (is (= expected-cursors cursors))
            (is (= expected-chars chars)))))))

(defn projected-diff-render [ctx k]
  (let [processed        (-> (move-end-fov ctx)
                             (process (char-key k) 10))
        last-n           (-> (:complete-hud processed) (:lines) (last) (count))
        {chars   :chars
         cursors :cursors} (discretise processed)
        expected-chars   (take-last last-n chars)
        expected-cursors (take-last last-n cursors)]
    (-> (stateful processed)
        (execute r/diff!)
        (inspect
          (fn [{:keys [chars cursors fgs bgs]}]
            (is (not (empty? bgs)))
            (is (not (empty? fgs)))
            (is (= expected-cursors cursors))
            (is (= expected-chars chars)))))))

(defn no-change-diff-render [ctx]
  (-> (move-end-fov ctx)
      (process up)
      (stateful)
      (execute r/diff!)
      (inspect
        (fn [{:keys [chars cursors fgs bgs]}]
          (is (empty? chars))
          (is (empty? cursors))
          (is (empty? fgs))
          (is (empty? bgs))))))

(defn diff-reset-to-total-render [ctx]
  (let [processed (-> (move-top-fov ctx)
                      (process up))
        {expected-chars   :chars
         expected-cursors :cursors} (discretise processed)]
    (-> (stateful processed)
        (execute r/diff!)
        (inspect
          (fn [{:keys [chars cursors clears fgs bgs]}]
            (is (not (empty? bgs)))
            (is (not (empty? fgs)))
            (is (= 1 clears))
            (is (= expected-chars chars))
            (is (= expected-cursors cursors)))))))

(defn diff-render [ctx]
  (projected-diff-render ctx (one gen/char-alpha))
  (padded-diff-render ctx)
  (no-change-diff-render ctx)
  (diff-reset-to-total-render ctx))

(defspec diff-render-test
         100
         (for-all [ctx (gen-context {:size   5
                                     :fov    27
                                     :seeker (one (gen-seeker-of 29))})]
                  (diff-render ctx)))

;; III. No rendering

(defn projected-no-render [ctx]
  (-> (stateful ctx)
      (execute r/nothing!)
      (inspect
        (fn [{:keys [chars cursors fgs bgs]}]
          (is (empty? bgs))
          (is (empty? fgs))
          (is (empty? chars))
          (is (empty? cursors))))))

(defn no-reset-to-total-render [ctx]
  (let [processed (-> (move-top-fov ctx)
                      (process up))
        {expected-chars   :chars
         expected-cursors :cursors} (discretise processed)]
    (-> (stateful processed)
        (execute r/nothing!)
        (inspect
          (fn [{:keys [chars cursors clears fgs bgs]}]
            (is (not (empty? bgs)))
            (is (not (empty? fgs)))
            (is (= 1 clears))
            (is (= expected-chars chars))
            (is (= expected-cursors cursors)))))))

(defn no-render [ctx]
  (projected-no-render ctx)
  (no-reset-to-total-render ctx))

(defspec no-render-test
         100
         (for-all [ctx (gen-context {:size   5
                                     :fov    27
                                     :seeker (one (gen-seeker-of 29))})]
                  (no-render ctx)))

;; IV. Selection highlighting

(defn total-selection-render-right [ctx]
  (let [processed (-> (move-end-fov ctx)
                      (process select-right 3))
        {expected-chars   :chars
         expected-cursors :cursors} (discretise-h processed)]
    (-> (stateful processed)
        (execute #(r/highlight! % (:highlights %)))
        (inspect
          (fn [{:keys [chars cursors bgs fgs]}]
            (is (not (empty? bgs)))
            (is (not (empty? fgs)))
            (is (= chars expected-chars))
            (is (= cursors expected-cursors)))))))

(defn total-selection-render-left [ctx]
  (let [processed (-> (move-end-fov ctx)
                      (process right 3)
                      (process select-left 3))
        {expected-chars   :chars
         expected-cursors :cursors} (discretise-h processed)]
    (-> (stateful processed)
        (execute #(r/highlight! % (:highlights %)))
        (inspect
          (fn [{:keys [chars cursors bgs fgs]}]
            (is (not (empty? bgs)))
            (is (not (empty? fgs)))
            (is (= chars expected-chars))
            (is (= cursors expected-cursors)))))))

(defn projected-selection-render [ctx]
  (let [processed (process ctx select-all)
        {expected-chars   :chars
         expected-cursors :cursors} (discretise-h processed)]
    (-> (stateful processed)
        (execute #(r/highlight! % (:highlights %)))
        (inspect
          (fn [{:keys [chars cursors bgs fgs]}]
            (is (not (empty? bgs)))
            (is (not (empty? fgs)))
            (is (= expected-chars chars))
            (is (= expected-cursors cursors)))))))

(defn selection-render [ctx]
  (total-selection-render-right ctx)
  (total-selection-render-left ctx)
  (projected-selection-render ctx))

(defspec selection-render-test
         100
         (for-all [ctx (gen-context {:size   5
                                     :fov    27
                                     :seeker (one (gen-seeker-of 29))})]
                  (selection-render ctx)))

;; V. Clean-up highlighting

(defn line-start-clean-up-render [ctx]
  (let [processed        (-> (move-end-fov ctx)
                             (process right)
                             (process select-right 100)
                             (process up))
        last-n           (-> (:complete-hud ctx) (:lines) (last) (count))
        {chars   :chars
         cursors :cursors} (discretise processed)
        expected-chars   (take-last last-n chars)
        expected-cursors (take-last last-n cursors)]
    (-> (stateful processed)
        (execute r/clean!)
        (inspect
          (fn [{:keys [chars cursors bgs fgs]}]
            (is (= :default (first (distinct bgs))))
            (is (not (empty? bgs)))
            (is (not (empty? fgs)))
            (is (= expected-chars chars))
            (is (= expected-cursors cursors)))))))

(defn clean-up-render [ctx]
  (line-start-clean-up-render ctx))

(defspec clean-up-render-test
         100
         (for-all [ctx (gen-context {:size   5
                                     :fov    27
                                     :seeker (one (gen-seeker-of 29))})]
                  (clean-up-render ctx)))

;; VI. Hud projection

(defn move-with [ctx {:keys [scroll ov]
                      :or   {scroll 0
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
         (for-all [ctx (gen-context {:size   20
                                     :fov    7
                                     :seeker (one (gen-seeker-of 10))})]
                  (hud-projection ctx)))

;; VII. Selection projection

(defn total-selection-projection [ctx]
  (let [total (move-end-fov (make-total ctx))
        [x y] (cursor total)]
    (can-be total
            #(-> (process % select-up 5)
                 (:highlights)
                 (contains? {:start [x (- y 5)]
                             :end   [x y]})))))

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
         (for-all [ctx (gen-context {:size   20
                                     :fov    7
                                     :seeker (one (gen-seeker-of 10))})]
                  (selection-projection ctx)))

;; VIII. Cursor projection

(defn total-cursor-projection [ctx]
  (let [total (move-end-fov (make-total ctx))
        [x y] (cursor total)
        hp    (get-in total [:persisted-hud :height])
        hc    (get-in total [:complete-hud :height])]
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
         (for-all [ctx (gen-context {:size   20
                                     :fov    7
                                     :seeker (one (gen-seeker-of 10))})]
                  (cursor-projection ctx)))

;; IX. Y Projection

(defn- bounded? [ctx y]
  (<= 0 y (fov ctx)))

(defn top-bounded-y-projection [ctx]
  (-> (move-end-fov ctx)
      (can-be #(bounded? % (-> % (process up 5) (project-y)))
              #(= 0 (-> % (process up 15) (project-y)))
              #(= 0 (-> % (process up 100) (project-y))))))

(defn bottom-bounded-y-projection [ctx]
  (let [end-y (dec (fov ctx))]                              ;; starts from 0
    (-> (move-top-fov ctx)
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