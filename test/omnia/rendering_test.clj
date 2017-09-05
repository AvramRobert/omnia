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
  (let [chars (atom [])
        cursors (atom [])
        bgs (atom [])
        fgs (atom [])
        moves (atom [])
        clears (atom 0)
        acc (fn [atm val] (swap! atm #(conj % val)))
        cnt (fn [atm] (swap! atm inc))
        size (t/size (:terminal ctx))]
    (assoc ctx
      :terminal (test-terminal {:background! (fn [colour] (acc bgs colour))
                                :foreground! (fn [colour] (acc fgs colour))
                                :clear! (fn [] (cnt clears))
                                :move! (fn [x y] (acc moves [x y]))
                                :put! (fn [ch x y]
                                        (acc chars ch)
                                        (acc cursors [x y]))
                                :size size})
      :state {:chars chars
              :cursors cursors
              :bgs bgs
              :fgs fgs
              :clears clears
              :moves moves})))

(defn inspect [ctx p]
  (when-let [state (:state ctx)]
    (p (map-vals deref state))))

(defn execute [ctx f]
  (f ctx)
  ctx)

(defn hud-cursors [{:keys [lines]}]
  (->> lines
       (reduce-idx
         (fn [y cursors line]
           (reduce-idx
             (fn [x icursors _]
               (conj icursors [x y])) cursors line)) [])
       (sort-by (juxt second first))))

(defn hud-chars [{:keys [lines]}]
  (reduce concat lines))

(defn extract-highlighted-chars [{:keys [complete-hud highlights]}]
  (let [{start :start
         end   :end} (first highlights)]
    (-> complete-hud
        (assoc :cursor start
               :selection end)
        (i/extract)
        (hud-chars))))

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

(defn expected-print [ctx]
  (-> (hud-traversal ctx)
      (aggregate)))

(defn expected-highlight [{:keys [complete-hud highlights] :as ctx}]
  (let [{start :start
         end   :end} (-> (first highlights)
                         (update :start (fn [[x y]] [x (r/project-y complete-hud y)]))
                         (update :end (fn [[x y]] [x (r/project-y complete-hud y)])))]
    (-> (hud-traversal ctx)
        (assoc :cursor start
               :selection end)
        (i/extract)
        (aggregate))))

;; I. Total rendering

(defn projected-total-render [ctx]
  (let [processed (-> (move-top-fov ctx)
                      (process up 2))
        projected (project-hud processed)
        expected-chars (hud-chars projected)
        expected-cursors (hud-cursors projected)]
    (-> (stateful processed)
        (execute r/total!)
        (inspect
          (fn [{:keys [chars cursors clears]}]
            (is (= 1 clears))
            (is (= expected-chars chars))
            (is (= expected-cursors cursors)))))))

(defn total-render [ctx]
  (projected-total-render ctx))

(defspec total-render-test
         100
         (for-all [tctx (gen-context {:size   5
                                      :fov    27
                                      :seeker (one (gen-seeker-of 29))})]
                  (total-render tctx)))

;; II. Diffed rendering

(defn padded-diff-render [ctx]
  (let [processed (-> (move-end-fov ctx)
                      (process select-right 2))
        projected (project-hud processed)
        deleted (process processed backspace)
        expected-chars (-> projected
                           (i/rebase #(take-last 1 %))
                           (hud-chars)
                           (->> (drop 2))
                           (concat [\space \space]))
        expected-cursors (->> (hud-cursors projected)
                              (take-last (count expected-chars)))]
    (-> (stateful deleted)
        (execute r/diff!)
        (inspect
          (fn [{:keys [chars cursors]}]
            (is (= expected-cursors cursors))
            (is (= expected-chars chars)))))))

(defn projected-diff-render [ctx k]
  (let [processed (-> (move-end-fov ctx)
                      (process (char-key k) 10))
        expected-chars (-> (:complete-hud processed)
                           (:lines)
                           (last))
        expected-cursors (->> (:complete-hud processed)
                              (r/project-hud)
                              (hud-cursors)
                              (take-last (count expected-chars)))]
    (-> (stateful processed)
        (execute r/diff!)
        (inspect
          (fn [{:keys [chars cursors]}]
            (is (= expected-cursors cursors))
            (is (= expected-chars chars)))))))

(defn diff-reset-to-total-render [ctx]
  (let [processed (-> (move-top-fov ctx)
                      (process up))
        projected (project-hud processed)
        expected-chars (hud-chars projected)
        expected-cursors (hud-cursors projected)]
    (-> (stateful processed)
        (execute r/diff!)
        (inspect
          (fn [{:keys [chars cursors clears]}]
            (is (= 1 clears))
            (is (= expected-chars chars))
            (is (= expected-cursors cursors)))))))

(defn diff-render [ctx]
  (projected-diff-render ctx (one gen/char-alpha))
  (padded-diff-render ctx)
  (diff-reset-to-total-render ctx))

(defspec diff-render-test
         100
         (for-all [tctx (gen-context {:size   5
                                      :fov    27
                                      :seeker (one (gen-seeker-of 29))})]
                  (diff-render tctx)))

;; III. No rendering

(defn projected-no-render [ctx]
  (-> (stateful ctx)
      (execute r/nothing!)
      (inspect
        (fn [{:keys [chars cursors]}]
          (is (empty? chars))
          (is (empty? cursors))))))

(defn no-reset-to-total-render [ctx]
  (let [processed (-> (move-top-fov ctx)
                      (process up))
        {expected-chars   :chars
         expected-cursors :cursors} (expected-print processed)]
    (-> (stateful processed)
        (execute r/nothing!)
        (inspect
          (fn [{:keys [chars cursors clears]}]
            (is (= 1 clears))
            (is (= expected-chars chars))
            (is (= expected-cursors cursors)))))))

(defn no-render [ctx]
  (projected-no-render ctx)
  (no-reset-to-total-render ctx))

(defspec no-render-test
         100
         (for-all [tctx (gen-context {:size 5
                                      :fov 27
                                      :seeker (one (gen-seeker-of 29))})]
                  (no-render tctx)))

;; IV. Selection highlighting

(defn total-selection-render-right [ctx]
  (let [processed (-> (move-end-fov ctx)
                      (process select-right 3))
        {expected-chars   :chars
         expected-cursors :cursors} (expected-highlight processed)]
    (-> (stateful processed)
        (execute #(r/highlight! % (:highlights %)))
        (inspect
          (fn [{:keys [chars cursors]}]
            (is (= chars expected-chars))
            (is (= cursors expected-cursors)))))))

(defn total-selection-render-left [ctx]
  (let [processed (-> (move-end-fov ctx)
                      (process right 3)
                      (process select-left 3))
        {expected-chars   :chars
         expected-cursors :cursors} (expected-highlight processed)]
    (-> (stateful processed)
        (execute #(r/highlight! % (:highlights %)))
        (inspect
          (fn [{:keys [chars cursors]}]
            (is (= chars expected-chars))
            (is (= cursors expected-cursors)))))))

(defn projected-selection-render [ctx] true)

(defn selection-render [ctx]
  (total-selection-render-right ctx)
  (total-selection-render-left ctx)
  (projected-selection-render ctx))

(defspec selection-render-test
         100
         (for-all [tctx (gen-context {:size 5
                                      :fov 27
                                      :seeker (one (gen-seeker-of 29))})]
                  (selection-render tctx)))

;; V. Clean-up highlighting
(defn clean-up-render [ctx] true)

;; VI. Hud projection

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
                                      :seeker (one (gen-seeker-of 10))})]
                  (hud-projection tctx)))

;; VII. Selection projection

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
                                      :seeker (one (gen-seeker-of 10))})]
                  (selection-projection tctx)))


;; VIII. Cursor projection

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

;; FIXME: Implement test
(defn y-projection [ctx] true)

(defn cursor-projection [ctx]
  (total-cursor-projection ctx)
  (paged-cursor-projection ctx)
  (y-projection ctx))

(defspec cursor-projection-test
         100
         (for-all [tctx (gen-context {:size 20
                                      :fov 7
                                      :seeker (one (gen-seeker-of 10))})]
                  (cursor-projection tctx)))



;; IX. Line printing ???

(defn line-print [] true)