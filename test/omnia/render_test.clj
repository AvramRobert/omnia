(ns omnia.render-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :refer [for-all]]
            [omnia.test-utils :refer :all]
            [omnia.view.render :refer :all]
            [omnia.util.collection :refer [map-vals reduce-idx]]
            [omnia.util.schema :refer [Point Region]]
            [omnia.util.generator :refer [one]]
            [clojure.test.check.generators :as gen]
            [schema.core :as s]
            [omnia.repl.context :as r]
            [omnia.repl.hud :as h]
            [omnia.view.terminal :as t]
            [omnia.text.core :as i]
            [omnia.repl.context :as c]
            [omnia.config.components.text :as ct]
            [omnia.config.defaults :as d])
  (:import (clojure.lang Atom)))

(def ^:const NR-OF-TESTS 100)

(def IndexedCharacter
  {:char   Character
   :cursor Point})

(def IndexedSeeker
  (assoc i/Seeker :lines [[IndexedCharacter]]))

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
  [ctx  :- r/Context]
  (let [chars    (atom [])
        cursors  (atom [])
        bgs      (atom [])
        fgs      (atom [])
        stls     (atom [])
        acc      (fn [atm val] (swap! atm #(conj % val)))
        terminal (test-terminal {:put! (fn [_ ch x y fg bg stl]
                                         (acc bgs bg)
                                         (acc fgs fg)
                                         (run! #(acc stls %) stl)
                                         (acc chars ch)
                                         (acc cursors [x y]))
                                 :size (fn [] (r/view-size ctx))})]
    {:ctx      ctx
     :terminal terminal
     :state    {:chars   chars
                :cursors cursors
                :bgs     bgs
                :fgs     fgs
                :stls    stls}}))

(s/defn inspect
  [acc :- Accumulate p]
  (->> acc (:state) (map-vals deref) (p)))

(s/defn execute :- Accumulate
  [acc :- Accumulate, f]
  (f (:terminal acc) (:ctx acc))
  acc)

(s/defn index-seeker :- IndexedSeeker
  [seeker :- i/Seeker]
  (i/rebase seeker #(map-indexed
                      (fn [y line]
                        (map-indexed
                          (fn [x c]
                            {:cursor [x y] :char c}) line)) %)))

(s/defn index :- [IndexedCharacter]
  ([hud :- h/Hud]
   (->> hud
        (h/project-hud)
        (index-seeker)
        (:lines)
        (flatten))))

(s/defn index-at :- [IndexedCharacter]
  ([hud :- h/Hud, region :- Region]
   (let [indexed (->> hud (h/project-hud) (index-seeker))]
     (->> (h/project-selection hud region)
          (i/extract-for indexed)
          (:lines)
          (flatten)))))

;; I. Diffed rendering

(defn padded-diff-render [ctx]
  (let [selected         (-> ctx
                             (at-input-end)
                             (at-line-start)
                             (process (repeat 100 select-right)))
        processed        (process selected [backspace])
        n-last           (-> ctx (r/preview-hud) (h/text) (:lines) (last) (count))
        expected-chars   (repeat n-last \space)
        expected-cursors (->> selected
                              (r/preview-hud)
                              (index)
                              (take-last n-last)
                              (map :cursor))]
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
                             (process (repeat 10 (character k))))
        last-n           (->> processed (r/preview-hud) (h/text) (:lines) (last) (count))
        expectation      (->> processed (r/preview-hud) (index) (take-last last-n))]
    (-> (accumulative processed)
        (execute render-diff!)
        (inspect
          (fn [{:keys [chars cursors fgs bgs stls]}]
            (is (not (empty? bgs)))
            (is (not (empty? fgs)))
            (is (empty? stls))
            (is (= (map :cursor expectation) cursors))
            (is (= (map :char expectation) chars)))))))

(defn no-change-diff-render [ctx]
  (-> ctx
      (at-input-end)
      (at-line-start)
      (process [up])
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
  (let [processed        (-> ctx
                             (at-main-view-start)
                             (process [down select-up select-up select-up backspace]))
        expectation      (-> processed (r/preview-hud) (index))
        expected-chars   (map :char expectation)
        expected-cursors (map :cursor expectation)]
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
         NR-OF-TESTS
         (for-all [ctx (gen-context {:prefilled-size 5
                                     :view-size      27
                                     :text-area      (gen-text-area-of 29)})]
                  (diff-render ctx)))

;; II. No rendering

(defn projected-no-render [ctx]
  (-> (accumulative ctx)
      (execute render-nothing!)
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
                      (process [up]))]
    (-> (accumulative processed)
        (execute render-nothing!)
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
         NR-OF-TESTS
         (for-all [ctx (gen-context {:prefilled-size 5
                                     :view-size      27
                                     :text-area      (gen-text-area-of 29)})]
                  (no-render ctx)))

;; III. Selection highlighting

(defn total-selection-render-right [ctx]
  (let [adapted          (-> ctx
                             (at-input-end)
                             (at-line-start))
        text             (-> adapted (r/preview-hud) (h/project-hud))
        expected-chars   [(i/current-char text)]
        expected-cursors [(:cursor text)]]
    (-> adapted
        (process [select-right])
        (accumulative)
        (execute render-highlights!)
        (inspect
          (fn [{:keys [chars cursors bgs fgs stls]}]
            (is (not (empty? bgs)))
            (is (not (empty? fgs)))
            (is (empty? stls))
            (is (= chars expected-chars))
            (is (= cursors expected-cursors)))))))

(defn total-selection-render-left [ctx]
  (let [adapted          (-> ctx
                             (at-input-end)
                             (at-line-start))
        text             (-> adapted (c/preview-hud) (h/project-hud))
        cursor           (:cursor text)
        expected-chars   [(i/current-char text)]
        expected-cursors [cursor]]
    (-> adapted
        (process [right select-left])
        (accumulative)
        (execute render-highlights!)
        (inspect
          (fn [{:keys [chars cursors bgs fgs stls]}]
            (is (not (empty? bgs)))
            (is (not (empty? fgs)))
            (is (empty? stls))
            (is (= chars expected-chars))
            (is (= cursors expected-cursors)))))))

(defn projected-selection-render [ctx]
  (let [processed        (process ctx [select-all])
        projected        (project-preview processed)
        expected-chars   (->> projected (:lines) (flatten))
        expected-cursors (->> projected (index-seeker) (:lines) (flatten) (map :cursor))]
    (-> processed
        (accumulative)
        (execute render-highlights!)
        (inspect
          (fn [{:keys [chars cursors bgs fgs stls]}]
            (is (not (empty? bgs)))
            (is (not (empty? fgs)))
            (is (empty? stls))
            (is (= expected-chars chars))
            (is (= expected-cursors cursors)))))))

(defn projected-stylised-render [ctx]
  (let [processed (process ctx [(character \()
                                (character \a)
                                left
                                left])
        [x y]     (project-preview-cursor processed)
        expected-chars   [\( \)]
        expected-cursors [[x y] [(+ x 2) y]]
        expected-styles  [:underline :underline]]
    (-> processed
        (accumulative)
        (execute render-highlights!)
        (inspect
          (fn [{:keys [chars cursors bgs fgs stls]}]
            (is (not (empty? bgs)))
            (is (not (empty? fgs)))
            (is (= expected-styles stls))
            (is (= expected-chars chars))
            (is (= expected-cursors cursors)))))))

(defn additive-exclusion-render [ctx]
  (let [adapted (process ctx [(character \()
                              (character \a)
                              left
                              left])
        [x y]     (project-preview-cursor adapted)
        expected-chars   [\( \a \)]
        expected-cursors [[x y] [(+ x 1) y] [(+ x 2) y]]]
    (-> adapted
        (process [expand])
        (accumulative)
        (execute render-highlights!)
        (inspect
          (fn [{:keys [chars cursors bgs fgs stls]}]
            (is (not (empty? bgs)))
            (is (not (empty? fgs)))
            (is (empty? stls))
            (is (= expected-chars chars))
            (is (= expected-cursors cursors)))))))

(defn expansive-render [ctx]
  (let [characters [\d \e \f]
        processed (-> ctx
                      (at-input-end)
                      (at-line-start)
                      (process (->> characters (cons \() (map character))))]
    (-> processed
        (process [left left expand])
        (accumulative)
        (execute render-highlights!)
        (inspect
          (fn [{:keys [chars bgs fgs stls]}]
            (is (not (empty? bgs)))
            (is (not (empty? fgs)))
            (is (empty? stls))
            (is (= chars characters)))))))

(defn selection-render [ctx]
  (expansive-render ctx)
  (total-selection-render-right ctx)
  (total-selection-render-left ctx)
  (projected-selection-render ctx)
  (projected-stylised-render ctx)
  (additive-exclusion-render ctx))

(defspec selection-render-test
         NR-OF-TESTS
         (for-all [ctx (gen-context {:prefilled-size 5
                                     :view-size      27
                                     :text-area      (gen-text-area-of 29)})]
                  (selection-render ctx)))

;; IV. Clean-up highlighting

(defn arbitrary-line-clean-up [ctx]
  (let [adapted     (-> ctx
                        (at-main-view-start)
                        (process [right
                                  select-right
                                  select-right
                                  left]))
        expectation (->> adapted
                         (r/garbage)
                         (vals)
                         (map :region)
                         (mapcat #(index-at (r/preview-hud adapted) %)))
        expected-bg (get d/default-colours ct/default)]
    (-> adapted
        (accumulative)
        (execute clean-highlights!)
        (inspect
          (fn [{:keys [chars cursors bgs fgs]}]
            (is (= (map :char expectation) chars))
            (is (= (map :cursor expectation) cursors))
            (is (= expected-bg (first (distinct bgs))))
            (is (not (empty? bgs)))
            (is (not (empty? fgs))))))))

(defn clean-up-render [ctx]
  (arbitrary-line-clean-up ctx))

;; Even though previously I only put the highlighted characters, I still started at the beginning of the line an iterated over it entirely
(defspec clean-up-render-test
         NR-OF-TESTS
         (for-all [ctx (gen-context {:prefilled-size 5
                                     :view-size      27
                                     :text-area      (gen-text-area-of 29)})]
                  (clean-up-render ctx)))

;; V. Hud projection

(defn move-with [ctx {:keys [scroll-offset
                             view-offset]
                      :or   {scroll-offset 0
                             view-offset   0}}]
  (let [fov  (r/view-size ctx)
        text (-> ctx (r/preview-hud) (h/text))]
    (i/rebase text #(->> % (take-last (+ fov scroll-offset view-offset)) (take fov)))))

(defn total-hud-projection [ctx]
  (let [complete-view       (maximise-view ctx)
        expected-projection (-> ctx (r/preview-hud) (h/text))
        projected           (-> complete-view
                                (process (repeat 10 scroll-up))
                                (project-preview))]
    (is (i/equivalent? projected expected-projection))))

(defn fov-hud-projection [ctx]
  (let [fov      (r/view-size ctx)
        actual   (-> ctx
                     (at-main-view-end)
                     (project-preview))
        expected (-> ctx
                     (r/preview-hud)
                     (h/text)
                     (i/rebase #(take-last fov %)))]
    (is (i/equivalent? actual expected))))

(defn fov-with-overview-hud-projection [ctx]
  (let [offset   2
        fov      (r/view-size ctx)
        actual   (-> ctx
                     (at-main-view-start)
                     (process (repeat offset up))
                     (project-preview))
        expected (-> ctx
                     (r/preview-hud)
                     (h/text)
                     (i/rebase #(->> % (drop-last offset) (take-last fov))))]
    (is (i/equivalent? actual expected))))

(defn scrolled-hud-projection [ctx]
  (should-be ctx
             #(-> %
               (process [scroll-up])
               (project-preview)
               (i/equivalent? (move-with % {:scroll-offset 1})))
             #(-> %
               (process [scroll-up
                         scroll-up
                         scroll-up
                         scroll-up])
               (project-preview)
               (i/equivalent? (move-with % {:scroll-offset 4})))
             #(-> %
               (process [scroll-up
                         scroll-up
                         scroll-up
                         scroll-up
                         scroll-down])
               (project-preview)
               (i/equivalent? (move-with % {:scroll-offset 3})))))

(defn paged-hud-projection [ctx]
  (-> ctx
      (at-main-view-start)
      (process [up up])
      (should-be
        #(-> %
             (process [scroll-up])
             (project-preview)
             (i/equivalent? (move-with % {:scroll-offset 1
                                          :view-offset 2})))
        #(-> %
             (process [scroll-up
                       scroll-up
                       scroll-up
                       scroll-up])
             (project-preview)
             (i/equivalent? (move-with % {:scroll-offset 4
                                          :view-offset 2})))
        #(-> %
             (process [scroll-up
                       scroll-up
                       scroll-up
                       scroll-up
                       scroll-down])
             (project-preview)
             (i/equivalent? (move-with % {:scroll-offset 3
                                          :view-offset 2}))))))

(defn hud-projection [ctx]
  (total-hud-projection ctx)
  (fov-hud-projection ctx)
  (fov-with-overview-hud-projection ctx)
  (scrolled-hud-projection ctx)
  (paged-hud-projection ctx))

(defspec hud-projection-test
         NR-OF-TESTS
         (for-all [ctx (gen-context {:prefilled-size 20
                                     :view-size      7
                                     :text-area      (gen-text-area-of 10)})]
                  (hud-projection ctx)))

;; VI. Selection projection

(defn total-selection-projection [ctx]
  (let [total (-> ctx (at-input-end) (at-line-start) (maximise-view))
        [x y] (cursor total)]
    (should-be total
               #(-> %
                 (process [select-up
                           select-up
                           select-up
                           select-up
                           select-up])
                 (r/highlights)
                 (:selection)
                 (highlights? {:start [x (- y 5)]
                               :end   [x y]})))))

(defn paged-selection-extension [ctx]
  (let [from-top-start    (-> ctx (at-main-view-start) (r/preview-hud) (h/text) (:cursor))
        top-clip-end      (-> ctx (at-main-view-end) (r/preview-hud) (h/text) (:cursor))
        bottom-clip-start (-> ctx (at-input-start) (r/preview-hud) (h/text) (:cursor))
        bottom-clip-end   (-> ctx (at-input-start) (at-view-bottom) (at-line-end) (r/preview-hud) (h/text) (:cursor))]
    (should-be ctx
               #(-> %
                 (at-main-view-start)
                 (process [select-all])
                 (project-highlight :selection)
                 (= {:start from-top-start
                     :end   top-clip-end}))
               #(-> %
                 (at-input-start)
                 (process (repeat 100 select-down))
                 (process (repeat 100 select-right))
                 (project-highlight :selection)
                 (= {:start from-top-start
                     :end   top-clip-end}))
               #(-> %
                 (at-input-end)
                 (process (repeat 100 select-up))
                 (process (repeat 100 select-left))
                 (project-highlight :selection)
                 (= {:start bottom-clip-start
                     :end   bottom-clip-end})))))

(defn paged-selection-lower-clip [ctx]
  (let [fov       (r/view-size ctx)
        processed (-> ctx
                      (at-main-view-start)
                      (process [up up select-right])
                      (extend-highlight :selection [0 fov]))]
    (is (= (project-highlight processed :selection) (no-projection processed)))))

(defn paged-selection-upper-clip [ctx]
  (let [fov       (r/view-size ctx)
        processed (-> ctx
                      (at-input-end)
                      (at-line-start)
                      (process [select-right])
                      (extend-highlight :selection [0 (- fov)]))]
    (is (= (project-highlight processed :selection) (no-projection processed)))))

(defn selection-projection [ctx]
  (total-selection-projection ctx)
  (paged-selection-extension ctx)
  (paged-selection-lower-clip ctx)
  (paged-selection-upper-clip ctx))

(defspec selection-projection-test
         NR-OF-TESTS
         (for-all [ctx (gen-context {:prefilled-size 20
                                     :view-size      7
                                     :text-area      (gen-text-area-of 10)})]
                  (selection-projection ctx)))

;; VII. Cursor projection

(defn total-cursor-projection [ctx]
  (let [total (-> ctx (at-input-end) (at-line-start) (maximise-view))
        [x y] (cursor total)
        hp    (-> total (r/persisted-hud) (h/text) (:height))
        hc    (-> total (r/preview-hud) (h/text) (:height))]
    (should-be total
               #(-> % (process [up]) (project-preview-cursor) (= [x (dec y)]))
               #(-> % (process [up up up up]) (project-preview-cursor) (= [x (- y 4)]))
               #(-> % (process (repeat 100 up)) (project-preview-cursor) (= [x hp]))
               #(-> % (process (repeat 100 down)) (project-preview-cursor) (= [x (dec hc)]))))) ;; starts with 0

(defn paged-cursor-projection [ctx]
  (let [end-y (dec (r/view-size ctx))]                              ;; starts from 0
    (-> ctx
        (at-main-view-start)
        (should-be
          #(-> % (process [up]) (project-preview-cursor) (= [0 0]))
          #(-> % (process [up up]) (project-preview-cursor) (= [0 0]))
          #(-> % (process [down]) (project-preview-cursor) (= [0 1]))
          #(-> % (process [down down]) (project-preview-cursor) (= [0 2]))
          #(-> % (at-view-bottom) (process [down]) (project-preview-cursor) (= [0 end-y]))
          #(-> % (at-view-bottom) (process [down down]) (project-preview-cursor) (= [0 end-y]))))))

(defn cursor-projection [ctx]
  (total-cursor-projection ctx)
  (paged-cursor-projection ctx))

(defspec cursor-projection-test
         NR-OF-TESTS
         (for-all [ctx (gen-context {:prefilled-size 20
                                     :view-size      7
                                     :text-area      (gen-text-area-of 10)})]
                  (cursor-projection ctx)))

;; VIII. Y Projection

(defn- bounded? [ctx y]
  (<= 0 y (r/view-size ctx)))

(defn top-bounded-y-projection [ctx]
  (-> ctx
      (at-input-end)
      (at-line-start)
      (should-be #(bounded? % (-> % (process (repeat 5 up)) (project-y)))
                 #(= 0 (-> % (process (repeat 15 up)) (project-y)))
                 #(= 0 (-> % (process (repeat 100 up)) (project-y))))))

(defn bottom-bounded-y-projection [ctx]
  (let [end-y (dec (r/view-size ctx))]                              ;; starts from 0
    (-> ctx
        (at-main-view-start)
        (process [up up up up up])
        (should-be #(bounded? % (-> % (process (repeat 10 down)) (project-y)))
                   #(= end-y (-> % (process (repeat 15 down)) (project-y)))
                   #(= end-y (-> % (process (repeat 100 down)) (project-y)))))))

(defn y-projection [ctx]
  (top-bounded-y-projection ctx)
  (bottom-bounded-y-projection ctx))

(defspec y-projection-test
         NR-OF-TESTS
         (for-all [ctx (gen-context {:prefilled-size 30
                                     :view-size      10
                                     :text-area      (gen-text-area-of 20)})]
                  (y-projection ctx)))


;; IX. Region diff

(defn- check-diff [{:keys [now then expected]}]
  (let [current (highlight-from now)
        former  (highlight-from then)
        result  (additive-diff current former)]
    (is (= expected (:region result)))))

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


(defn total-selection-law [ctx]
  "extract (clip-sel (select-all (hud))) == project-hud (hud)"
  (is true))

(defn distributive-projection-law [ctx]
  "project-sel (select (hud)) = map project-cursor (select (hud))"
  (is true))

(defn projection-containment-law [ctx]
  "extract (select (hud)) `contains` extract (project-hud (hud)) (project-sel (select (hud))"
  (is true))

(defn projection-laws [ctx]
  (total-selection-law ctx)
  (distributive-projection-law ctx)
  (projection-containment-law ctx))