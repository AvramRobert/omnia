(ns omnia.render-test
  (:require [schema.core :as s]
            [omnia.repl.hud :as r]
            [omnia.repl.text :as i]
            [omnia.repl.events :as e]
            [omnia.schema.syntax :as ct]
            [omnia.config.defaults :as d]
            [omnia.test-utils :refer :all]
            [omnia.display.render :refer :all]
            [clojure.test :refer [deftest is]]
            [omnia.util.collection :refer [map-vals]]
            [omnia.schema.text :refer [Text]]
            [omnia.schema.hud :refer [Hud]]
            [omnia.schema.config :refer [Config]]
            [omnia.schema.common :refer [Point =>]]
            [omnia.display.terminal :as t])
  (:import (clojure.lang Atom)))

(def RenderedElements
  {:chars   Atom
   :cursors Atom
   :bgs     Atom
   :fgs     Atom
   :stls    Atom})

(s/defn execute :- RenderedElements
  [hud :- Hud,
   f   :- (=> t/Terminal Config Hud Hud)]
  (let [chars    (atom [])
        cursors  (atom [])
        bgs      (atom [])
        fgs      (atom [])
        stls     (atom [])
        acc      (fn [atm val] (swap! atm #(conj % val)))
        terminal (terminal {:put! (fn [_ ch x y fg bg stl]
                                    (acc bgs bg)
                                    (acc fgs fg)
                                    (run! #(acc stls %) stl)
                                    (acc chars ch)
                                    (acc cursors [x y]))
                            :size (fn [] (r/view-size hud))})
        _        (f terminal default-config hud)]
    {:chars   chars
     :cursors cursors
     :bgs     bgs
     :fgs     fgs
     :stls    stls}))

(def cleanup-background (get d/default-colours ct/default))

(s/defn selected-chars :- [Character]
  [text :- Text]
  (->> text
       (i/extract)
       (:lines)
       (mapcat identity)))

(s/defn selected-cursors :- [Point]
  [text :- Text]
  ;; Turn off schema due to the lines actually being cursors
  (s/without-fn-validation
    (->> text
         (:lines)
         (map-indexed
           (fn [y line]
             (map-indexed
               (fn [x _] [x y]) line)))
         (i/reset-lines text)
         (i/extract)
         (:lines)
         (mapcat identity))))

(s/defn inspect
  [state :- RenderedElements p]
  (->> state (map-vals deref) (p)))

;; I. Diffed rendering

(deftest clears-removed-characters-in-diff
  (let [hud         (-> ["persisted"
                         ---
                         -| "some"
                         -| "small |text"]
                        (derive-hud)
                        (process [e/jump-select-right e/delete-previous]))
        expected    (-> ["some"
                         "⦇small     ⦈"]
                        (derive-text))
        exp-chars   (selected-chars expected)
        exp-cursors (selected-cursors expected)]
    (-> hud
        (execute render-diff!)
        (inspect
          (fn [{:keys [chars cursors fgs bgs stls]}]
            (is (not (empty? bgs)))
            (is (not (empty? fgs)))
            (is (empty? stls))
            (is (= exp-cursors cursors))
            (is (= exp-chars chars)))))))

(deftest skips-rendering-when-no-diff
  (let [hud (-> ["persisted"
                 ---
                 -| "some"
                 -| "small |text"]
                (derive-hud)
                (process [e/move-right e/move-right]))]
    (-> hud
        (execute render-diff!)
        (inspect
          (fn [{:keys [chars cursors fgs bgs stls]}]
            (is (empty? chars))
            (is (empty? cursors))
            (is (empty? fgs))
            (is (empty? bgs))
            (is (empty? stls)))))))

(deftest falls-back-to-total-rendering-when-impossible-diff
  (let [hud         (-> ["persisted"
                         ---
                         "some"
                         -| "exceeding"
                         -| "|text"
                         -+ "limit"]
                        (derive-hud)
                        (process [e/select-up e/delete-previous]))
        expected    (-> ["⦇text     "
                         "limit⦈"]
                        (derive-text))
        exp-chars   (selected-chars expected)
        exp-cursors (selected-cursors expected)]
    (-> hud
        (execute render-diff!)
        (inspect
          (fn [{:keys [chars cursors fgs bgs stls]}]
            (is (not (empty? bgs)))
            (is (not (empty? fgs)))
            (is (empty? stls))
            (is (= exp-cursors cursors))
            (is (= exp-chars chars)))))))

;; II. No rendering

(deftest renders-nothing
  (let [hud (-> ["persisted"
                 ---
                 -| "some"
                 -| "text"]
                (derive-hud))]
    (-> hud
        (execute render-nothing!)
        (inspect
          (fn [{:keys [chars cursors fgs bgs stls]}]
            (is (empty? chars))
            (is (empty? cursors))
            (is (empty? fgs))
            (is (empty? bgs))
            (is (empty? stls)))))))

;; III. Selection highlighting

(deftest renders-only-selected-areas
  (let [hud         (-> ["persisted"
                         ---
                         -| "some"
                         -| "te|xt"]
                        (derive-hud)
                        (process [e/select-right]))
        expected    (-> ["some"
                         "te⦇x⦈t"]
                        (derive-text))
        exp-chars   (selected-chars expected)
        exp-cursors (selected-cursors expected)]
    (-> hud
        (execute (fn [terminal config hud]
                   (render-highlights! terminal hud)))
        (inspect
          (fn [{:keys [chars cursors fgs bgs stls]}]
            (is (not (empty? bgs)))
            (is (not (empty? fgs)))
            (is (empty? stls))
            (is (= exp-cursors cursors))
            (is (= exp-chars chars)))))))

(deftest renders-using-styles
  (let [hud            (-> ["persisted"
                            ---
                            -| "(|some)"
                            -| "text"]
                           (derive-hud)
                           (process [e/move-left]))
        expected-left  (-> ["⦇(⦈some)"
                            "text"]
                           (derive-text))
        expected-right (-> ["(some⦇)⦈"
                            "text"]
                           (derive-text))
        exp-cursors    (concat (selected-cursors expected-left)
                               (selected-cursors expected-right))
        exp-chars      (concat (selected-chars expected-left)
                               (selected-chars expected-right))
        exp-style      [:underline :underline]]
    (-> hud
        (execute (fn [terminal config hud]
                   (render-highlights! terminal hud)))
        (inspect
          (fn [{:keys [chars cursors fgs bgs stls]}]
            (is (not (empty? bgs)))
            (is (not (empty? fgs)))
            (is (= exp-style stls))
            (is (= exp-cursors cursors))
            (is (= exp-chars chars)))))))

(deftest renders-only-viewable-selections
  (let [hud         (-> ["persisted"
                         ---
                         -| "(|some"
                         -| "piece"
                         -+ "unviewable)"]
                        (derive-hud)
                        (process [e/move-left]))
        expected    (-> ["⦇(⦈some"
                         "piece"]
                        (derive-text))
        exp-chars   (selected-chars expected)
        exp-cursors (selected-cursors expected)
        exp-style   [:underline]]
    (-> hud
        (execute (fn [terminal config hud]
                   (render-highlights! terminal hud)))
        (inspect
          (fn [{:keys [chars cursors fgs bgs stls]}]
            (is (not (empty? bgs)))
            (is (not (empty? fgs)))
            (is (= exp-style stls))
            (is (= exp-cursors cursors))
            (is (= exp-chars chars)))))))

;; IV. Clean-up highlighting

(deftest cleans-up-single-lines-through-partial-cull
  (let [hud         (-> ["persisted"
                         ---
                         -| "|this is a hud"]
                        (derive-hud)
                        (process [e/select-right e/select-right e/select-right e/select-left]))
        expected    (-> ["th⦇i⦈s is a hud"] (derive-text))
        exp-chars   (selected-chars expected)
        exp-cursors (selected-cursors expected)]
    (-> hud
        (execute (fn [terminal _ hud]
                   (clean-highlights! terminal hud)))
        (inspect
          (fn [{:keys [chars cursors bgs fgs]}]
            (is (= exp-chars chars))
            (is (= exp-cursors cursors))
            (is (= cleanup-background (first (distinct bgs))))
            (is (not (empty? fgs))))))))

(deftest cleans-up-single-lines-through-total-cull
  (let [hud         (-> ["persisted"
                         ---
                         -| "|This is a hud"]
                        (derive-hud)
                        (process [e/move-right e/select-right e/select-right e/move-left]))
        expected    (-> ["T⦇hi⦈s is a hud"]
                        (derive-text))
        exp-chars   (selected-chars expected)
        exp-cursors (selected-cursors expected)]
    (-> hud
        (execute (fn [terminal _ hud]
                   (clean-highlights! terminal hud)))
        (inspect
          (fn [{:keys [chars cursors bgs fgs]}]
            (is (= exp-chars chars))
            (is (= exp-cursors cursors))
            (is (= cleanup-background (first (distinct bgs))))
            (is (not (empty? fgs))))))))

(deftest cleans-up-multiple-lines-through-partial-cull
  (let [hud         (-> ["persisted"
                         ---
                         -| "These |are"
                         -| "multiple lines"]
                        (derive-hud)
                        (process [e/select-down e/select-left]))
        expected    (-> ["These are"
                         "multi⦇p⦈le lines"]
                        (derive-text))
        exp-chars   (selected-chars expected)
        exp-cursors (selected-cursors expected)]
    (-> hud
        (execute (fn [terminal _ hud]
                   (clean-highlights! terminal hud)))
        (inspect
          (fn [{:keys [chars cursors bgs fgs]}]
            (is (= exp-chars chars))
            (is (= exp-cursors cursors))
            (is (= cleanup-background (first (distinct bgs))))
            (is (not (empty? fgs))))))))

(deftest cleans-up-multiple-lines-through-complete-cull
  (let [hud         (-> ["persisted"
                         ---
                         -| "These |are"
                         -| "multiple lines"]
                        (derive-hud)
                        (process [e/select-down e/move-right]))
        expected    (-> ["These ⦇are"
                         "multip⦈le lines"]
                        (derive-text))
        exp-chars   (selected-chars expected)
        exp-cursors (selected-cursors expected)]
    (-> hud
        (execute (fn [terminal _ hud]
                   (clean-highlights! terminal hud)))
        (inspect
          (fn [{:keys [chars cursors bgs fgs]}]
            (is (= exp-chars chars))
            (is (= exp-cursors cursors))
            (is (= cleanup-background (first (distinct bgs))))
            (is (not (empty? fgs))))))))

;; V. Region diff

(defn- check-diff [{:keys [now then expected]}]
  (let [current (highlight-from now)
        former  (highlight-from then)
        result  (additive-diff current former)]
    (is (= expected (:region result)))))

(deftest upper-x-diff
  (let [a {:from [4 1] :until [4 3]}
        b {:from [0 1] :until [4 3]}
        r {:from [0 1] :until [4 1]}]
    (check-diff {:now a :then b :expected nil})
    (check-diff {:now b :then a :expected r})))

(deftest lower-x-diff
  (let [a {:from [2 1] :until [2 4]}
        b {:from [2 1] :until [5 4]}
        r {:from [2 4] :until [5 4]}]
    (check-diff {:now a :then b :expected nil})
    (check-diff {:now b :then a :expected r})))

(deftest upper-y-diff
  (let [a {:from [4 4] :until [7 6]}
        b {:from [2 1] :until [7 6]}
        r {:from [2 1] :until [4 4]}]
    (check-diff {:now a :then b :expected nil})
    (check-diff {:now b :then a :expected r})))

(deftest lower-y-diff
  (let [a {:from [2 1] :until [4 2]}
        b {:from [2 1] :until [6 5]}
        r {:from [4 2] :until [6 5]}]
    (check-diff {:now a :then b :expected nil})
    (check-diff {:now b :then a :expected r})))

(deftest scissor-upper-y
  (let [a  {:from [2 1] :until [4 1]}
        a' {:from [2 0] :until [4 1]}
        b  {:from [4 1] :until [2 2]}
        r  {:from [2 0] :until [2 1]}]
    (check-diff {:now a :then b :expected a})
    (check-diff {:now a' :then a :expected r})))

(deftest scissor-lower-y
  (let [a  {:from [2 2] :until [4 2]}
        a' {:from [2 2] :until [4 3]}
        b  {:from [4 1] :until [2 2]}
        r  {:from [4 2] :until [4 3]}]
    (check-diff {:now a :then b :expected a})
    (check-diff {:now a' :then a :expected r})))

(deftest keep-diff
  (let [a {:from [2 3] :until [4 5]}
        b {:from [2 3] :until [4 5]}]
    (check-diff {:now a :then b :expected nil})))

(deftest no-diff
  (let [a {:from [2 3] :until [4 5]}
        b {:from [4 5] :until [4 6]}]
    (check-diff {:now a :then b :expected a})
    (check-diff {:now b :then a :expected b})))
