(ns omnia.render-test
  (:require [schema.core :as s]
            [omnia.repl.context :as r]
            [omnia.repl.text :as i]
            [omnia.repl.events :as e]
            [omnia.schema.syntax :as ct]
            [omnia.config.defaults :as d]
            [omnia.test-utils :refer :all]
            [omnia.view.render :refer :all]
            [clojure.test :refer [deftest is]]
            [omnia.util.collection :refer [map-vals]]
            [omnia.schema.text :refer [Text]]
            [omnia.schema.hud :refer [Hud]]
            [omnia.schema.context :refer [Context]]
            [omnia.schema.config :refer [Config]]
            [omnia.schema.common :refer [Point Region =>]]
            [omnia.view.terminal :as t])
  (:import (clojure.lang Atom)))

(def RenderedElements
  {:chars   Atom
   :cursors Atom
   :bgs     Atom
   :fgs     Atom
   :stls    Atom})

(s/defn execute :- RenderedElements
  [context :- Context,
   f       :- (=> t/Terminal Config Context Context)]
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
                            :size (fn [] (r/view-size context))})
        _        (f terminal default-config context)]
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
  (letfn [(index [lines]
            (map-indexed
              (fn [y line]
                (map-indexed
                  (fn [x _] [x y]) line)) lines))]
    (->> (i/rebase text index)
         (i/extract)
         (:lines)
         (mapcat identity))))

(s/defn inspect
  [state :- RenderedElements p]
  (->> state (map-vals deref) (p)))

;; I. Diffed rendering

(deftest clears-removed-characters-in-diff
  (let [context     (-> ["persisted"
                         ---
                         -| "some"
                         -| "small |text"]
                        (derive-context)
                        (process [e/jump-select-right e/delete-previous]))
        expected    (-> ["some"
                         "⦇small     ⦈"]
                        (derive-text))
        exp-chars   (selected-chars expected)
        exp-cursors (selected-cursors expected)]
    (-> context
        (execute render-diff!)
        (inspect
          (fn [{:keys [chars cursors fgs bgs stls]}]
            (is (not (empty? bgs)))
            (is (not (empty? fgs)))
            (is (empty? stls))
            (is (= exp-cursors cursors))
            (is (= exp-chars chars)))))))

(deftest skips-rendering-when-no-diff
  (let [context (-> ["persisted"
                     ---
                     -| "some"
                     -| "small |text"]
                    (derive-context)
                    (process [e/move-right e/move-right]))]
    (-> context
        (execute render-diff!)
        (inspect
          (fn [{:keys [chars cursors fgs bgs stls]}]
            (is (empty? chars))
            (is (empty? cursors))
            (is (empty? fgs))
            (is (empty? bgs))
            (is (empty? stls)))))))

(deftest falls-back-to-total-rendering-when-impossible-diff
  (let [context     (-> ["persisted"
                         ---
                         "some"
                         -| "exceeding"
                         -| "|text"
                         -+ "limit"]
                        (derive-context)
                        (process [e/select-up e/delete-previous]))
        expected    (-> ["⦇text     "
                         "limit⦈"]
                        (derive-text))
        exp-chars   (selected-chars expected)
        exp-cursors (selected-cursors expected)]
    (-> context
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
  (let [context (-> ["persisted"
                     ---
                     -| "some"
                     -| "text"]
                    (derive-context))]
    (-> context
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
  (let [context     (-> ["persisted"
                         ---
                         -| "some"
                         -| "te|xt"]
                        (derive-context)
                        (process [e/select-right]))
        expected    (-> ["some"
                         "te⦇x⦈t"] (derive-text))
        exp-chars   (selected-chars expected)
        exp-cursors (selected-cursors expected)]
    (-> context
        (execute (fn [terminal config context]
                   (render-highlights! terminal context)))
        (inspect
          (fn [{:keys [chars cursors fgs bgs stls]}]
            (is (not (empty? bgs)))
            (is (not (empty? fgs)))
            (is (empty? stls))
            (is (= exp-cursors cursors))
            (is (= exp-chars chars)))))))

(deftest renders-using-styles
  (let [context        (-> ["persisted"
                            ---
                            -| "(|some)"
                            -| "text"]
                           (derive-context)
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
    (-> context
        (execute (fn [terminal config context]
                   (render-highlights! terminal context)))
        (inspect
          (fn [{:keys [chars cursors fgs bgs stls]}]
            (is (not (empty? bgs)))
            (is (not (empty? fgs)))
            (is (= exp-style stls))
            (is (= exp-cursors cursors))
            (is (= exp-chars chars)))))))

(deftest renders-only-viewable-selections
  (let [context     (-> ["persisted"
                         ---
                         -| "(|some"
                         -| "piece"
                         -+ "unviewable)"]
                        (derive-context)
                        (process [e/move-left]))
        expected    (-> ["⦇(⦈some"
                         "piece"]
                        (derive-text))
        exp-chars   (selected-chars expected)
        exp-cursors (selected-cursors expected)
        exp-style   [:underline]]
    (-> context
        (execute (fn [terminal config context]
                   (render-highlights! terminal context)))
        (inspect
          (fn [{:keys [chars cursors fgs bgs stls]}]
            (is (not (empty? bgs)))
            (is (not (empty? fgs)))
            (is (= exp-style stls))
            (is (= exp-cursors cursors))
            (is (= exp-chars chars)))))))

;; IV. Clean-up highlighting

(deftest cleans-up-single-lines-through-partial-cull
  (let [context     (-> ["persisted"
                         ---
                         -| "|this is a context"]
                        (derive-context)
                        (process [e/select-right e/select-right e/select-right e/select-left]))
        expected    (-> ["th⦇i⦈s is a context"] (derive-text))
        exp-chars   (selected-chars expected)
        exp-cursors (selected-cursors expected)]
    (-> context
        (execute (fn [terminal _ context]
                   (clean-highlights! terminal context)))
        (inspect
          (fn [{:keys [chars cursors bgs fgs]}]
            (is (= exp-chars chars))
            (is (= exp-cursors cursors))
            (is (= cleanup-background (first (distinct bgs))))
            (is (not (empty? fgs))))))))

(deftest cleans-up-single-lines-through-total-cull
  (let [context     (-> ["persisted"
                         ---
                         -| "|This is a context"]
                        (derive-context)
                        (process [e/move-right e/select-right e/select-right e/move-left]))
        expected    (-> ["T⦇hi⦈s is a context"]
                        (derive-text))
        exp-chars   (selected-chars expected)
        exp-cursors (selected-cursors expected)]
    (-> context
        (execute (fn [terminal _ context]
                   (clean-highlights! terminal context)))
        (inspect
          (fn [{:keys [chars cursors bgs fgs]}]
            (is (= exp-chars chars))
            (is (= exp-cursors cursors))
            (is (= cleanup-background (first (distinct bgs))))
            (is (not (empty? fgs))))))))

(deftest cleans-up-multiple-lines-through-partial-cull
  (let [context     (-> ["persisted"
                         ---
                         -| "These |are"
                         -| "multiple lines"]
                        (derive-context)
                        (process [e/select-down e/select-left]))
        expected    (-> ["These are"
                         "multi⦇p⦈le lines"]
                        (derive-text))
        exp-chars   (selected-chars expected)
        exp-cursors (selected-cursors expected)]
    (-> context
        (execute (fn [terminal _ context]
                   (clean-highlights! terminal context)))
        (inspect
          (fn [{:keys [chars cursors bgs fgs]}]
            (is (= exp-chars chars))
            (is (= exp-cursors cursors))
            (is (= cleanup-background (first (distinct bgs))))
            (is (not (empty? fgs))))))))

(deftest cleans-up-multiple-lines-through-complete-cull
  (let [context     (-> ["persisted"
                         ---
                         -| "These |are"
                         -| "multiple lines"]
                        (derive-context)
                        (process [e/select-down e/move-right]))
        expected    (-> ["These ⦇are"
                         "multip⦈le lines"]
                        (derive-text))
        exp-chars   (selected-chars expected)
        exp-cursors (selected-cursors expected)]
    (-> context
        (execute (fn [terminal _ context]
                   (clean-highlights! terminal context)))
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
  (let [a {:start [4 1] :end [4 3]}
        b {:start [0 1] :end [4 3]}
        r {:start [0 1] :end [4 1]}]
    (check-diff {:now a :then b :expected nil})
    (check-diff {:now b :then a :expected r})))

(deftest lower-x-diff
  (let [a {:start [2 1] :end [2 4]}
        b {:start [2 1] :end [5 4]}
        r {:start [2 4] :end [5 4]}]
    (check-diff {:now a :then b :expected nil})
    (check-diff {:now b :then a :expected r})))

(deftest upper-y-diff
  (let [a {:start [4 4] :end [7 6]}
        b {:start [2 1] :end [7 6]}
        r {:start [2 1] :end [4 4]}]
    (check-diff {:now a :then b :expected nil})
    (check-diff {:now b :then a :expected r})))

(deftest lower-y-diff
  (let [a {:start [2 1] :end [4 2]}
        b {:start [2 1] :end [6 5]}
        r {:start [4 2] :end [6 5]}]
    (check-diff {:now a :then b :expected nil})
    (check-diff {:now b :then a :expected r})))

(deftest scissor-upper-y
  (let [a  {:start [2 1] :end [4 1]}
        a' {:start [2 0] :end [4 1]}
        b  {:start [4 1] :end [2 2]}
        r  {:start [2 0] :end [2 1]}]
    (check-diff {:now a :then b :expected a})
    (check-diff {:now a' :then a :expected r})))

(deftest scissor-lower-y
  (let [a  {:start [2 2] :end [4 2]}
        a' {:start [2 2] :end [4 3]}
        b  {:start [4 1] :end [2 2]}
        r  {:start [4 2] :end [4 3]}]
    (check-diff {:now a :then b :expected a})
    (check-diff {:now a' :then a :expected r})))

(deftest keep-diff
  (let [a {:start [2 3] :end [4 5]}
        b {:start [2 3] :end [4 5]}]
    (check-diff {:now a :then b :expected nil})))

(deftest no-diff
  (let [a {:start [2 3] :end [4 5]}
        b {:start [4 5] :end [4 6]}]
    (check-diff {:now a :then b :expected a})
    (check-diff {:now b :then a :expected b})))
