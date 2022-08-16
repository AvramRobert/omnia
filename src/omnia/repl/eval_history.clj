(ns omnia.repl.eval-history
  (:require [schema.core :as s]
            [omnia.schema.text :refer [Text]]
            [omnia.schema.eval-history :refer [EvalHistory]]))

(s/defn create-eval-history :- EvalHistory
  [limit :- s/Int]
  {:evaluations []
   :position    0
   :limit       limit
   :temp        nil})

(s/defn evaluations :- [Text]
  [history :- EvalHistory]
  (:evaluations history))

(s/defn limit :- s/Int
  [history :- EvalHistory]
  (:limit history))

(s/defn size :- s/Int
  [history :- EvalHistory]
  (-> history (evaluations) (count)))

(s/defn temp :- (s/maybe Text)
  [history :- EvalHistory]
  (:temp history))

(s/defn position :- s/Int
  [history :- EvalHistory]
  (:position history))

(s/defn current-eval :- (s/maybe Text)
  [history :- EvalHistory]
  (let [position  (position history)
        evals     (evaluations history)
        temporary (temp history)]
    (nth evals position temporary)))

(s/defn travel-back :- EvalHistory
  [history :- EvalHistory]
  (let [position  (position history)
        evals     (evaluations history)
        limit     (limit history)
        temp      (temp history)
        position' (dec position)]
    {:evaluations evals
     :limit       limit
     :temp        temp
     :position    (if (> position' 0) position' 0)}))

(s/defn travel-forward :- EvalHistory
  [history :- EvalHistory]
  (let [size      (size history)
        position  (position history)
        evals     (evaluations history)
        limit     (limit history)
        threshold (min limit size)
        temp      (temp history)
        position' (inc position)]
    {:evaluations evals
     :limit       limit
     :temp        temp
     :position    (if (<= position' threshold) position' position)}))

(s/defn keep-temp :- EvalHistory
  [temp :- Text
   history :- EvalHistory]
  {:evaluations (evaluations history)
   :limit       (limit history)
   :position    (position history)
   :temp        temp})

(s/defn insert :- EvalHistory
  [text :- Text
   history :- EvalHistory]
  (let [limit    (limit history)
        temp     (temp history)
        position (position history)
        size     (size history)
        evals    (evaluations history)]
    {:evaluations (if (< size limit)
                    (conj evals text)
                    (conj (vec (rest evals)) text))
     :position    (if (< position limit)
                    (inc position)
                    position)
     :limit       limit
     :temp        temp}))

(s/defn reset :- EvalHistory
  [history :- EvalHistory]
  (let [evals     (evaluations history)
        size      (size history)
        limit     (limit history)
        temp      (temp history)
        position  (position history)
        position' (min limit size)
        temp'     nil]
    (if (and (= temp temp')
             (= position position'))
      history
      {:evaluations evals
       :limit       limit
       :position    position'
       :temp        temp'})))