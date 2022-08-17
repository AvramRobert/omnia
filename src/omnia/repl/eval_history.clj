(ns omnia.repl.eval-history
  (:require [schema.core :as s]
            [omnia.util.misc :as um]
            [omnia.schema.text :refer [Text]]
            [omnia.schema.eval-history :refer [EvalHistory]]
            [omnia.repl.text :as t]))

(s/defn create-eval-history :- EvalHistory
  [limit :- s/Int]
  {:evaluations []
   :position    0
   :limit       limit})

(s/defn evaluations :- [Text]
  [history :- EvalHistory]
  (:evaluations history))

(s/defn limit :- s/Int
  [history :- EvalHistory]
  (:limit history))

(s/defn size :- s/Int
  [history :- EvalHistory]
  (-> history (evaluations) (count)))

(s/defn position :- s/Int
  [history :- EvalHistory]
  (:position history))

(s/defn current-eval :- (s/maybe Text)
  [history :- EvalHistory]
  (let [position  (position history)
        evals     (evaluations history)]
    (nth evals position nil)))

(s/defn travel-back :- EvalHistory
  [history :- EvalHistory]
  (let [position  (position history)
        evals     (evaluations history)
        limit     (limit history)
        position' (dec position)]
    {:evaluations evals
     :limit       limit
     :position    (if (> position' 0) position' 0)}))

(s/defn travel-forward :- EvalHistory
  [history :- EvalHistory]
  (let [size      (size history)
        position  (position history)
        evals     (evaluations history)
        limit     (limit history)
        threshold (min limit size)
        position' (inc position)]
    {:evaluations evals
     :limit       limit
     :position    (if (<= position' threshold) position' position)}))

(s/defn insert :- EvalHistory
  [text :- Text
   history :- EvalHistory]
  (let [limit    (limit history)
        position (position history)
        size     (size history)
        evals    (evaluations history)]
    {:evaluations (if (< size limit)
                    (conj evals text)
                    (conj (vec (rest evals)) text))
     :position    (if (< position limit)
                    (inc position)
                    position)
     :limit       limit}))

(s/defn reset :- EvalHistory
  [history :- EvalHistory]
  (let [evals     (evaluations history)
        size      (size history)
        limit     (limit history)
        position  (position history)
        position' (min limit size)]
    (if (= position position')
      history
      {:evaluations evals
       :limit       limit
       :position    position'})))

(s/defn serialise :- [s/Str]
  [history :- EvalHistory]
  (->> history (evaluations) (mapv t/as-string)))

(s/defn read-eval-history :- EvalHistory
  [path :- s/Str
   limit :- s/Int]
  (let [history (create-eval-history limit)
        data    (um/slurp-or-else path [])]
    (->> data
         (mapv t/from-string)
         (reduce (fn [history' text] (insert text history')) history))))

(s/defn write-eval-history :- nil
  [path :- s/Str
   history :- EvalHistory]
  (->> history (serialise) (spit path)))