(ns omnia.repl.text-history
  (:require [schema.core :as s]
            [omnia.util.arithmetic :as ua]
            [omnia.schema.text :refer [Text]]
            [omnia.schema.text-history :refer [TextHistory]]))

(s/defn create-text-history :- TextHistory
  [limit :- s/Int]
  {:records '()
   :size    0
   :limit   limit})

(s/defn records :- [Text]
  [history :- TextHistory]
  (:records history))

(s/defn size :- s/Int
  [history :- TextHistory]
  (:size history))

(s/defn limit :- s/Int
  [history :- TextHistory]
  (:limit history))

(s/defn insert :- TextHistory
  [text :- Text
   history :- TextHistory]
  (let [limit   (limit history)
        size    (size history)
        records (records history)]
    {:records (if (< size limit)
                (cons text records)
                (cons text (take (dec limit) records)))
     :size    (if (< size limit)
                (inc size)
                limit)
     :limit   limit}))

(s/defn revert :- TextHistory
  [history :- TextHistory]
  (if (= 0 (size history))
    history
    {:records (rest (records history))
     :size    (ua/dec< (size history) 0)
     :limit   (limit history)}))

(s/defn next-record :- (s/maybe Text)
  [history :- TextHistory]
  (some-> history (records) (first)))