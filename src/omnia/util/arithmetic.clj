(ns omnia.util.arithmetic
  (:require [schema.core :as s]
            [omnia.schema.common :refer [NonNegativeInt]]))

(s/defn dec< :- s/Int
  [value :- s/Int
   min   :- s/Int]
  (let [x (dec value)]
    (if (< x min) value x)))

(s/defn -- :- NonNegativeInt
  [& values :- [s/Int]]
  (let [r (apply - values)]
    (if (neg? r) 0 r)))

(s/defn ++ :- NonNegativeInt
  [& values :- [s/Int]]
  (let [r (apply + values)]
    (if (neg? r) 0 r)))

(s/defn mod* :- NonNegativeInt
  [num :- s/Int
   div :- s/Int]
  (let [pdiv (if (zero? div) 1 div)]
    (mod num pdiv)))