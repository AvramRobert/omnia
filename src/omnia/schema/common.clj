(ns omnia.schema.common
  (:require [schema.core :as s])
  (:import (java.util UUID)))

(def Point
  (s/constrained [s/Int] #(= 2 (count %))))

(def Region
  "Denotes a region between two points.
    from - inclusive
    until - exclusive"
  {:from  Point
   :until Point})

(def Pair
  {:left  Region
   :right Region})

(def StringUUID
  (s/pred #(try (UUID/fromString %) true (catch Exception _ false))))

(def NonNegativeInt
  (s/pred #(and (int? %) (>= 0 %))))