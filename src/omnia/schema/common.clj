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

(defn =>
  ([in out]
   (s/->FnSchema out in))
  ([in1 in2 out]
   (s/->FnSchema out [in1 in2]))
  ([in1 in2 in3 out]
   (s/->FnSchema out [in1 in2 in3])))