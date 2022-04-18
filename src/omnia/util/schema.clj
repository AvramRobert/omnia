(ns omnia.util.schema
  (:require [schema.core :as s])
  (:import (java.util UUID)))

(def Point
  (s/constrained [s/Int] #(= 2 (count %))))

(def Region
  {:start Point
   :end   Point})

(def Pair
  {:left  Region
   :right Region})

(def StringUUID
  (s/pred #(try (UUID/fromString %) true (catch Exception _ false))))

(def StringBool (s/enum "true" "false"))

(defn =>
  ([in out]
   (s/->FnSchema out in))
  ([in1 in2 out]
   (s/->FnSchema out [in1 in2]))
  ([in1 in2 in3 out]
   (s/->FnSchema out [in1 in2 in3])))

