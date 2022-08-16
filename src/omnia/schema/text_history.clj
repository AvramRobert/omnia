(ns omnia.schema.text-history
  (:require [schema.core :as s]
            [omnia.schema.text :refer [Text]]))

(def Records [Text])

(def TextHistory
  {:records Records
   :size    s/Int
   :limit   s/Int})
