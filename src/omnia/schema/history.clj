(ns omnia.schema.history
  (:require [schema.core :as s]
            [omnia.schema.text :refer [Line]]))

(def TextInstant [Line])

(def Timeframe [TextInstant])

(def Epoch
  {:timeframe Timeframe
   :size      s/Int
   :limit     s/Int})

(def History
  {:undo-history Epoch
   :redo-history Epoch})
