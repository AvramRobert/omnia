(ns omnia.schema.store
  (:require [schema.core :as s]
            [omnia.schema.text :refer [Line]]))

(def TimeEntry [Line])

(def Timeframe [TimeEntry])

(def LinearHistory
  {:timeframe Timeframe
   :size      s/Int
   :limit     s/Int})

(def IndexedHistory
  {:timeframe Timeframe
   :instant   s/Int
   :limit     s/Int})

(def History (s/either IndexedHistory LinearHistory))

(def Store
  {:undo-history LinearHistory
   :redo-history LinearHistory
   :eval-history IndexedHistory})
