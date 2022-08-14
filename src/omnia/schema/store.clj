(ns omnia.schema.store
  (:require [schema.core :as s]
            [omnia.schema.text :refer [Text]]))

(def Timeframe [Text])

(def LinearHistory
  {:timeframe Timeframe
   :size      s/Int
   :limit     s/Int})

(def EvalHistory
  {:timeframe Timeframe
   :temp      (s/maybe Text)
   :position  s/Int
   :limit     s/Int})

(def History (s/either EvalHistory LinearHistory))

(def Store
  {:undo-history LinearHistory
   :redo-history LinearHistory
   :eval-history EvalHistory})

(def SerialisedStore
  {:eval-history [s/Str]})