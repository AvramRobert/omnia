(ns omnia.schema.store
  (:require [schema.core :as s]
            [omnia.schema.text :refer [Text]]))

(def Timeframe [Text])

(def UndoRedoHistory
  {:timeframe Timeframe
   :size      s/Int
   :limit     s/Int})

(def EvalHistory
  {:timeframe Timeframe
   :temp      (s/maybe Text)
   :instant   s/Int
   :limit     s/Int})

(def History (s/either EvalHistory UndoRedoHistory))

(def Store
  {:undo-history UndoRedoHistory
   :redo-history UndoRedoHistory
   :eval-history EvalHistory})
