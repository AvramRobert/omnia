(ns omnia.schema.history
  (:require [schema.core :as s]
            [omnia.schema.text :refer [Line]]))

(def Timeframe [Line])

(def Timeline [Timeframe])

(def History
  {:undo-history Timeline
   :redo-history Timeline
   :eval-history Timeline})
