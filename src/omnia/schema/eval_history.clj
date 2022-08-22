(ns omnia.schema.eval-history
  (:require [schema.core :as s]
            [omnia.schema.text :refer [Text]]))

(def history-file-path :history-file-path)
(def history-size :history-size)

(def EvalHistory
  {:evaluations [Text]
   :position    s/Int
   :limit       s/Int})
