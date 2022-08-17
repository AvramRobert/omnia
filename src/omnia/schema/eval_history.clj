(ns omnia.schema.eval-history
  (:require [schema.core :as s]
            [omnia.schema.text :refer [Text]]))

(def EvalHistory
  {:evaluations [Text]
   :position    s/Int
   :limit       s/Int})