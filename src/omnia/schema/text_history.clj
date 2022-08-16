(ns omnia.schema.text-history
  (:require [schema.core :as s]
            [omnia.schema.text :refer [Text]]))

(def TextHistory
  {:records  [Text]
   :size     s/Int
   :limit    s/Int
   :position s/Int})
