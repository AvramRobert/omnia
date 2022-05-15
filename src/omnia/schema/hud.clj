(ns omnia.schema.hud
  (:require [schema.core :as s]
            [omnia.schema.text :as t]))

(def View
  {:text          t/Text
   :field-of-view s/Int
   :scroll-offset s/Int
   :view-offset   s/Int})