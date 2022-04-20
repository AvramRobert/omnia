(ns omnia.schema.hud
  (:require [schema.core :as s]
            [omnia.schema.text :as t]))

(def Hud
  {:text          t/Seeker
   :field-of-view s/Int
   :scroll-offset s/Int
   :view-offset   s/Int})