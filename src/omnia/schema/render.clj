(ns omnia.schema.render
  (:require [schema.core :as s]
            [omnia.schema.common :as u]
            [omnia.schema.syntax :as x]
            [omnia.schema.config :as c]))
(def RenderingStrategy
  (s/enum :diff :total :clear))

(def HighlightInfo
  {:region u/Region
   :scheme c/Highlighting
   :styles [x/Style]})

(def HighlightType (s/enum :selection :open-paren :closed-paren :manual))

(def Highlights {HighlightType HighlightInfo})