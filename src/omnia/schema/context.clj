(ns omnia.schema.context
  (:require [schema.core :as s]
            [omnia.schema.nrepl :as n]
            [omnia.schema.hud :as h]
            [omnia.schema.text :as t]
            [omnia.schema.config :as c]
            [omnia.schema.render :as r]))

(def Context
  {:nrepl         n/NReplClient
   :render        r/RenderingStrategy
   :previous-hud  h/View
   :persisted-hud h/View
   :preview-hud   h/View
   :input-area    t/Text
   :suggestions   h/View
   :documentation h/View
   :signatures    h/View
   :highlights    r/HighlightInstructions
   :garbage       r/HighlightInstructions})

(s/def ProcessingStep
  {:status  (s/enum :continue :terminate)
   :context Context})