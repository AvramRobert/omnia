(ns omnia.schema.render
  (:require [schema.core :as s]
            [omnia.schema.common :as u]
            [omnia.schema.syntax :as x]
            [omnia.schema.config :as c]))

(def ^:const selection-highlight :selection-highlight)
(def ^:const open-paren-highlight :open-paren-highlight)
(def ^:const closed-paren-highlight :closed-paren-highlight)
(def ^:const manual-highlight :manual-highlight)

(def RenderingStrategy
  (s/enum :diff :total :clear))

(def HighlightInstructionData
  {:region u/Region
   :scheme c/Highlighting
   :styles [x/Style]})

(def HighlightInstructionType (s/enum selection-highlight open-paren-highlight closed-paren-highlight manual-highlight))

(def HighlightInstructions {HighlightInstructionType HighlightInstructionData})