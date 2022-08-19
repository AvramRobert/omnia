(ns omnia.schema.context
  (:require [schema.core :as s]
            [omnia.schema.hud :refer [Hud]]
            [omnia.schema.information :refer [Information]]
            [omnia.schema.config :refer [Config]]
            [omnia.schema.event :refer [Event]]
            [omnia.schema.text-history :refer [TextHistory]]
            [omnia.schema.eval-history :refer [EvalHistory]]))

(def ^:const processing :processing)
(def ^:const terminated :terminated)

(def Status (s/enum processing terminated))

(def Context
  {:status       Status
   :undo-history TextHistory
   :redo-history TextHistory
   :eval-history EvalHistory
   :information  Information
   :hud          Hud})

(def EventHandler
  (s/=> Context Event Config))
