(ns omnia.schema.context
  (:require [schema.core :as s]
            [omnia.schema.hud :refer [Hud]]
            [omnia.schema.history :refer [History]]
            [omnia.schema.config :refer [Config]]
            [omnia.schema.event :refer [Event]]
            [omnia.schema.common :refer [=>]]))

(def ^:const processing :processing)
(def ^:const terminated :terminated)

(def Status (s/enum processing terminated))

(def Context
  {:status  Status
   :history History
   :hud     Hud})

(def EventHandler
  (=> Context Event Config))
