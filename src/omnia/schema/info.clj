(ns omnia.schema.info
  (:require [schema.core :as s]
            [omnia.schema.view :refer [View]]))

(def ^:const none :none)
(def ^:const suggestion :suggestion)
(def ^:const signature :signature)
(def ^:const documentation :documentation)

(def InfoType
  (s/enum none
          suggestion
          signature
          documentation))

(def Info
  {:type                   InfoType
   (s/optional-key :value) View})
