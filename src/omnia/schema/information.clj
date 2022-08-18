(ns omnia.schema.information
  (:require [schema.core :as s]
            [omnia.schema.view :refer [View]]))

(def ^:const none :none)
(def ^:const suggestion :suggestion)
(def ^:const signature :signature)
(def ^:const documentation :documentation)

(def InformationType
  (s/enum none
          suggestion
          signature
          documentation))

(def Information
  {:type                   InformationType
   (s/optional-key :value) View})
