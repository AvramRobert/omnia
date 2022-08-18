(ns omnia.schema.information
  (:require [schema.core :as s]
            [omnia.schema.view :refer [View]]))

(def Information
  {:suggestions   (s/maybe View)
   :signatures    (s/maybe View)
   :documentation (s/maybe View)})
