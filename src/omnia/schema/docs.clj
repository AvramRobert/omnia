(ns omnia.schema.docs
  (:require [schema.core :as s]
            [omnia.schema.view :refer [View]]))

(def Docs
  {:suggestions   (s/maybe View)
   :signatures    (s/maybe View)
   :documentation (s/maybe View)})
