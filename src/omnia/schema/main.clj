(ns omnia.schema.main
  (:require [schema.core :as s]))

(def ^:const app-path-arg "app-path")

(def ThrowableMap
  {(s/optional-key :cause) s/Str
   (s/optional-key :trace) [StackTraceElement]
   s/Any                   s/Any})

(def AppArgs
  {:app-path s/Str
   s/Any s/Any})
