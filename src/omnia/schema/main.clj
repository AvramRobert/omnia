(ns omnia.schema.main
  (:require [schema.core :as s]))

(def ThrowableMap
  {(s/optional-key :cause) s/Str
   (s/optional-key :trace) [StackTraceElement]
   s/Any                   s/Any})
