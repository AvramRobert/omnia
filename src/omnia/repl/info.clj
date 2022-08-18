(ns omnia.repl.info
  (:require [schema.core :as s]
            [omnia.schema.view :refer [View]]
            [omnia.schema.info :refer [none signature documentation suggestion InfoType Info]]))

(s/def empty-info :- Info
  {:type none})

(s/defn info-type :- InfoType
  [info :- Info]
  (:type info))

(s/defn info-value :- (s/maybe View)
  [info :- Info]
  (:value info))

(s/defn suggestion? :- s/Bool
  [info :- Info]
  (= (info-type info) suggestion))

(s/defn signature? :- s/Bool
  [info :- Info]
  (= (info-type info) signature))

(s/defn documentation? :- s/Bool
  [info :- Info]
  (= (info-type info) documentation))

(s/defn none? :- s/Bool
  [info :- Info]
  (= (info-type info) none))

(s/defn create-documentation-info :- Info
  [value :- View]
  {:type documentation :value value})

(s/defn create-suggestion-info :- Info
  [value :- View]
  {:type suggestion :value value})

(s/defn create-signature-info :- Info
  [value :- View]
  {:type signature :value value})
