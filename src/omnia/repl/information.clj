(ns omnia.repl.information
  (:require [schema.core :as s]
            [omnia.schema.view :refer [View]]
            [omnia.schema.information :refer [none signature documentation suggestion InformationType Information]]))

(s/def empty-information :- Information
  {:type none})

(s/defn info-type :- InformationType
  [info :- Information]
  (:type info))

(s/defn info-value :- (s/maybe View)
  [info :- Information]
  (:value info))

(s/defn suggestion? :- s/Bool
  [info :- Information]
  (= (info-type info) suggestion))

(s/defn signature? :- s/Bool
  [info :- Information]
  (= (info-type info) signature))

(s/defn documentation? :- s/Bool
  [info :- Information]
  (= (info-type info) documentation))

(s/defn none? :- s/Bool
  [info :- Information]
  (= (info-type info) none))

(s/defn create-documentation-info :- Information
  [value :- View]
  {:type documentation :value value})

(s/defn create-suggestion-info :- Information
  [value :- View]
  {:type suggestion :value value})

(s/defn create-signature-info :- Information
  [value :- View]
  {:type signature :value value})
