(ns omnia.repl.information
  (:require [schema.core :as s]
            [omnia.schema.information :refer [Information]]
            [omnia.schema.view :refer [View]]))

(s/defn suggestions :- (s/maybe View)
  [information :- Information]
  (:suggestions information))

(s/defn documentation :- (s/maybe View)
  [information :- Information]
  (:documentation information))

(s/defn signatures :- (s/maybe View)
  [information :- Information]
  (:signatures information))

(s/defn with-suggestions :- Information
  [information :- Information
   view :- View]
  {:suggestions   view
   :signatures    nil
   :documentation nil})

(s/defn with-documentation :- Information
  [information :- Information
   view :- View]
  {:suggestions   nil
   :signatures    nil
   :documentation view})

(s/defn with-signatures :- Information
  [information :- Information
   view :- View]
  {:suggestions   nil
   :signatures    view
   :documentation nil})

(s/def empty-information :- Information
  {:suggestions   nil
   :documentation nil
   :signatures    nil})
