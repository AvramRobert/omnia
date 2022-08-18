(ns omnia.repl.docs
  (:require [schema.core :as s]
            [omnia.schema.docs :refer [Docs]]
            [omnia.schema.view :refer [View]]))

(s/defn suggestions :- (s/maybe View)
  [docs :- Docs]
  (:suggestions docs))

(s/defn documentation :- (s/maybe View)
  [docs :- Docs]
  (:documentation docs))

(s/defn signatures :- (s/maybe View)
  [docs :- Docs]
  (:signatures docs))

(s/defn with-suggestions :- Docs
  [docs :- Docs
   view :- View]
  {:suggestions   view
   :signatures    nil
   :documentation nil})

(s/defn with-documentation :- Docs
  [docs :- Docs
   view :- View]
  {:suggestions   nil
   :signatures    nil
   :documentation view})

(s/defn with-signatures :- Docs
  [docs :- Docs
   view :- View]
  {:suggestions   nil
   :signatures    view
   :documentation nil})

(s/def empty-docs :- Docs
  {:suggestions   nil
   :documentation nil
   :signatures    nil})
