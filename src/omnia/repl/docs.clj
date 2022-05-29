(ns omnia.repl.docs
  (:require [schema.core :as s]
            [omnia.repl.view :as v]
            [omnia.schema.docs :refer [Docs]]
            [omnia.schema.view :refer [View]]
            [omnia.schema.text :refer [Text]]))

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

(s/defn suggest :- Docs
  [docs :- Docs
   text :- Text]
  (with-suggestions docs (v/riffle-window text 10)))

(s/def empty-docs :- Docs
  {:suggestions   nil
   :documentation nil
   :signatures    nil})