(ns omnia.repl.history
  (:require [schema.core :as s]
            [omnia.schema.history :refer [History Timeline]]))

(s/defn undo-history :- Timeline
  [history :- History]
  (:undo-history history))

(s/defn redo-history :- Timeline
  [history :- History]
  (:redo-history history))

(s/defn with-undo-history :- History
  [history  :- History
   timeline :- Timeline]
  (assoc history :undo-history timeline))

(s/defn with-redo-history :- History
  [history  :- History
   timeline :- Timeline]
  (assoc history :redo-history timeline))

;; this should have a limit
(s/defn undo :- History
  [history :- History]
  (let [undo (undo-history history)
        redo (redo-history history)]
    (-> history
        (with-undo-history (rest undo))
        (with-redo-history (cons (first undo) redo)))))

(s/defn redo :- History
  [history :- History]
  (let [undo (undo-history history)
        redo (redo-history history)]
    (-> history
        (with-undo-history (cons (first redo) undo))
        (with-redo-history (rest redo)))))
