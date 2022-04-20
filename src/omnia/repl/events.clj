(ns omnia.repl.events
  (:require [schema.core :as s]
            [omnia.schema.event :as e]))

(s/defn event :- e/Event
  ([action :- e/Action] {:action action})
  ([action :- e/Action, value :- s/Any] {:action action :value value}))

(s/defn character :- e/Event
  [char :- Character]
  (event e/character char))

(s/def move-up :- e/Event
  (event e/move-up))

(s/def move-down :- e/Event
  (event e/move-down))

(s/def move-left :- e/Event
  (event e/move-left))

(s/def move-right :- e/Event
  (event e/move-right))

(s/def jump-left :- e/Event
  (event e/jump-left))

(s/def jump-right :- e/Event
  (event e/jump-right))

(s/def select-all :- e/Event
  (event e/select-all))

(s/def select-up :- e/Event
  (event e/select-up))

(s/def select-down :- e/Event
  (event e/select-down))

(s/def select-left :- e/Event
  (event e/select-left))

(s/def select-right :- e/Event
  (event e/select-right))

(s/def jump-select-left :- e/Event
  (event e/jump-select-left))

(s/def jump-select-right :- e/Event
  (event e/jump-select-right))

(s/def expand :- e/Event
  (event e/expand))

(s/def copy :- e/Event
  (event e/copy))

(s/def paste :- e/Event
  (event e/paste))

(s/def cut :- e/Event
  (event e/paste))

(s/def delete-previous :- e/Event
  (event e/delete-previous))

(s/def delete-current :- e/Event
  (event e/delete-current))

(s/def new-line :- e/Event
  (event e/new-line))

(s/def undo :- e/Event
  (event e/undo))

(s/def redo :- e/Event
  (event e/redo))

(s/def docs :- e/Event
  (event e/docs))

(s/def signature :- e/Event
  (event e/signature))

(s/def paren-match :- e/Event
  (event e/paren-match))

(s/def suggest :- e/Event
  (event e/suggest))

(s/def scroll-up :- e/Event
  (event e/scroll-up))

(s/def scroll-down :- e/Event
  (event e/scroll-down))

(s/def prev-eval :- e/Event
  (event e/prev-eval))

(s/def next-eval :- e/Event
  (event e/next-eval))

(s/def indent :- e/Event
  (event e/indent))

(s/def clear :- e/Event
  (event e/clear))

(s/def evaluate :- e/Event
  (event e/evaluate))

(s/def exit :- e/Event
  (event e/exit))

(s/def ignore :- e/Event
  (event e/ignore))

(s/defn resize :- e/Event
  [width :- s/Int, height :- s/Int]
  {:action e/resize :value [width height]})

(s/defn inject :- e/Event
  [code :- s/Str]
  (event e/inject code))