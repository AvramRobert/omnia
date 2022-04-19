(ns omnia.components.events
  (:require [schema.core :as s]
            [omnia.components.actions :as a]))

(defn- action? [action]
  (fn [event]
    (-> event (:action) (= action))))

(def CharEvent
  {:action (s/eq a/character)
   :value  Character})

(def InjectEvent
  {:action (s/eq a/inject)
   :value   s/Str})

(def ControlEvent
  {:action a/Action})

(def Event
  (s/conditional
    (action? a/character) CharEvent
    (action? a/inject)    InjectEvent
    :else                 ControlEvent))

(s/defn event :- Event
  ([action :- a/Action] {:action action})
  ([action :- a/Action, value :- s/Any] {:action action :value value}))

(s/defn character :- Event
  [char :- Character]
  (event a/character char))

(s/def move-up :- Event
  (event a/move-up))

(s/def move-down :- Event
  (event a/move-down))

(s/def move-left :- Event
  (event a/move-left))

(s/def move-right :- Event
  (event a/move-right))

(s/def jump-left :- Event
  (event a/jump-left))

(s/def jump-right :- Event
  (event a/jump-right))

(s/def select-all :- Event
  (event a/select-all))

(s/def select-up :- Event
  (event a/select-up))

(s/def select-down :- Event
  (event a/select-down))

(s/def select-left :- Event
  (event a/select-left))

(s/def select-right :- Event
  (event a/select-right))

(s/def jump-select-left :- Event
  (event a/jump-select-left))

(s/def jump-select-right :- Event
  (event a/jump-select-right))

(s/def expand :- Event
  (event a/expand))

(s/def copy :- Event
  (event a/copy))

(s/def paste :- Event
  (event a/paste))

(s/def cut :- Event
  (event a/paste))

(s/def delete-previous :- Event
  (event a/delete-previous))

(s/def delete-current :- Event
  (event a/delete-current))

(s/def new-line :- Event
  (event a/new-line))

(s/def undo :- Event
  (event a/undo))

(s/def redo :- Event
  (event a/redo))

(s/def docs :- Event
  (event a/docs))

(s/def signature :- Event
  (event a/signature))

(s/def paren-match :- Event
  (event a/paren-match))

(s/def suggest :- Event
  (event a/suggest))

(s/def scroll-up :- Event
  (event a/scroll-up))

(s/def scroll-down :- Event
  (event a/scroll-down))

(s/def prev-eval :- Event
  (event a/prev-eval))

(s/def next-eval :- Event
  (event a/next-eval))

(s/def indent :- Event
  (event a/indent))

(s/def clear :- Event
  (event a/clear))

(s/def evaluate :- Event
  (event a/evaluate))

(s/def exit :- Event
  (event a/exit))

(s/def ignore :- Event
  (event a/ignore))

(s/defn resize :- Event
  [width :- s/Int, height :- s/Int]
  {:action a/resize :value [width height]})

(s/defn inject :- Event
  [code :- s/Str]
  (event a/inject code))