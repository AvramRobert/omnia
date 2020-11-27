(ns omnia.event
  (:require [schema.core :as s]))

(def character :character)                                  ;; replaces char
(def expand :expand)
(def select-all :select-all)
(def paste :paste)
(def copy :copy)
(def cut :cut)
(def up :up)
(def down :down)
(def left :left)
(def right :right)
(def jump-left :jump-left)
(def jump-right :jump-right)
(def select-up :select-up)
(def select-down :select-down)
(def select-left :select-left)
(def select-right :select-right)
(def jump-select-left :jump-select-left)
(def jump-select-right :jump-select-right)
(def backspace :backspace)
(def delete :delete)
(def break :break)                                          ;; replaces newline
(def undo :undo)
(def redo :redo)

(def inject :inject)
(def docs :docs)
(def signature :signature)
(def match :match)
(def suggest :suggest)
(def scroll-up :scroll-up)
(def scroll-down :scroll-down)
(def prev-eval :prev-eval)
(def next-eval :next-eval)
(def indent :indent)                                        ;; replaces format
(def clear :clear)
(def evaluate :evaluate)                                    ;; replaces eval
(def exit :exit)
(def ignore :ignore)
(def refresh :refresh)

(def input-actions
  #{select-all
    expand
    select-right
    select-left
    select-down
    select-up
    copy
    cut
    paste
    undo
    redo
    delete
    break
    up
    down
    left
    right
    backspace
    jump-left
    jump-right
    jump-select-left
    jump-select-right})

(def hud-actions
  #{docs
    signature
    match
    suggest
    scroll-up
    scroll-down
    prev-eval
    next-eval
    indent
    clear
    evaluate
    exit
    ignore
    refresh})

(defn action? [action]
  (fn [event]
    (-> event (:action) (= action))))

(def CharEvent
  {:action (s/eq character)
   :value  Character})

(def InjectEvent
  {:action (s/eq inject)
   :value   s/Str})

(defn ControlEvent [actions]
  {:action (apply s/enum actions)})

(def InputEvent
  (s/conditional
    (action? character) CharEvent
    :else              (ControlEvent input-actions)))

(def HudEvent
  (s/conditional
    (action? inject)   InjectEvent
    :else              (ControlEvent hud-actions)))

(def Event
  (s/conditional
    (action? character) CharEvent
    (action? inject)    InjectEvent
    :else               (ControlEvent (concat input-actions hud-actions))))

(s/defn event
  ([action :- s/Keyword] :- Event
   {:action action})
  ([action :- s/Keyword
    value  :- s/Any] :- Event
   {:action action
    :value  value}))