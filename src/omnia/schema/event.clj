(ns omnia.schema.event
  (:require [schema.core :as s]))

(def character :character)
(def expand :expand)
(def select-all :select-all)
(def paste :paste)
(def copy :copy)
(def cut :cut)
(def move-up :move-up)
(def move-down :move-down)
(def move-left :move-left)
(def move-right :move-right)
(def jump-left :jump-left)
(def jump-right :jump-right)
(def select-up :select-up)
(def select-down :select-down)
(def select-left :select-left)
(def select-right :select-right)
(def jump-select-left :jump-select-left)
(def jump-select-right :jump-select-right)
(def delete-previous :delete-previous)
(def delete-current :delete-current)
(def new-line :new-line)
(def undo :undo)
(def redo :redo)

(def inject :inject)
(def docs :docs)
(def signature :signature)
(def paren-match :paren-match)
(def suggest :suggest)
(def scroll-up :scroll-up)
(def scroll-down :scroll-down)
(def prev-eval :prev-eval)
(def next-eval :next-eval)
(def indent :indent)
(def clear :clear)
(def evaluate :evaluate)
(def exit :exit)
(def ignore :ignore)
(def resize :resize)

(def text-actions
  #{character
    select-all
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
    delete-current
    new-line
    move-up
    move-down
    move-left
    move-right
    delete-previous
    jump-left
    jump-right
    jump-select-left
    jump-select-right})

(def context-actions
  #{inject
    docs
    signature
    paren-match
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
    resize})

(defn- action? [action]
  (fn [event]
    (-> event (:action) (= action))))

(def TextAction (apply s/enum text-actions))
(def ContextAction (apply s/enum context-actions))
(def Action (s/cond-pre TextAction ContextAction))

(def CharEvent
  {:action (s/eq character)
   :value  Character})

(def InjectEvent
  {:action (s/eq inject)
   :value   s/Str})

(def ControlEvent
  {:action Action})

(def Event
  (s/conditional
    (action? character) CharEvent
    (action? inject)    InjectEvent
    :else               ControlEvent))