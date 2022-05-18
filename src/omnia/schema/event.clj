(ns omnia.schema.event
  (:require [schema.core :as s]))

(def ^:const character :character)
(def ^:const expand-selection :expand)
(def ^:const select-all :select-all)
(def ^:const paste :paste)
(def ^:const copy :copy)
(def ^:const cut :cut)
(def ^:const move-up :move-up)
(def ^:const move-down :move-down)
(def ^:const move-left :move-left)
(def ^:const move-right :move-right)
(def ^:const jump-left :jump-left)
(def ^:const jump-right :jump-right)
(def ^:const select-up :select-up)
(def ^:const select-down :select-down)
(def ^:const select-left :select-left)
(def ^:const select-right :select-right)
(def ^:const jump-select-left :jump-select-left)
(def ^:const jump-select-right :jump-select-right)
(def ^:const delete-previous :delete-previous)
(def ^:const delete-current :delete-current)
(def ^:const new-line :new-line)
(def ^:const undo :undo)
(def ^:const redo :redo)
(def ^:const inject :inject)
(def ^:const docs :docs)
(def ^:const signature :signature)
(def ^:const suggest :suggest)
(def ^:const scroll-up :scroll-up)
(def ^:const scroll-down :scroll-down)
(def ^:const prev-eval :prev-eval)
(def ^:const next-eval :next-eval)
(def ^:const indent :indent)
(def ^:const clear :clear)
(def ^:const evaluate :evaluate)
(def ^:const exit :exit)
(def ^:const ignore :ignore)
(def ^:const resize :resize)

(def actions
  #{character
    select-all
    expand-selection
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
    jump-select-right
    inject
    docs
    signature
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

(defn- is-action? [action]
  (fn [event]
    (-> event (:action) (= action))))

(def Action (apply s/enum actions))

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
    (is-action? character) CharEvent
    (is-action? inject)    InjectEvent
    :else                  ControlEvent))