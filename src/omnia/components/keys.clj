(ns omnia.components.keys
  (:require [schema.core :as s]))

(def up :up)
(def down :down)
(def left :left)
(def right :right)
(def delete :delete)
(def enter :enter)
(def backspace :backspace)
(def tab :tab)
(def page-up :page-up)
(def page-down :page-down)
(def escape :escape)
(def insert :insert)
(def home :home)
(def end :end)
(def reverse-tab :reverse-tab)
(def f1 :f1)
(def f2 :f2)
(def f3 :f3)
(def f4 :f4)
(def f5 :f5)
(def f6 :f6)
(def f7 :f7)
(def f8 :f8)
(def f9 :f9)
(def f10 :f10)
(def f11 :f11)
(def f12 :f12)
(def f13 :f13)
(def f14 :f14)
(def f15 :f15)
(def f16 :f16)
(def f17 :f17)
(def f18 :f18)
(def f19 :f19)

(def key-set #{up
               down
               left
               right
               delete
               enter
               backspace
               tab
               page-up
               page-down
               escape
               insert
               home
               end
               reverse-tab
               f1
               f2
               f3
               f4
               f5
               f6
               f7
               f8
               f9
               f10
               f11
               f12
               f13
               f14
               f15
               f16
               f17
               f18
               f19})

(def ControlKey
  (apply s/enum key-set))

(def CharKey
  (s/pred char?))

(def Key
  (s/cond-pre CharKey ControlKey))