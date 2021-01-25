(ns omnia.config.components.input
  (:require [schema.core :as s]))

(def ControlKey
  (s/enum :up
          :down
          :left
          :right
          :delete
          :enter
          :backspace
          :tab
          :page-up
          :page-down
          :character
          :escape
          :insert
          :home
          :end
          :reverse-tab
          :f1
          :f2
          :f3
          :f4
          :f5
          :f6
          :f7
          :f8
          :f9
          :f10
          :f11
          :f12
          :f13
          :f14
          :f15
          :f16
          :f17
          :f18
          :f19
          :unknown
          :cursor-location
          :mouse-event
          :eof))

(def CharKey
  (s/pred char?))

(def Key
  (s/cond-pre CharKey ControlKey))