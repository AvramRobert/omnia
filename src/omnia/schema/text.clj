(ns omnia.schema.text
  (:require [schema.core :as s]
            [omnia.schema.common :as u]))

(def Line [Character])

(def Expansion (s/enum :word :expr))

(def Text
  {:lines     [Line]
   ;; position in text. The cursor is placed at the index where a character can be input
   :cursor    u/Point
   :expansion Expansion
   :selection (s/maybe u/Region)
   :clipboard (s/maybe (s/recursive #'Text))})
