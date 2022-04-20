(ns omnia.schema.text
  (:require [schema.core :as s]
            [omnia.schema.common :as u]))

(def Line [Character])

(def Expansion (s/enum :word :expr))

(def Seeker
  {:lines     [Line]
   ;; position in text, the cursor is placed at the index where a character can be input
   :cursor    u/Point
   :size      s/Int
   :expansion Expansion
   :history   [(s/recursive #'Seeker)]
   :rhistory  [(s/recursive #'Seeker)]
   ;; range of text selected. Inclusive start and exclusive in end
   :selection (s/maybe u/Region)
   :clipboard (s/maybe (s/recursive #'Seeker))})