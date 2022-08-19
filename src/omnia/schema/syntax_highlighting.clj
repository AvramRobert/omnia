(ns omnia.schema.syntax-highlighting
  (:require [schema.core :as s]
            [omnia.schema.syntax :refer [SyntaxElement]]))

(def ^:const -open-list :open-list)
(def ^:const -closed-list :close-list)
(def ^:const -open-vector :open-vector)
(def ^:const -closed-vector :close-vector)
(def ^:const -open-map :open-map)
(def ^:const -closed-map :close-map)
(def ^:const -escape-char :escape)
(def ^:const -char :character)
(def ^:const -special-char :special-character)
(def ^:const -number :number)
(def ^:const -open-string :open-string)
(def ^:const -closed-string :close-string)
(def ^:const -keyword :keyword)
(def ^:const -function :function)
(def ^:const -comment :comment)
(def ^:const -word :word)
(def ^:const -text :text)
(def ^:const -break :break)
(def ^:const -space :space)
(def ^:const -comma :comma)
(def ^:const -null :null)

(def nodes
  [-open-list
   -closed-list
   -open-vector
   -closed-vector
   -open-map
   -closed-map
   -open-string
   -closed-string
   -break
   -space
   -word
   -number
   -escape-char
   -comment
   -keyword
   -comma
   -special-char
   -function
   -text
   -char
   -null])

(def words
  #{[\n \i \l]
    [\t \r \u \e]
    [\f \a \l \s \e]})

(def special-characters
  #{[\n \e \w \l \i \n \e]
    [\s \p \a \c \e]})

(def digits
  #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})

(def signs
  #{\- \+})

(def Node (apply s/enum nodes))
(def EmissionFn (s/=> (s/maybe Node) [Character] SyntaxElement))
(def CharStream (s/cond-pre s/Str [Character]))