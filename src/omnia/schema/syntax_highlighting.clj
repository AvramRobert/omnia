(ns omnia.schema.syntax-highlighting
  (:require [schema.core :as s]
            [omnia.schema.syntax :refer [SyntaxElement]]
            [omnia.schema.common :refer [=>]]))

(def ^:const open-list-node :open-list)
(def ^:const closed-list-node :close-list)
(def ^:const open-vector-node :open-vector)
(def ^:const closed-vector-node :close-vector)
(def ^:const open-map-node :open-map)
(def ^:const closed-map-node :close-map)
(def ^:const escape-node :escape)
(def ^:const character-node :character)
(def ^:const special-character-node :special-character)
(def ^:const number-node :number)
(def ^:const open-string-node :open-string)
(def ^:const closed-string-node :close-string)
(def ^:const keyword-node :keyword)
(def ^:const function-node :function)
(def ^:const comment-node :comment)
(def ^:const word-node :word)
(def ^:const text-node :text)
(def ^:const break-node :break)
(def ^:const space-node :space)
(def ^:const comma-node :comma)

(def nodes
  [open-list-node
   closed-list-node
   open-vector-node
   closed-vector-node
   open-map-node
   closed-map-node
   open-string-node
   closed-string-node
   escape-node
   character-node
   special-character-node
   number-node
   keyword-node
   function-node
   comment-node
   word-node
   text-node
   break-node
   space-node
   comma-node])

(def ^:const none :none)

(def Node (apply s/enum nodes))
(def TransitionFn (=> Character (s/maybe Node)))
(def EmissionFn (=> (s/maybe Node) [Character] SyntaxElement))
(def CharStream (s/cond-pre s/Str [Character]))
(def PairNode (s/cond-pre (s/eq none) Node))

(def State
  "State in the syntax highlighting state-machine:
   node       - unique node in the state-machine graph
   pair       - a node associated with this one which together form a pair
              - if defined, this state is pushed back and kept until a node matching its pair is encountered
   emission   - function that computes the emitted `SyntaxElement`
              - the computation is made based on a possibly pushed-down previous node
                and the characters accumulated while processing this state
   transition - function that computes the nodes this state can transition to"
  {:node           Node
   :pair           PairNode
   :emission       EmissionFn
   :transition     TransitionFn})
