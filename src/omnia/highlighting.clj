(ns omnia.highlighting
  (:require [schema.core :as s]
            [omnia.more :refer [=>]]
            [clojure.set :refer [intersection]]))

;; FIXME: Make library. Name it polychrome

(def ^:const -list :list)
(def ^:const -vector :vector)
(def ^:const -map :map)
(def ^:const -set :set)
(def ^:const -number :number)
(def ^:const -char :character)
(def ^:const -keyword :keyword)
(def ^:const -text :text)
(def ^:const -string :string)
(def ^:const -comment :comment)
(def ^:const -function :function)
(def ^:const -word :word)
(def ^:const -select :selection)
(def ^:const -back :background)

(def ^:private ^:const  open-list-node :open-list)
(def ^:private ^:const  closed-list-node :close-list)
(def ^:private ^:const  open-vector-node :open-vector)
(def ^:private ^:const  closed-vector-node :close-vector)
(def ^:private ^:const  open-map-node :open-map)
(def ^:private ^:const  closed-map-node :close-map)
(def ^:private ^:const  character-node :character)
(def ^:private ^:const  number-node :number)
(def ^:private ^:const  open-string-node :open-string)
(def ^:private ^:const  closed-string-node :close-string)
(def ^:private ^:const  keyword-node :keyword)
(def ^:private ^:const  function-node :function)
(def ^:private ^:const  comment-node :comment)
(def ^:private ^:const  word-node :word)
(def ^:private ^:const  text-node :text)
(def ^:private ^:const  break-node :break)
(def ^:private ^:const  space-node :space)

(def Node (s/enum open-list-node
                  closed-list-node
                  open-vector-node
                  closed-vector-node
                  open-map-node
                  closed-map-node
                  open-string-node
                  closed-string-node
                  character-node
                  number-node
                  keyword-node
                  function-node
                  comment-node
                  word-node
                  text-node
                  break-node
                  space-node))

(def State
  {:node       Node
   :emission   (=> [Character] s/Keyword)
   :transition (=> Character (s/maybe Node))})

(def triggers
  {space-node         #{\space},
   closed-string-node #{\"},
   open-string-node   #{\"}
   number-node        #{\+ \- \0 \1 \2 \3 \4 \5 \6 \7 \8 \9},
   closed-map-node    #{\}},
   closed-list-node   #{\)},
   closed-vector-node #{\]},
   break-node         #{\newline},
   open-vector-node   #{\[},
   keyword-node       #{\:},
   character-node     #{\\},
   word-node          #{\f \n \t},
   comment-node       #{\;},
   open-map-node      #{\{},
   open-list-node     #{\(}})

(def words
  #{[\n \i \l]
    [\t \r \u \e]
    [\f \a \l \s \e]})

(s/defn transitions :- {Character Node}
  [& nodes :- [Node]]
  (letfn [(define [node]
            (->> node (triggers) (map #(vector % node))))]
    (->> nodes (mapcat define) (into {}))))

(s/def function :- State
  (let [lookup (transitions
                 open-list-node
                 closed-list-node
                 open-vector-node
                 closed-vector-node
                 open-map-node
                 closed-map-node
                 open-string-node
                 break-node
                 space-node
                 comment-node
                 character-node)]
    {:node       function-node
     :emission   (constantly -function)
     :transition #(lookup % function-node)}))

(s/def open-list :- State
  (let [lookup (transitions
                 break-node
                 space-node
                 open-list-node
                 closed-list-node
                 open-vector-node
                 closed-vector-node
                 open-map-node
                 closed-map-node
                 character-node
                 open-string-node
                 comment-node
                 keyword-node
                 number-node)]
    {:node       open-list-node
     :emission   (constantly -list)
     :transition #(lookup % function-node)}))

(s/def close-list :- State
  (let [lookup (transitions
                 break-node
                 space-node
                 word-node
                 open-list-node
                 closed-list-node
                 open-vector-node
                 closed-vector-node
                 open-map-node
                 closed-map-node
                 number-node
                 character-node
                 open-string-node
                 comment-node
                 keyword-node)]
    {:node       closed-list-node
     :emission   (constantly -list)
     :transition #(lookup % text-node)}))

(s/def text :- State
  (let [lookup (transitions
                 break-node
                 space-node
                 open-list-node
                 closed-list-node
                 open-vector-node
                 closed-vector-node
                 open-map-node
                 closed-map-node
                 character-node
                 open-string-node
                 comment-node)]
    {:node       text-node
     :emission   (constantly -text)
     :transition #(lookup % text-node)}))

(s/def break :- State
  (let [lookup (transitions
                 space-node
                 word-node
                 open-list-node
                 closed-list-node
                 open-vector-node
                 closed-vector-node
                 open-map-node
                 closed-map-node
                 number-node
                 character-node
                 open-string-node
                 comment-node
                 keyword-node)]
    {:node       break-node
     :emission   (constantly -text)
     :transition #(lookup % text-node)}))

(s/def space :- State
  (let [lookup (transitions
                 break-node
                 word-node
                 open-list-node
                 closed-list-node
                 open-vector-node
                 closed-vector-node
                 open-map-node
                 closed-map-node
                 number-node
                 character-node
                 open-string-node
                 comment-node
                 keyword-node
                 space-node)]
    {:node       space-node
     :emission   (constantly -text)
     :transition #(lookup % text-node)}))

(s/def word :- State
  (let [lookup (transitions
                 break-node
                 space-node
                 open-list-node
                 closed-list-node
                 open-vector-node
                 closed-vector-node
                 open-map-node
                 closed-map-node
                 open-string-node
                 number-node
                 character-node
                 comment-node
                 keyword-node)]
    {:node       word-node
     :emission   #(if (words %) -word -text)
     :transition #(lookup % word-node)}))

(s/def open-vector :- State
  (let [lookup (transitions
                 break-node
                 space-node
                 word-node
                 open-list-node
                 closed-list-node
                 open-vector-node
                 closed-vector-node
                 open-map-node
                 closed-map-node
                 number-node
                 character-node
                 open-string-node
                 comment-node
                 keyword-node)]
    {:node       open-vector-node
     :emission   (constantly -vector)
     :transition #(lookup % text-node)}))

(s/def close-vector :- State
  (let [lookup (transitions
                 break-node
                 space-node
                 word-node
                 open-list-node
                 closed-list-node
                 open-vector-node
                 closed-vector-node
                 open-map-node
                 closed-map-node
                 open-string-node
                 number-node
                 character-node
                 comment-node
                 keyword-node)]
    {:node       closed-vector-node
     :emission   (constantly -vector)
     :transition #(lookup % text-node)}))

(s/def open-map :- State
  (let [lookup (transitions
                 break-node
                 space-node
                 word-node
                 open-list-node
                 closed-list-node
                 open-vector-node
                 closed-vector-node
                 open-map-node
                 closed-map-node
                 open-string-node
                 number-node
                 character-node
                 comment-node
                 keyword-node)]
    {:node       open-map-node
     :emission   (constantly -map)
     :transition #(lookup % text-node)}))

(s/def close-map :- State
  (let [lookup (transitions
                 break-node
                 space-node
                 word-node
                 open-list-node
                 closed-list-node
                 open-vector-node
                 closed-vector-node
                 open-map-node
                 closed-map-node
                 open-string-node
                 number-node
                 character-node
                 comment-node
                 keyword-node)]
    {:node       closed-map-node
     :emission   (constantly -map)
     :transition #(lookup % text-node)}))

(s/def key-word :- State
  (let [lookup (transitions
                 break-node
                 space-node
                 open-list-node
                 closed-list-node
                 open-vector-node
                 closed-vector-node
                 open-map-node
                 closed-map-node
                 open-string-node
                 character-node
                 comment-node)]
    {:node       keyword-node
     :emission   (constantly -keyword)
     :transition #(lookup % keyword-node)}))

(s/def number :- State
  (let [lookup (transitions
                 break-node
                 space-node
                 open-list-node
                 closed-list-node
                 open-vector-node
                 closed-vector-node
                 open-map-node
                 closed-map-node
                 open-string-node
                 comment-node)]
    {:node       number-node
     :emission   (fn [[a b & _]]
                   (case [a b]
                     [\+ \+] -text
                     [\- \-] -text
                     [\+ \-] -text
                     [\- \+] -text
                     [\+ nil] -text
                     [\- nil] -text
                     [nil nil] -text
                     -number))
     :transition #(lookup % number-node)}))

(s/def open-string :- State
  (let [lookup (transitions closed-string-node)]
    {:node       open-string-node
     :emission   (constantly -string)
     :transition #(lookup % open-string-node)}))

(s/def character :- State
  (let [lookup (transitions
                 break-node
                 space-node)]
    {:node       character-node
     :emission   #(if (= (count %) 2) -char -text)
     :transition #(lookup % character-node)}))

(s/def com-ment :- State
  (let [lookup (transitions break-node)]
    {:node       comment-node
     :emission   (constantly -comment)
     :transition #(lookup % comment-node)}))

(s/def close-string :- State
  (let [lookup (transitions
                 break-node
                 space-node
                 word-node
                 open-list-node
                 closed-list-node
                 open-vector-node
                 closed-vector-node
                 open-map-node
                 closed-map-node
                 number-node
                 character-node
                 open-string-node
                 comment-node
                 keyword-node)]
    {:node       closed-string-node
     :emission   (constantly -string)
     :transition #(lookup % text-node)}))

(s/def node->state :- {Node State}
  {function-node      function
   text-node          text
   open-list-node     open-list
   closed-list-node   close-list
   open-vector-node   open-vector
   closed-vector-node close-vector
   open-map-node      open-map
   closed-map-node    close-map
   open-string-node   open-string
   closed-string-node close-string
   break-node         break
   space-node         space
   word-node          word
   keyword-node       key-word
   number-node        number
   character-node     character
   comment-node       com-ment})

(defn consume-with [f]
  (fn [[intermediate accumulate state] char]
    (let [node       (:node state)
          transition (:transition state)
          emission   (:emission state)
          node'      (transition char)
          state'     (node->state node')]
      (if (= node node')
        [intermediate (conj accumulate char) state']
        [(f intermediate state (emission accumulate) accumulate) [char] state']))))

;; We apply the function one last time to "flush" any accumulation that wasn't processed
;; Because the initial state is a `break`, the highlighter always emits an initial empty :text
(defn fold' [f init chars]
  (let [consumption        (consume-with f)
        [output acc state] (reduce consumption [init [] break] chars)
        emission           ((:emission state) acc)]
    (f output state emission acc)))

(defn fold [f init stream]
  (fold' (fn [output _ emission acc] (f output emission acc)) init stream))

(defn consume [f stream]
  (fold (fn [_ emission acc] (f emission acc)) nil stream))