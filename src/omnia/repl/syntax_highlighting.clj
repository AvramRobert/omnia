(ns omnia.repl.syntax-highlighting
  (:require [schema.core :as s]
            [omnia.schema.common :refer [=>]]
            [omnia.schema.syntax :as t]
            [omnia.schema.syntax-highlighting :refer :all]))

(def triggers
  {space-node             #{\space},
   closed-string-node     #{\"},
   open-string-node       #{\"}
   number-node            #{\+ \- \0 \1 \2 \3 \4 \5 \6 \7 \8 \9},
   closed-map-node        #{\}},
   closed-list-node       #{\)},
   closed-vector-node     #{\]},
   break-node             #{\newline},
   open-vector-node       #{\[},
   keyword-node           #{\:},
   escape-node            #{\\},
   word-node              #{\f \n \t},
   special-character-node #{\s \n},
   comment-node           #{\;},
   open-map-node          #{\{},
   open-list-node         #{\(},
   comma-node             #{\,}})

(def words
  #{[\n \i \l]
    [\t \r \u \e]
    [\f \a \l \s \e]})

(def special-characters
  #{[\n \e \w \l \i \n \e]
    [\s \p \a \c \e]})

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
                 escape-node
                 comma-node)]
    {:node       function-node
     :pair       none
     :emission   (constantly t/functions)
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
                 escape-node
                 open-string-node
                 comment-node
                 keyword-node
                 number-node
                 comma-node)]
    {:node       open-list-node
     :pair       closed-list-node
     :emission   (constantly t/lists)
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
                 escape-node
                 open-string-node
                 comment-node
                 keyword-node
                 comma-node)]
    {:node       closed-list-node
     :pair       none
     :emission   (constantly t/lists)
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
                 escape-node
                 open-string-node
                 comment-node
                 comma-node)]
    {:node       text-node
     :pair       none
     :emission   (constantly t/texts)
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
                 escape-node
                 open-string-node
                 comment-node
                 keyword-node
                 break-node
                 comma-node)]
    {:node       break-node
     :pair       none
     :emission   (constantly t/texts)
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
                 escape-node
                 open-string-node
                 comment-node
                 keyword-node
                 space-node
                 comma-node)]
    {:node       space-node
     :pair       none
     :emission   (constantly t/texts)
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
                 comment-node
                 comma-node)]
    {:node       word-node
     :pair       none
     :emission   (fn [_ acc]
                   (if (words acc) t/words t/texts))
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
                 escape-node
                 open-string-node
                 comment-node
                 keyword-node
                 comma-node)]
    {:node       open-vector-node
     :pair       closed-vector-node
     :emission   (constantly t/vectors)
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
                 escape-node
                 comment-node
                 keyword-node
                 comma-node)]
    {:node       closed-vector-node
     :pair       none
     :emission   (constantly t/vectors)
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
                 escape-node
                 comment-node
                 keyword-node
                 comma-node)]
    {:node       open-map-node
     :pair       closed-map-node
     :emission   (constantly t/maps)
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
                 escape-node
                 comment-node
                 keyword-node
                 comma-node)]
    {:node       closed-map-node
     :pair       none
     :emission   (constantly t/maps)
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
                 escape-node
                 comment-node
                 comma-node)]
    {:node       keyword-node
     :pair       none
     :emission   (constantly t/keywords)
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
                 comment-node
                 comma-node)
        sign?     #(contains? #{\+ \-} %)
        number?   #(contains? #{\0 \1 \2 \3 \4 \5 \6 \7 \8} %)
        function? #(= % open-list-node)]
    {:node       number-node
     :pair       none
     :emission   (fn [n [a b & _]]
                   (cond
                     (and (sign? a) (number? b)) t/numbers
                     (number? a)                 t/numbers
                     (function? n)               t/functions
                     :else                       t/texts))
     :transition #(lookup % number-node)}))

(s/def open-string :- State
  (let [lookup (transitions closed-string-node)]
    {:node       open-string-node
     :pair       none
     :emission   (constantly t/strings)
     :transition #(lookup % open-string-node)}))

(s/def character :- State
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
                 comment-node
                 comma-node
                 escape-node
                 number-node
                 keyword-node)]
    {:node       character-node
     :pair       none
     :emission   (constantly t/characters)
     :transition #(lookup % text-node)}))

(s/def special-character :- State
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
                 comment-node
                 comma-node
                 escape-node
                 number-node
                 keyword-node)]
    {:node       special-character-node
     :pair       none
     :emission   (fn [_ acc]
                   (if (or (special-characters acc) (= (count acc) 1))
                     t/characters
                     t/texts))
     :transition #(lookup % special-character-node)}))

(s/def escape :- State
  (let [lookup (transitions
                 break-node
                 space-node
                 special-character-node)]
    {:node       escape-node
     :pair       none
     :emission   (constantly t/characters)
     :transition #(lookup % character-node)}))

(s/def com-ment :- State
  (let [lookup (transitions break-node)]
    {:node       comment-node
     :pair       none
     :emission   (constantly t/comments)
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
                 escape-node
                 open-string-node
                 comment-node
                 keyword-node
                 comma-node)]
    {:node       closed-string-node
     :pair       none
     :emission   (constantly t/strings)
     :transition #(lookup % text-node)}))

(s/def comma :- State
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
                 escape-node
                 open-string-node
                 comment-node
                 keyword-node
                 comma-node)]
    {:node       comma-node
     :pair       none
     :emission   (constantly t/commas)
     :transition #(lookup % text-node)}))

(s/def node->state :- {Node State}
  {function-node          function
   text-node              text
   open-list-node         open-list
   closed-list-node       close-list
   open-vector-node       open-vector
   closed-vector-node     close-vector
   open-map-node          open-map
   closed-map-node        close-map
   open-string-node       open-string
   closed-string-node     close-string
   break-node             break
   space-node             space
   word-node              word
   keyword-node           key-word
   number-node            number
   escape-node            escape
   character-node         character
   special-character-node special-character
   comment-node           com-ment
   comma-node             comma})

;; 1. We apply the function one last time to "flush" any accumulation that wasn't processed.
;; 2. Because the initial state is a `break`, the highlighter always emits an initial empty :text
(s/defn fold' :- s/Any
  [f      :- (=> s/Any State t/SyntaxElement [Character])
   init   :- s/Any
   stream :- CharStream]
  (loop [result         init
         accumulate     []
         state          break
         state-stack   '()
         [char & chars] stream]
    (let [node          (:node state)
          transition    (:transition state)
          emit          (:emission state)
          pair          (:pair state)
          pushed-node   (-> state-stack (first) (:node))
          expected-pair (-> state-stack (first) (:pair))]
      (if (nil? char)
        (f result state (emit pushed-node accumulate) accumulate)
        (let [node'  (transition char)
              state' (get node->state node')]
          (if (= node node')
            (cond
              (= pair none)
              (recur result (conj accumulate char) state' state-stack chars)

              (= node expected-pair)
              (recur result (conj accumulate char) state' (rest state-stack) chars)

              :else
              (recur result (conj accumulate char) state' (cons state state-stack) chars))

            (cond
              (= pair none)
              (recur (f result state (emit pushed-node accumulate) accumulate) [char] state' state-stack chars)

              (= node expected-pair)
              (recur (f result state (emit pushed-node accumulate) accumulate) [char] state' (rest state-stack) chars)

              :else
              (recur (f result state (emit pushed-node accumulate) accumulate) [char] state' (cons state state-stack) chars))))))))

(s/defn fold :- s/Any
  [f      :- (=> s/Any t/SyntaxElement [Character])
   init   :- s/Any
   stream :- CharStream]
  (fold' (fn [result _ emission acc] (f result emission acc)) init stream))
