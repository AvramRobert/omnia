(ns omnia.repl.syntax-highlighting
  (:require [schema.core :as s]
            [omnia.schema.syntax :as t]
            [omnia.schema.syntax-highlighting :refer :all]))

(s/defn >open-list :- t/SyntaxElement
  [node :- (s/maybe Node), chars :- [Character]]
  t/lists)

(s/defn >closed-list :- t/SyntaxElement
  [node :- (s/maybe Node), chars :- [Character]]
  t/lists)

(s/defn >open-vector :- t/SyntaxElement
  [node :- (s/maybe Node), chars :- [Character]]
  t/vectors)

(s/defn >closed-vector :- t/SyntaxElement
  [node :- (s/maybe Node), chars :- [Character]]
  t/vectors)

(s/defn >open-map :- t/SyntaxElement
  [node :- (s/maybe Node), chars :- [Character]]
  t/maps)

(s/defn >closed-map :- t/SyntaxElement
  [node :- (s/maybe Node), chars :- [Character]]
  t/maps)

(s/defn >open-string :- t/SyntaxElement
  [node :- (s/maybe Node), chars :- [Character]]
  t/strings)

(s/defn >closed-string :- t/SyntaxElement
  [node :- (s/maybe Node), chars :- [Character]]
  t/strings)

(s/defn >break :- t/SyntaxElement
  [node :- (s/maybe Node), chars :- [Character]]
  t/texts)

(s/defn >space :- t/SyntaxElement
  [node :- (s/maybe Node), chars :- [Character]]
  t/texts)

(s/defn >word :- t/SyntaxElement
  [node :- (s/maybe Node), chars :- [Character]]
  (if (words chars) t/words t/texts))

(s/defn >number :- t/SyntaxElement
  [node :- (s/maybe Node), [a, b & _] :- [Character]]
  (let [signed-num? (and (signs a) (digits b))
        num?        (digits a)
        function?   (= node -open-list)]
    (cond
      num?        t/numbers
      signed-num? t/numbers
      function?   t/functions
      :else       t/texts)))

(s/defn >escape-char :- t/SyntaxElement
  [node :- (s/maybe Node), chars :- [Character]]
  t/characters)

(s/defn >comment :- t/SyntaxElement
  [node :- (s/maybe Node), chars :- [Character]]
  t/comments)

(s/defn >keyword :- t/SyntaxElement
  [node :- (s/maybe Node), chars :- [Character]]
  t/keywords)

(s/defn >comma :- t/SyntaxElement
  [node :- (s/maybe Node), chars :- [Character]]
  t/commas)

(s/defn >special-char :- t/SyntaxElement
  [node :- (s/maybe Node), chars :- [Character]]
  (if (or (special-characters chars)
          (= (count chars) 1))
    t/characters
    t/texts))

(s/defn >function :- t/SyntaxElement
  [node :- (s/maybe Node), chars :- [Character]]
  t/functions)

(s/defn >text :- t/SyntaxElement
  [node :- (s/maybe Node), chars :- [Character]]
  t/texts)

(s/defn >char :- t/SyntaxElement
  [node :- (s/maybe Node), chars :- [Character]]
  t/characters)

(s/def transitions :- {Node [Node]}
  {-open-list     [-open-list -closed-list -open-vector -closed-vector
                   -open-map -closed-map -open-string -break -space
                   -number -escape-char -comment -keyword -comma]

   -closed-list   [-open-list -closed-list -open-vector -closed-vector
                   -open-map -closed-map -open-string -break -space
                   -word -number -escape-char -comment -keyword -comma]

   -open-vector   [-open-list -closed-list -open-vector -closed-vector
                   -open-map -closed-map -open-string -break -space
                   -word -number -escape-char -comment -keyword -comma]

   -closed-vector [-open-list -closed-list -open-vector -closed-vector
                   -open-map -closed-map -open-string -break -space
                   -word -number -escape-char -comment -keyword -comma]

   -open-map      [-open-list -closed-list -open-vector -closed-vector
                   -open-map -closed-map -open-string -break -space
                   -word -number -escape-char -comment -keyword -comma]

   -closed-map    [-open-list -closed-list -open-vector -closed-vector
                   -open-map -closed-map -open-string -break -space
                   -word -number -escape-char -comment -keyword -comma]

   -open-string   [-closed-string]

   -closed-string [-open-list -closed-list -open-vector -closed-vector
                   -open-map -closed-map -open-string -break -space
                   -word -number -escape-char -comment -keyword -comma]

   -break         [-open-list -closed-list -open-vector -closed-vector
                   -open-map -closed-map -open-string -break -space
                   -word -number -escape-char -comment -keyword -comma]

   -space         [-open-list -closed-list -open-vector -closed-vector
                   -open-map -closed-map -open-string -break -space
                   -word -number -escape-char -comment -keyword -comma]

   -word          [-open-list -closed-list -open-vector -closed-vector
                   -open-map -closed-map -open-string -break -space
                   -comment -comma]

   -number        [-open-list -closed-list -open-vector -closed-vector
                   -open-map -closed-map -open-string -break -space
                   -comment -comma]

   -escape-char   [-break -space -special-char]

   -comment       [-break]

   -keyword       [-open-list -closed-list -open-vector -closed-vector
                   -open-map -closed-map -open-string -break -space
                   -escape-char -comment -comma]

   -comma         [-open-list -closed-list -open-vector -closed-vector
                   -open-map -closed-map -open-string -break -space
                   -word -number -escape-char -comment -keyword -comma]

   -special-char  [-open-list -closed-list -open-vector -closed-vector
                   -open-map -closed-map -open-string -break -space
                   -comment -comma -escape-char -number -keyword]

   -function      [-open-list -closed-list -open-vector -closed-vector
                   -open-map -closed-map -open-string -break -space
                   -comment -escape-char -comma]

   -text          [-open-list -closed-list -open-vector -closed-vector
                   -open-map -closed-map -open-string -break -space
                   -escape-char -comment -comma]

   -char          [-open-list -closed-list -open-vector -closed-vector
                   -open-map -closed-map -open-string -break -space
                   -comment -comma -escape-char -number -keyword]})

(s/def defaults :- {Node Node}
  {-open-list     -function
   -closed-list   -text
   -open-vector   -text
   -closed-vector -text
   -open-map      -text
   -closed-map    -text
   -open-string   -open-string
   -closed-string -text
   -break         -text
   -space         -text
   -word          -word
   -number        -number
   -escape-char   -char
   -comment       -comment
   -keyword       -keyword
   -comma         -text
   -special-char  -special-char
   -function      -function
   -text          -text
   -char          -text})

(s/def emissions :- {Node EmissionFn}
  {-open-list     >open-list
   -closed-list   >closed-list
   -open-vector   >open-vector
   -closed-vector >closed-vector
   -open-map      >open-map
   -closed-map    >closed-map
   -open-string   >open-string
   -closed-string >closed-string
   -break         >break
   -space         >space
   -word          >word
   -number        >number
   -escape-char   >escape-char
   -comment       >comment
   -keyword       >keyword
   -comma         >comma
   -special-char  >special-char
   -function      >function
   -text          >text
   -char          >char})

(s/def pairs :- {Node Node}
  {-open-list     -closed-list
   -closed-list   -null
   -open-vector   -closed-vector
   -closed-vector -null
   -open-map      -closed-map
   -closed-map    -null
   -open-string   -closed-string
   -closed-string -null
   -break         -null
   -space         -null
   -word          -null
   -number        -null
   -escape-char   -null
   -comment       -null
   -keyword       -null
   -comma         -null
   -special-char  -null
   -function      -null
   -text          -null
   -char          -null})

(s/def triggers :- {Node #{Character}}
  {-open-list     #{\(},
   -closed-list   #{\)},
   -open-vector   #{\[},
   -closed-vector #{\]},
   -open-map      #{\{},
   -closed-map    #{\}},
   -open-string   #{\"}
   -closed-string #{\"}
   -break         #{\newline},
   -space         #{\space},
   -word          #{\f \n \t},
   -number        #{\+ \- \0 \1 \2 \3 \4 \5 \6 \7 \8 \9},
   -escape-char   #{\\},
   -keyword       #{\:},
   -comma         #{\,}
   -special-char  #{\s \n},
   -comment       #{\;}})

(s/def trigger-transitions :- {Node {Character Node}}
  (letfn [(define [node]
            (->> node (get triggers) (map #(vector % node))))]
    (reduce-kv
      (fn [table main-node transition-nodes]
        (->> transition-nodes
             (mapcat define)
             (into {})
             (assoc table main-node))) {} transitions)))

;; 1. We apply the function one last time to "flush" any accumulation that wasn't processed.
;; 2. Because the initial node is a `-break`, the highlighter always emits an initial empty :text
(s/defn fold' :- s/Any
  [f      :- (s/=> s/Any Node t/SyntaxElement [Character])
   init   :- s/Any
   stream :- CharStream]
  (loop [result         init
         accumulate     []
         node           -break
         stack          '()
         [char & chars] stream]
    (let [transition    (get trigger-transitions node)
          emit          (get emissions node)
          pair          (get pairs node)
          default-node  (get defaults node)
          pushed-node   (first stack)
          expected-node (get pairs pushed-node)
          node'         (transition char default-node)]
      (if (nil? char)
        (f result node (emit pushed-node accumulate) accumulate)
        (if (= node node')
          (cond
            (= pair -null)
            (recur result (conj accumulate char) node' stack chars)

            (= node expected-node)
            (recur result (conj accumulate char) node' (rest stack) chars)

            :else
            (recur result (conj accumulate char) node' (cons node stack) chars))

          (cond
            (= pair -null)
            (recur (f result node (emit pushed-node accumulate) accumulate) [char] node' stack chars)

            (= node expected-node)
            (recur (f result node (emit pushed-node accumulate) accumulate) [char] node' (rest stack) chars)

            :else
            (recur (f result node (emit pushed-node accumulate) accumulate) [char] node' (cons node stack) chars)))))))

(s/defn fold :- s/Any
  [f      :- (s/=> s/Any t/SyntaxElement [Character])
   init   :- s/Any
   stream :- CharStream]
  (fold' (fn [result _ emission acc] (f result emission acc)) init stream))
