(ns omnia.highlighting
  (:require [schema.core :as s]
            [omnia.more :refer [=>]]
            [clojure.set :refer [intersection]]))

(def ^:const -open-list :open-list)
(def ^:const -close-list :close-list)
(def ^:const -open-vector :open-vector)
(def ^:const -close-vector :close-vector)
(def ^:const -open-map :open-map)
(def ^:const -close-map :close-map)
(def ^:const -char :character)
(def ^:const -number :number)
(def ^:const -signed-number :signed-number)
(def ^:const -open-string :open-string)
(def ^:const -close-string :close-string)
(def ^:const -keyword :keyword)
(def ^:const -function :function)
(def ^:const -comment :comment)
(def ^:const -word :word)
(def ^:const -text :text)
(def ^:const -break :break)
(def ^:const -space :space)

(def ^:const -select :selection)
(def ^:const -back :background)

(def Node (s/enum -open-list
                  -close-list
                  -open-vector
                  -close-vector
                  -open-map
                  -close-map
                  -open-string
                  -close-string
                  -char
                  -number
                  -signed-number
                  -keyword
                  -function
                  -comment
                  -word
                  -text
                  -break
                  -space))

(def State
  {:node       Node
   :emission   (=> [Character] s/Keyword)
   :transition (=> Character (s/maybe Node))})

(def triggers
  {-space         #{\space},
   -close-string  #{\"},
   -open-string   #{\"}
   -number        #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9},
   -signed-number #{\+ \- \0 \1 \2 \3 \4 \5 \6 \7 \8 \9},
   -close-map     #{\}},
   -close-list    #{\)},
   -close-vector  #{\]},
   -break         #{\newline},
   -open-vector   #{\[},
   -keyword       #{\:},
   -char          #{\\},
   -word          #{\f \n \t},
   -comment       #{\;},
   -open-map      #{\{},
   -open-list     #{\(}})

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
                 -open-list
                 -close-list
                 -open-vector
                 -close-vector
                 -open-map
                 -close-map
                 -open-string
                 -break
                 -space
                 -comment
                 -char)]
    {:node       -function
     :emission   (constantly :function)
     :transition #(lookup % -function)}))

(s/def open-list :- State
  (let [lookup (transitions
                 -break
                 -space
                 -open-list
                 -close-list
                 -open-vector
                 -close-vector
                 -open-map
                 -close-map
                 -char
                 -open-string
                 -comment
                 -keyword)]
    {:node       -open-list
     :emission   (constantly :list)
     :transition #(lookup % -function)}))

(s/def close-list :- State
  (let [lookup (transitions
                 -break
                 -space
                 -word
                 -open-list
                 -close-list
                 -open-vector
                 -close-vector
                 -open-map
                 -close-map
                 -number
                 -signed-number
                 -char
                 -open-string
                 -comment
                 -keyword)]
    {:node       -close-list
     :emission   (constantly :list)
     :transition #(lookup % -text)}))

(s/def text :- State
  (let [lookup (transitions
                 -break
                 -space
                 -open-list
                 -close-list
                 -open-vector
                 -close-vector
                 -open-map
                 -close-map
                 -char
                 -open-string
                 -comment)]
    {:node       -text
     :emission   (constantly :text)
     :transition #(lookup % -text)}))

(s/def break :- State
  (let [lookup (transitions
                 -space
                 -word
                 -open-list
                 -close-list
                 -open-vector
                 -close-vector
                 -open-map
                 -close-map
                 -number
                 -signed-number
                 -char
                 -open-string
                 -comment
                 -keyword)]
    {:node       -break
     :emission   (constantly :text)
     :transition #(lookup % -text)}))

(s/def space :- State
  (let [lookup (transitions
                 -break
                 -word
                 -open-list
                 -close-list
                 -open-vector
                 -close-vector
                 -open-map
                 -close-map
                 -number
                 -signed-number
                 -char
                 -open-string
                 -comment
                 -keyword)]
    {:node       -space
     :emission   (constantly :text)
     :transition #(lookup % -text)}))

(s/def word :- State
  (let [lookup (transitions
                 -break
                 -space
                 -open-list
                 -close-list
                 -open-vector
                 -close-vector
                 -open-map
                 -close-map
                 -open-string
                 -number
                 -signed-number
                 -char
                 -comment)]
    {:node       -word
     :emission   #(if (words %) :word :text)
     :transition #(lookup % -word)}))

(s/def open-vector :- State
  (let [lookup (transitions
                 -break
                 -space
                 -word
                 -open-list
                 -close-list
                 -open-vector
                 -close-vector
                 -open-map
                 -close-map
                 -number
                 -signed-number
                 -char
                 -open-string
                 -comment
                 -keyword)]
    {:node       -open-vector
     :emission   (constantly :vector)
     :transition #(lookup % -text)}))

(s/def close-vector :- State
  (let [lookup (transitions
                 -break
                 -space
                 -word
                 -open-list
                 -close-list
                 -open-vector
                 -close-vector
                 -open-map
                 -close-map
                 -open-string
                 -number
                 -signed-number
                 -char
                 -comment
                 -keyword)]
    {:node       -close-vector
     :emission   (constantly :vector)
     :transition #(lookup % -text)}))

(s/def open-map :- State
  (let [lookup (transitions
                 -break
                 -space
                 -word
                 -open-list
                 -close-list
                 -open-vector
                 -close-vector
                 -open-map
                 -close-map
                 -open-string
                 -number
                 -signed-number
                 -char
                 -comment
                 -keyword)]
    {:node       -open-map
     :emission   (constantly :map)
     :transition #(lookup % -text)}))

(s/def close-map :- State
  (let [lookup (transitions
                 -break
                 -space
                 -word
                 -open-list
                 -close-list
                 -open-vector
                 -close-vector
                 -open-map
                 -close-map
                 -open-string
                 -number
                 -signed-number
                 -char
                 -comment
                 -keyword)]
    {:node       -close-map
     :emission   (constantly :map)
     :transition #(lookup % -text)}))

(s/def key-word :- State
  (let [lookup (transitions
                 -break
                 -space
                 -open-list
                 -close-list
                 -open-vector
                 -close-vector
                 -open-map
                 -close-map
                 -open-string
                 -char
                 -comment)]
    {:node       -keyword
     :emission   (constantly :keyword)
     :transition #(lookup % -keyword)}))

(s/def number :- State
  (let [lookup (transitions
                 -break
                 -space
                 -open-list
                 -close-list
                 -open-vector
                 -close-vector
                 -open-map
                 -close-map
                 -open-string
                 -comment
                 -number)]
    {:node       -number
     :emission   (constantly :number)
     :transition #(lookup % -text)}))

;; I don't think I need "signed number" as a state
;; can just map these into the number state
(s/def signed-number :- State
  (let [lookup (transitions
                 -break
                 -space
                 -open-list
                 -close-list
                 -open-vector
                 -close-vector
                 -open-map
                 -close-map
                 -open-string
                 -comment
                 -signed-number)]
    {:node       -signed-number
     :emission   #(if (> (count %) 1) :number :text)
     :transition #(lookup % -text)}))

(s/def open-string :- State
  (let [lookup (transitions -close-string)]
    {:node       -open-string
     :emission   (constantly :string)
     :transition #(lookup % -open-string)}))

(s/def close-string :- State
  (let [lookup (transitions
                 -break
                 -space
                 -word
                 -open-list
                 -close-list
                 -open-vector
                 -close-vector
                 -open-map
                 -close-map
                 -number
                 -char
                 -open-string
                 -comment
                 -keyword)]
    {:node       -close-list
     :emission   (constantly :string)
     :transition #(lookup % -text)}))

(s/def node->state :- {Node State}
  {-function      function
   -text          text
   -open-list     open-list
   -close-list    close-list
   -open-vector   open-vector
   -close-vector  close-vector
   -open-map      open-map
   -close-map     close-map
   -break         break
   -space         space
   -word          word
   -keyword       key-word
   -number        number
   -signed-number signed-number
   -open-string   open-string
   -close-string  close-string})

(defn consume-with [f]
  (fn [[intermediate accumulate state] char]
    (let [node       (:node state)
          transition (:transition state)
          emission   (:emission state)
          node'      (transition char)
          state'     (node->state node')]
      (if (= node node')
        [intermediate (conj accumulate char) state']
        [(f intermediate (emission accumulate) accumulate) [char] state']))))

;; We apply the function one last time to "flush" any accumulation that wasn't processed
(defn fold [f init chars]
  (let [consumption        (consume-with f)
        [output acc state] (reduce consumption [init [] break] chars)
        emission           ((:emission state) acc)]
    (f output emission acc)))

(defn verify [input]
  (fold (fn [_ x acc] (println x " :: " acc)) nil (vec input)))