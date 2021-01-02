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

(def Node (s/enum -open-list -close-list
                  -open-vector -close-vector
                  -open-map -close-map
                  -open-string -close-string
                  -char
                  -number
                  -keyword
                  -function
                  -comment
                  -word
                  -text
                  -break
                  -space))
;
(def Transition (=> Character Node))
(def Validation (=> [Character] s/Bool))

(def State
  {:node       Node
   :fallback   Node
   :validation Validation
   :transition Transition})

(def triggers
  {\space   #{-space},
   \(       #{-open-list},
   \)       #{-close-list},
   \[       #{-open-vector},
   \]       #{-close-vector},
   \{       #{-open-map},
   \}       #{-close-map}
   \newline #{-break},
   \0       #{-number},
   \1       #{-number},
   \2       #{-number},
   \3       #{-number},
   \4       #{-number},
   \5       #{-number},
   \6       #{-number},
   \7       #{-number},
   \8       #{-number},
   \9       #{-number},
   \:       #{-keyword},
   \\       #{-comment}
   \"       #{-open-string -close-string}
   \n       #{-word}
   \t       #{-word}
   \f       #{-word}})

(def emissions
  {-open-list    :list
   -close-list   :list
   -open-vector  :vector
   -close-vector :vector
   -function     :function})

(s/defn transition-to
  [nodes :- #{Node}
   fallback :- Node]
  (fn [char]
    (or (some->> char (get triggers) (intersection nodes) (first))
        fallback)))

(s/def function :- State
  (let [fallback -function
        nodes    #{-open-list
                   -close-list
                   -open-vector
                   -close-vector
                   -open-map
                   -close-map
                   -break
                   -space
                   -comment
                   -char
                   -open-string}]
    {:node       -function
     :fallback   fallback
     :validation (constantly true)
     :transition (transition-to nodes fallback)}))

(s/def open-list :- State
  (let [fallback -function
        nodes    #{-break
                   -space
                   -open-list
                   -close-list
                   -open-vector
                   -close-vector
                   -open-map
                   -close-map
                   -number
                   -char
                   -open-string
                   -close-string
                   -comment
                   -keyword}]
    {:node       -open-list
     :fallback   fallback
     :validation (constantly true)
     :transition (transition-to nodes fallback)}))

(s/def close-list :- State
  (let [fallback -text
        nodes    #{-break
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
                   -close-string
                   -comment
                   -keyword}]
    {:node       -close-list
     :fallback   fallback
     :validation (constantly true)
     :transition (transition-to nodes fallback)}))

(s/def text :- State
  (let [fallback -text
        nodes    #{-break
                   -space
                   -open-list
                   -close-list
                   -open-vector
                   -close-vector
                   -open-map
                   -close-map
                   -char
                   -open-string
                   -close-string
                   -comment}]
    {:node       -text
     :fallback   fallback
     :validation (constantly true)
     :transition (transition-to nodes fallback)}))

(s/def break :- State
  (let [fallback -text
        nodes    #{-space
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
                   -close-string
                   -comment
                   -keyword}]
    {:node       -break
     :fallback   fallback
     :validation (constantly true)
     :transition (transition-to nodes fallback)}))

(s/def space :- State
  (let [fallback -text
        nodes    #{-break
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
                   -keyword}]
    {:node       -space
     :fallback   fallback
     :validation (constantly true)
     :transition (transition-to nodes fallback)}))

(s/defn emission :- s/Keyword
  [node :- Node]
  (get emissions node :text))

(s/def states :- {Node State}
  {-function   function
   -text       text
   -open-list  open-list
   -close-list close-list
   -break      break
   -space      space})

(defn consume-with [f]
  (fn [[intermediate accumulate state] char]
    (let [node       (:node state)
          transition (:transition state)
          valid?     (:validation state)
          fallback   (:fallback state)
          node'      (transition char)
          state'     (states node')]
      (cond
        (= node node')      [intermediate (conj accumulate char) state']
        (valid? accumulate) [(f intermediate [(emission node) accumulate]) [char] state']
        :else               [(f intermediate [(emission fallback) accumulate]) [char] state']))))

(defn fold [f init chars]
  (-> (consume-with f)
      (reduce [init [] break] (conj chars \space))
      (first)))