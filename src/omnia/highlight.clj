(ns omnia.highlight
  (:require [clojure.core.match :as m]))

;; FIXME: Rename this
(defrecord Transiton [state fallback guard nodes valid?])

(declare transition transitions changed?)

(def ^:const -list :list)
(def ^:const -vector :vector)
(def ^:const -map :map)
(def ^:const -char :char)
(def ^:const -number :number)
(def ^:const -string :string)
(def ^:const -string* :string*)
(def ^:const -keyword :keyword)
(def ^:const -function :function)
(def ^:const -comment :comment)
(def ^:const -word :word)
(def ^:const -text :text)
(def ^:const -break :break)
(def ^:const -space :space)
(def ^:const -select :selection)
(def ^:const -back :background)

(def ^:const diff-nodes :diff-nodes)

(def ^:const inferred :inferred)

(def ^:const empty-vec [])

(def ^:const numbers #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})

(def ^:const words #{[\n \i \l]
                     [\t \r \u \e]
                     [\f \a \l \s \e]})

(defn- invert [nodes]
  (let [ts (delay (->> nodes
                       (mapcat transitions)
                       (mapv :guard)))]
    #(reduce
       (fn [bool p] (and bool (not (p %)))) true @ts)))

(defn- applies? [transiton]
  (let [guard (:guard transiton)
        immuted? #(->> (transition transiton %) (changed? transiton) (not))]
    (fn [chars]
      (and (guard (first chars))
           (every? immuted? chars)))))

(defn transiton [{:keys [state
                         fallback
                         guard
                         nodes
                         valid?]
                  :or   {valid? (comp not empty?)
                         fallback inferred
                         nodes  []}}]
  (assert (not (nil? state)) "A transiton must always have a state")
  (assert (not (nil? guard)) "A transiton must always have a guard")
  (let [nguard (if (= diff-nodes guard) (invert nodes) guard)]
    (Transiton. state fallback nguard nodes valid?)))

(def ->break
  (transiton {:state -break
              :fallback -text
              :guard #(= \newline %)
              :nodes [-space
                      -word
                      -list
                      -vector
                      -map
                      -number
                      -char
                      -string
                      -comment
                      -keyword
                      -text]}))

(def ->space
  (transiton {:state -space
              :fallback -text
              :guard #(= \space %)
              :nodes [-break
                      -word
                      -list
                      -vector
                      -map
                      -number
                      -char
                      -string
                      -comment
                      -keyword
                      -text]}))

(def ->open-list
  (transiton {:state -list
              :fallback -function
              :guard #(= \( %)
              :nodes [-break
                      -space
                      -list
                      -vector
                      -map
                      -number
                      -char
                      -string
                      -comment
                      -keyword
                      -function]}))

(def ->close-list
  (transiton {:state -list
              :fallback -text
              :guard #(= \) %)
              :nodes [-break
                      -space
                      -word
                      -list
                      -vector
                      -map
                      -number
                      -char
                      -string
                      -comment
                      -keyword
                      -text]}))

(def ->open-vector
  (transiton {:state -vector
              :guard #(= \[ %)
              :nodes [-break
                      -space
                      -word
                      -list
                      -vector
                      -map
                      -number
                      -char
                      -string
                      -comment
                      -keyword
                      -text]}))

(def ->close-vector
  (transiton {:state -vector
              :guard #(= \] %)
              :nodes [-break
                      -space
                      -word
                      -list
                      -vector
                      -map
                      -number
                      -char
                      -string
                      -comment
                      -keyword
                      -text]}))

(def ->open-map
  (transiton {:state -map
              :guard #(= \{ %)
              :nodes [-break
                      -space
                      -word
                      -list
                      -vector
                      -map
                      -number
                      -char
                      -string
                      -comment
                      -keyword
                      -text]}))

(def ->close-map
  (transiton {:state -map
              :guard #(= \} %)
              :nodes [-break
                      -space
                      -word
                      -list
                      -vector
                      -map
                      -number
                      -char
                      -string
                      -comment
                      -keyword
                      -text]}))

(def ->open-string
  (transiton {:state -string
              :guard #(= \" %)
              :nodes [-string*]}))

(def ->close-string
  (transiton {:state -string*
              :guard #(= \" %)
              :nodes [-break
                      -space
                      -word
                      -list
                      -vector
                      -map
                      -number
                      -char
                      -string
                      -comment
                      -keyword
                      -text]}))

(def ->comment
  (transiton {:state -comment
              :guard #(= \; %)
              :nodes [-break]}))

(def ->char
  (transiton {:state -char
              :guard #(= \\ %)
              :nodes [-break
                      -space]}))

(def ->number
  (transiton {:state -number
              :guard #(contains? numbers %)
              :nodes [-break
                      -space
                      -list
                      -vector
                      -map
                      -string
                      -comment]}))

(def ->signed-number
  (transiton {:state -number
              :guard #(or (= \+ %) (= \- %))
              :nodes [-break
                      -space
                      -list
                      -vector
                      -map
                      -string
                      -comment]
              :valid? (comp (applies? ->number) rest)}))

(def ->keyword
  (transiton {:state -keyword
              :guard #(= \: %)
              :nodes [-list
                      -vector
                      -map
                      -string
                      -char
                      -comment
                      -break
                      -space]}))

(def ->word
  (transiton {:state  -word
              :guard  #(some (fn [[l & _]] (= l %)) words)
              :nodes  [-break
                       -space
                       -list
                       -vector
                       -map
                       -string
                       -char
                       -comment]
              :valid? #(some (fn [w] (= % w)) words)}))

(def ->function
  (transiton {:state -function
              :fallback -function
              :guard diff-nodes
              :nodes [-break
                      -space
                      -list
                      -vector
                      -map
                      -comment
                      -char
                      -string]}))

(def ->text
  (transiton {:state -text
              :fallback -text
              :guard diff-nodes
              :nodes [-break
                      -space
                      -list
                      -vector
                      -map
                      -char
                      -string
                      -comment]}))

(def transitions
  {-list     [->open-list ->close-list]
   -vector   [->open-vector ->close-vector]
   -map      [->open-map ->close-map]
   -function [->function]
   -text     [->text]
   -string   [->open-string]
   -string*  [->close-string]
   -comment  [->comment]
   -word     [->word]
   -number   [->number ->signed-number]
   -char     [->char]
   -keyword  [->keyword]
   -break    [->break]
   -space    [->space]})

(defn transition [transiton c]
  (loop [[state & states] (:nodes transiton)]
    (if state
      (or (->> state (transitions) (some #(when ((:guard %) c) %)))
          (recur states))
      transiton)))

(defn changed? [otransiton ntransiton]
  (not= (:state ntransiton) (:state otransiton)))

(defn fall-back [new-transiton old-fallback]
  (let [nfallback (:fallback new-transiton)]
    (if (= inferred nfallback) old-fallback nfallback)))

(defn emit! [{:keys [transiton store fallback]} f]
  (if ((:valid? transiton) store)
    (f store (:state transiton))
    (f store fallback)))

;; If -text sits higher in the node list than -word, words will be processed as text
(defn consume! [f init stream]
  (-> (fn [{:keys [store transiton fallback] :as data} c]
        (let [new-t    (transition transiton c)
              new-f    (fall-back new-t fallback)
              changed? (changed? transiton new-t)
              new-s    (if changed? [c] (conj store c))
              _        (when changed? (emit! data f))]
          {:store     new-s
           :fallback  new-f
           :transiton new-t}))
      (reduce init stream)
      (emit! f)))

(defn process! [stream f]
  (consume! f {:store     empty-vec
               :transiton ->break
               :fallback  -text} stream))

(defn process-from! [stream t0 f]
  (consume! f {:store empty-vec
               :transiton t0
               :fallback -text} stream))

(defn process-as [stream f]
  (let [emissions (atom [])]
    (process! stream
              (fn [emission state]
                (swap! emissions #(conj % (f emission state)))))))

(defn state-at [stream x]
  (if (>= (count stream) x)
    ->text
    (loop [[c & chrs] stream
           transiton ->break
           cnt -1]
      (let [new-t    (transition transiton c)
            changed? (changed? new-t transiton)]
        (cond
          (and changed? (>= cnt x)) transiton
          (nil? c) ->text
          :else (recur chrs new-t (inc cnt)))))))