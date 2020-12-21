(ns omnia.highlight)

(defrecord State [id fallback guard nodes valid?])

(declare transition transitions changed?)

(def ^:const -list :list)
(def ^:const -vector :vector)
(def ^:const -map :map)
(def ^:const -char :character)
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

(defn- applies? [state]
  (let [guard (:guard state)
        immuted? #(->> (transition state %) (changed? state) (not))]
    (fn [chars]
      (and (guard (first chars))
           (every? immuted? chars)))))

(defn state [{:keys [id
                     fallback
                     guard
                     nodes
                     valid?]
              :or {valid? (comp not empty?)
                   nodes []}}]
  (assert (not (nil? id)) "A state must always have an identifier")
  (assert (not (nil? guard)) "A state must always have a guard")
  (let [nguard (if (= diff-nodes guard) (invert nodes) guard)]
    (State. id fallback nguard nodes valid?)))

(def ->function
  (state {:id -function
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
  (state {:id -text
          :guard diff-nodes
          :nodes [-break
                  -space
                  -list
                  -vector
                  -map
                  -char
                  -string
                  -comment]}))

(def ->break
  (state {:id -break
          :fallback ->text
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
  (state {:id -space
          :fallback ->text
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
  (state {:id -list
          :fallback ->function
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
  (state {:id -list
          :fallback ->text
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
  (state {:id -vector
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
  (state {:id -vector
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
  (state {:id -map
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
  (state {:id -map
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
  (state {:id -string
          :guard #(= \" %)
          :nodes [-string*]}))

(def ->close-string
  (state {:id -string*
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
  (state {:id -comment
          :guard #(= \; %)
          :nodes [-break]}))

(def ->char
  (state {:id -char
          :guard #(= \\ %)
          :nodes [-break
                  -space]}))

(def ->number
  (state {:id -number
          :guard #(contains? numbers %)
          :nodes [-break
                  -space
                  -list
                  -vector
                  -map
                  -string
                  -comment]}))

(def ->signed-number
  (state {:id -number
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
  (state {:id -keyword
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
  (state {:id -word
          :guard #(some (fn [[l & _]] (= l %)) words)
          :nodes [-break
                  -space
                  -list
                  -vector
                  -map
                  -string
                  -char
                  -comment]
          :valid? #(some (fn [w] (= % w)) words)}))

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

(defn transition [state c]
  (letfn [(valid-node [node]
            (->> node (transitions) (some #(when ((:guard %) c) %))))]
    (or (some valid-node (:nodes state)) state)))

(defn changed? [old-state new-state]
  (not= (:id new-state) (:id old-state)))

(defn fall-back [new-state old-state]
  (or (:fallback new-state) old-state))

(defn emit [{:keys [state store fallback result]} f]
  (if ((:valid? state) store)
    (f result [store state])
    (f result [store fallback])))

;; If -text sits higher in the node list than -word, words will be processed as text
(defn foldl [f b stream]
  (let [init {:store    []
              :state    ->break
              :fallback ->text
              :result   b}]
    (-> (fn [{:keys [store state fallback result] :as data} c]
          (let [state'    (transition state c)
                fallback' (fall-back state' fallback)
                changed?  (changed? state state')
                store'    (if changed? [c] (conj store c))
                result'   (if changed? (emit data f) result)]
            {:state    state'
             :fallback fallback'
             :store    store'
             :result   result'}))
        (reduce init stream)
        (emit f))))