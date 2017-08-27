(ns omnia.highlight-beta
  (require [clojure.core.match :as m]
           [clojure.string :as s]))

(defrecord Transiton [state guard nodes valid?])

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

(def ^:const empty-vec [])

(def ^:const numbers #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})

(def ^:const words #{[\n \i \l]
                     [\t \r \u \e]
                     [\f \a \l \s \e]})

(defn- alphabetic? [c] (Character/isAlphabetic (int c)))

(defmacro deftrans [name {:keys [state
                                 guard
                                 nodes
                                 valid?]
                          :or {valid? (comp not empty?)}}]
  `(def ~name (Transiton. ~state ~guard ~nodes ~valid?)))


(deftrans ->break {:state -break
                   :guard #(= \newline %)
                   :nodes [-space
                           -word
                           -text
                           -function
                           -list
                           -vector
                           -map
                           -number
                           -char
                           -string
                           -comment
                           -keyword]})

(deftrans ->space {:state -space
                   :guard #(= \space %)
                   :nodes [-break
                           -word
                           -text
                           -function
                           -list
                           -vector
                           -map
                           -number
                           -char
                           -string
                           -comment
                           -keyword]})

(deftrans ->open-list {:state -list
                       :guard #(= \( %)
                       :nodes [-break
                               -space
                               -function
                               -list
                               -vector
                               -map
                               -number
                               -char
                               -string
                               -comment
                               -keyword]})

(deftrans ->close-list {:state -list
                        :guard #(= \) %)
                        :nodes [-break
                                -space
                                -word
                                -text
                                -list
                                -vector
                                -map
                                -number
                                -char
                                -string
                                -comment
                                -keyword]})

(deftrans ->open-vector {:state -vector
                         :guard #(= \[ %)
                         :nodes [-break
                                 -space
                                 -word
                                 -text
                                 -list
                                 -vector
                                 -map
                                 -number
                                 -char
                                 -string
                                 -comment
                                 -keyword]})

(deftrans ->close-vector {:state -vector
                          :guard #(= \] %)
                          :nodes [-break
                                  -space
                                  -word
                                  -text
                                  -list
                                  -vector
                                  -map
                                  -number
                                  -char
                                  -string
                                  -comment
                                  -keyword]})

(deftrans ->open-map {:state -map
                      :guard #(= \{ %)
                      :nodes [-break
                              -space
                              -word
                              -text
                              -list
                              -vector
                              -map
                              -number
                              -char
                              -string
                              -comment
                              -keyword]})

(deftrans ->close-map {:state -map
                       :guard #(= \} %)
                       :nodes [-break
                               -space
                               -word
                               -text
                               -list
                               -vector
                               -map
                               -number
                               -char
                               -string
                               -comment
                               -keyword]})

(deftrans ->function {:state -function
                      :guard alphabetic?
                      :nodes [-break
                              -space
                              -list
                              -vector
                              -map
                              -comment
                              -char
                              -number
                              -string]})

(deftrans ->text {:state -text
                  :guard alphabetic?
                  :nodes [-break
                          -space
                          -list
                          -vector
                          -map
                          -char
                          -string
                          -comment]})

(deftrans ->open-string {:state -string
                         :guard #(= \" %)
                         :nodes [-string*]})

(deftrans ->close-string {:state -string*
                          :guard #(= \" %)
                          :nodes [-break
                                  -space
                                  -word
                                  -text
                                  -list
                                  -vector
                                  -map
                                  -number
                                  -char
                                  -string
                                  -comment
                                  -keyword]})

(deftrans ->comment {:state -comment
                     :guard #(= \; %)
                     :nodes [-break]})

(deftrans ->char {:state -char
                  :guard #(= \\ %)
                  :nodes [-break
                          -space]})

(deftrans ->number {:state -number
                    :guard #(contains? numbers %)
                    :nodes [-break
                            -space
                            -list
                            -vector
                            -map
                            -string
                            -comment]})

(deftrans ->keyword {:state -keyword
                     :guard #(= \: %)
                     :nodes [-break
                             -space]})

(deftrans ->word
          {:state -word
           :guard #(some (fn [[l & _]] (= l %)) words)
           :nodes [-break
                   -space
                   -list
                   -vector
                   -map
                   -string
                   -char
                   -comment]
           :valid? #(some (fn [w] (= % w)) words)})

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
   -number   [->number]
   -char     [->char]
   -keyword  [->keyword]
   -break    [->break]
   -space    [->space]})

(defn- propagate [transiton c]
  (or (some->> (:nodes transiton)
               (map transitions)
               (flatten)
               (some #(when ((:guard %) c) %)))
      transiton))

(defn changed? [this that]
  (not= (:state this) (:state that)))

(defn emit [transiton pushed f]
  (if ((:valid? transiton) pushed)
    (f pushed (:state transiton))
    (f pushed (:state ->text))))

;; If -text sits higher in the node list than -word, words will be processed as text
(defn process [stream f]
  (loop [rem stream
         transiton ->break
         store empty-vec
         ems empty-vec]
    (m/match [store rem]
             [[] []] ems
             [_ []] (->> (emit transiton store f)
                         (conj ems)
                         (recur rem transiton empty-vec))
             [_ [a & tail]]
             (let [t (propagate transiton a)]
               (if (changed? transiton t)
                  (->> (emit transiton store f)
                       (conj ems)
                       (recur tail t [a]) )
                 (recur tail t (conj store a) ems))))))