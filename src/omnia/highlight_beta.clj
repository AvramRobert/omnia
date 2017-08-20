(ns omnia.highlight-beta
  (require [clojure.core.match :as m]
           [clojure.string :as s]))

(def ^:const -list :list)
(def ^:const -vector :vector)
(def ^:const -map :map)
(def ^:const -char :char)
(def ^:const -number :number)
(def ^:const -string :string)
(def ^:const -keyword :keyword)
(def ^:const -function :function)
(def ^:const -comment :comment)
(def ^:const -word :word)
(def ^:const -text :txt)

(def ^:const numbers #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})

(def ^:const words #{[\n \i \l]
                     [\t \r \u \e]
                     [\f \a \l \s \e]})

(def ^:const standard [-list
                       -vector
                       -map
                       -text
                       -number
                       -word
                       -char
                       -string
                       -keyword
                       -comment])

(def ->open-list {:pred  #(= \( %)
                  :state -list
                  :trans (vec (concat standard [-function]))})

(def ->close-list {:pred  #(= \) %)
                   :state -list
                   :trans standard})

(def ->open-vector {:pred  #(= \[ %)
                    :state -vector
                    :trans standard})

(def ->close-vector {:pred  #(= \] %)
                     :state -vector
                     :trans standard})

(def ->open-map {:pred  #(= \{ %)
                 :state -map
                 :trans standard})

(def ->close-map {:pred  #(= \} %)
                  :state -map
                  :trans standard})

(def ->function {:pred  #(Character/isAlphabetic (int %))
                 :state -function
                 :trans [-text
                         -list
                         -vector
                         -map
                         -comment
                         -char]})

(def ->text {:pred  #(or (= \space %) (= \newline %))
             :state -text
             :trans standard})

(def ->open-string {:pred  #(= \" %)
                    :state -string
                    :trans [-text]})

(def ->close-string {:pred  #(= \" %)
                     :state -string
                     :trans standard})

(def ->comment {:pred  #(= \; %)
                :state -comment
                :trans [-text]})

(def ->char {:pred  #(= \\ %)
             :state -char
             :trans [-text]})

(def ->number {:pred  #(contains? numbers %)
               :state -number
               :trans [-text
                       -list
                       -vector
                       -map
                       -string]})

(def ->keyword {:pred  #(= \: %)
                :state -keyword
                :trans [-text]})

(def ->word {:pred  #(some (fn [[l & _]] (= l %)) words)
             :state -word
             :trans [-text
                     -list
                     -vector
                     -map
                     -string]})

(def transitions
  {-list     [->open-list ->close-list]
   -vector   [->open-vector ->close-vector]
   -map      [->open-map ->close-map]
   -function [->function]
   -text     [->text]
   -string   [->open-string ->close-string]
   -comment  [->comment]
   -word     [->word]
   -number   [->number]
   -char     [->char]
   -keyword  [->keyword]})

(defn- propagate [transiton c]
  (or (some->> (:trans transiton)
               (map transitions)
               (flatten)
               (some #(when ((:pred %) c) %)))
      transiton))

(defn colour [state])

(defrecord Painter [transiton action store])

(def ^:const store [])
(def ^:const emit :emit)
(def ^:const consume :consume)
(def ^:const process :process)
(def ^:const terminate :terminate)

(def painter (Painter. ->text consume store))

(defn push [painter c]
  (update painter :store #(conj % c)))

(defn act [painter action]
  (update painter :action action))

(defn transition [painter c]
  (update painter :transiton #(propagate % c)))

(defn react [next-painter cur-painter c])

(defn rewind [{:keys [transitions]}])

(defn paint! [painter f])