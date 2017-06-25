(ns omnia.more
  (require [clojure.core.match :as m]))

(defn take-right [n coll]
  (if-let [x (take-last n coll)] x '()))

(defn inc< [value max]
  (let [x (inc value)]
    (if (>= x max) value x)))                               ;; >= because we count from 0

(defn dec< [value min]
  (let [x (dec value)]
    (if (< x min) value x)))

(defn -- [& values]
  (let [r (apply - values)]
    (if (neg? r) 0 r)))

(defn ++ [& values]
  (let [r (apply + values)]
    (if (neg? r) 0 r)))

(defn zip-all [coll-a coll-b]
  (m/match [coll-a coll-b]
           [[a & t] []] (cons [a nil] (lazy-seq (zip-all t [])))
           [[] [a & t]] (cons [nil a] (lazy-seq (zip-all [] t)))
           [[a & t1] [b & t2]] (cons [a b] (lazy-seq (zip-all t1 t2)))
           :else nil))

(defn reduce-idx
  ([f seed coll]
   (reduce-idx 0 f seed coll))
  ([from f seed coll]
   (if (empty? coll)
     seed
     (recur (inc from) f (f from seed (first coll)) (rest coll)))))

(defn do-until [elm f p]
  (if (p elm) elm (recur (f elm) f p)))

(defn mod* [num div]
  (let [pdiv (if (zero? div) 1 div)]
    (mod num pdiv)))

(defn map-vals [f hmap]
  (reduce (fn [nmap [k v]]
            (assoc nmap k (f v))) {} hmap))

(defn or-else [nilable else]
  (if nilable nilable else))

(defmacro time-return [& body]
  `(let [s# (System/nanoTime)
        val# ~@body
        e# (System/nanoTime)
        total# (/ (- e# s#) 1000000.0)]
    [(str total# " ms") val#]))

(defmacro time-out [& body]
  `(let [[s# v#] (time-return ~@body)]
     (spit "debug" s#)
     v#))