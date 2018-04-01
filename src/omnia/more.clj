(ns omnia.more
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(defn inc< [value max]
  (let [x (inc value)]
    (if (> x max) value x)))

(defn dec< [value min]
  (let [x (dec value)]
    (if (< x min) value x)))

(defn -- [& values]
  (let [r (apply - values)]
    (if (neg? r) 0 r)))

(defn ++ [& values]
  (let [r (apply + values)]
    (if (neg? r) 0 r)))

(defn reduce-idx
  ([f seed coll]
   (reduce-idx f 0 seed coll))
  ([f from seed coll]
   (-> (fn [[idx b] a] [(inc idx) (f idx b a)])
       (reduce [from seed] coll)
       (nth 1))))

(defn do-until [elm f p]
  (if (p elm) elm (recur (f elm) f p)))

(defn mod* [num div]
  (let [pdiv (if (zero? div) 1 div)]
    (mod num pdiv)))

(defn map-vals [f hmap]
  (reduce (fn [nmap [k v]]
            (assoc nmap k (f v))) {} hmap))

(defn gulp-or-else [path else]
  (if (-> path (io/file) (.exists))
    (-> path (slurp) (edn/read-string))
    else))

(defmacro omnia-version []
  (System/getProperty "omnia.version"))

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

(defmacro debug [body]
  `(spit "debug" ~body))