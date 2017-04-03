(ns omnia.more)

(defn take-right [n coll]
  (if-let [x (take-last n coll)] x []))

(defn drop+ [n coll]
  (if (neg? n) '() (drop n coll)))

(defn vtake-right [n coll]
  (vec (take-right n coll)))

(defn bound-inc [value max]
  (let [x (inc value)]
    (if (>= x max) value x)))                               ;; >= because we count from 0

(defn bound-dec [value min]
  (let [x (dec value)]
    (if (< x min) value x)))

