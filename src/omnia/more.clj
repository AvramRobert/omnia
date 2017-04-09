(ns omnia.more)

(defn take-right [n coll]
  (if-let [x (take-last n coll)] x '()))

(defn bound-inc [value max]
  (let [x (inc value)]
    (if (>= x max) value x)))                               ;; >= because we count from 0

(defn bound-dec [value min]
  (let [x (dec value)]
    (if (< x min) value x)))

(defn make-str [coll sep]
  (let [c (count coll)
        max (if (zero? c) 0 (+ c (dec c)))]
    (->> coll
        (reduce #(str %1 %2 sep) "")
        (drop-last)
        (apply str))))

