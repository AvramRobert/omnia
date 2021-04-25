(ns omnia.util.arithmetic)

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

(defn mod* [num div]
  (let [pdiv (if (zero? div) 1 div)]
    (mod num pdiv)))
