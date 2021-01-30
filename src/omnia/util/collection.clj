(ns omnia.util.collection)

(defn merge-culling [f m1 m2]
  "Merges two maps. Keep just the common elements.
   Removes any element that is `nil` either by itself
   or as a result of applying `f`."
  (reduce
    (fn [nm [k a]]
      (if-let [b (get m2 k)]
        (or (some->> b (f a) (assoc nm k))
            nm)
        (assoc nm k a))) {} m1))

(defn reduce-idx
  ([f seed coll]
   (reduce-idx f 0 seed coll))
  ([f from seed coll]
   (-> (fn [[idx b] a] [(inc idx) (f idx b a)])
       (reduce [from seed] coll)
       (nth 1))))

(defn merge-from-both [map1 map2]
  (merge-with (fn [a b] (or a b)) map1 map2))

(defn map-vals [f hmap]
  (reduce (fn [nmap [k v]]
            (assoc nmap k (f v))) {} hmap))

(defn map-keys [f hmap]
  (reduce (fn [nmap [k v]]
            (assoc nmap (f k) v)) {} hmap))

(defn do-until [elm f p]
  (if (p elm) elm (recur (f elm) f p)))