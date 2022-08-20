(ns omnia.util.collection)

(def empty-vector [])

(defn reduce-idx
  ([f seed coll]
   (reduce-idx f 0 seed coll))
  ([f from seed coll]
   (-> (fn [[idx b] a] [(inc idx) (f idx b a)])
       (reduce [from seed] coll)
       (nth 1))))

(defn dissoc-nth [vector idx]
  (vec (concat (take idx vector) (drop (inc idx) vector))))

(defn merge-from-both [map1 map2]
  (merge-with (fn [a b] (or a b)) map1 map2))

(defn map-vals [f hmap]
  (if (empty? hmap)
    hmap
    (reduce (fn [nmap [k v]]
              (assoc nmap k (f v))) {} hmap)))

(defn do-until [elm f p]
  (let [elm' (f elm)]
    (if (p elm')
      elm'
      (recur elm' f p))))

(defn bounded-subvec [vector start end]
  (let [size  (count vector)
        start (if (neg? start) 0 start)
        end   (if (> end size) size end)]
    (subvec vector start end)))

(defmacro assoc-new [map key val]
  `(let [m# ~map
         k# ~key
         v# ~val]
     (if (= (get m# k#) v#)
       m#
       (assoc m# k# v#))))