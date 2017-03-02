(ns omnia.transmute
  (:gen-class))

(defrecord Seeker [lines cursor])

(def empty-seeker (Seeker. [] [0 0]))

(defn- slice [seeker f]
  (let [[x y] (:cursor seeker)]
    (update-in seeker [:lines y]
               #(vec
                 (if-let [line %]
                  (->> line
                       (split-at x)
                       (map vec)
                       (apply f))
                  (f [] []))))))
;; returns a seeker
(defn slicel [seeker f]
  (slice seeker (fn [l r] (concat (f l) r))))

;; returns a seeker
(defn slicer [seeker f]
  (slice seeker (fn [l r] (concat l (f r)))))

(defn edit [seeker f]
  (update-in seeker [:lines] f))

(defn prepend-at [seeker s]
  (slicer seeker #(concat (clojure.string/split s #"") %)))

(defn displace-at [seeker]
  (slicel seeker #(drop-last %)))

(defn n-of [c n]
  (take n (repeat c)))

(defn within [seeker [x y]]
  (let [lines (:lines seeker)]
    (if (and (>= y 0)
             (< y (.count lines)))
      (if (and (>= x 0)
               (<= x (.count (nth lines y))))
        [x y]
        [(first (:cursor seeker)) y])
      (:cursor seeker))))

(defn move [seeker f]
  (update-in seeker [:cursor] #(within seeker (f %))))

(defn inputs [seeker key]
  (case key
    :left (move seeker (fn [[x y]] [(dec x) y]))
    :right (move seeker (fn [[x y]] [(inc x) y]))
    :up (move seeker (fn [[x y]] [x (dec y)]))
    :down (move seeker (fn [[x y]] [x (inc y)]))
    :tab (-> seeker
             (prepend-at (apply str (n-of " " 3)))
             (move (fn [[x y]] [(+ x 3) y])))
    :backspace (-> seeker
                   (displace-at)
                   (move (fn [[x y]] [(dec x) y])))
    :enter (->> seeker
                (edit #(conj % []))
                (move (fn [[x y]] [0 (inc y)])))
    (-> seeker
        (slicer #(conj % key))
        (move (fn [[x y]] [(inc x) y])))))
