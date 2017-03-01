(ns omnia.transmute
  (:gen-class))

(defrecord Seeker [lines cursor])

(defn- slice [seeker f]
  (let [lines (:lines seeker)
        [x y] (:cursor seeker)]
    (if (empty? lines)
      (assoc lines y (vec (f [] [])))
      (let [[hs ts] (split-at (inc x) (nth lines y))]
        (assoc lines y (vec (f hs ts)))))))

(defn slicel [seeker f]
  (assoc seeker :lines
                (slice seeker (fn [hs ts] (concat (f hs) ts)))))

(defn slicer [seeker f]
  (assoc seeker :lines
                (slice seeker (fn [hs ts] (concat hs (f ts))))))

(defn edit [seeker f]
  (update-in seeker [:lines] f))

(defn prepend-at [seeker s]
  (edit seeker (fn [lines]
                 (slicer seeker #(concat (clojure.string/split s #"") %)))))

(defn displace-at [seeker]
  (edit seeker (fn [lines]
                 (slicel seeker #(drop-last %)))))

(defn n-of [c n]
  (take n (repeat c)))

(defn contain [seeker [x y]]
  (if (and (>= y 0)
           (< y (.count (:lines seeker))))
    (if (and (>= x 0)
             (< x (.count (nth (:lines seeker) y))))
      [x y]
      [(first (:cursor seeker)) y])
    (:cursor seeker)))

(defn move [seeker f]
  (update-in seeker [:cursor] #(contain seeker (f %))))

;; FIXME: There is a slight problem with this
;; If I press enter, whilst the cursor is inside some line
;; then, essentially, what needs to happen is that the vector of chars
;; describing that line would be split, and another vector would be
;; put in between.
;; this i sort of have to into look at and take care of
(defn inputs [seeker key]
  (case key
    :tab (prepend-at seeker (apply str (n-of " " 3)))
    :left (move seeker (fn [[x y]] [(dec x) y]))
    :right (move seeker (fn [[x y]] [(inc x) y]))
    :up (move seeker (fn [[x y]] [x (dec y)]))
    :down (move seeker (fn [[x y]] [x (inc y)]))
    :backspace (displace-at seeker)
    :enter (edit seeker #(conj % []))
    (edit seeker (fn [hs]))))
