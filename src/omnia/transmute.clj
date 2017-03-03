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
  (slicel seeker drop-last))

(defn n-of [c n]
  (take n (repeat c)))

(defn move [seeker f]
  (update-in seeker [:cursor] f))

(defn move-x [seeker f]
  (move seeker
        (fn [[x y]]
          (let [length (.count (nth (:lines seeker) y []))
                nx (f x)]
            (if (and (>= nx 0)
                     (<= nx length))
              [nx y]
              [x y])))))

(defn move-y [seeker f]
  (move seeker
        (fn [[x y]]
          (let [height (-> seeker :lines count)
                ny (f y)]
            (if (and (>= ny 0)
                     (< ny height))
              [x ny]
              [x y])))))

(defn rollback [seeker]
  (if (-> seeker :lines last empty?)
    (-> seeker
        (edit (comp vec drop-last))
        (move-y dec)
        (move-x (fn [_] (-> seeker :lines drop-last last count))))
    (move-x seeker dec)))

(defn inputs [seeker key]
  (case key
    :left (move-x seeker dec)
    :right (move-x seeker inc)
    :up (move-y seeker dec)
    :down (move-y seeker inc)
    :tab (-> seeker
             (prepend-at (apply str (n-of " " 3)))
             (move-x #(+ % 3)))
    :backspace (-> seeker
                   (displace-at)
                   (rollback))
    :enter (-> seeker
               (edit #(conj % []))
               (move-y inc)
               (move-x (fn [_] 0)))
    (-> seeker
        (slicer #(conj % key))
        (move-x inc))))
