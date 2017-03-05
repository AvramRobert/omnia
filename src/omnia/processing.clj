(ns omnia.processing
  (:gen-class)
  (require [clojure.core.match :as m]))

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

(defn move [seeker f]
  (update-in seeker [:cursor] f))

(defn displace-at [seeker]
  (slicel seeker drop-last))

(defn sym-at [seeker]
  (let [[x y] (:cursor seeker)]
    (-> seeker :lines (nth y []) (nth x nil))))

(defn move-x [seeker f]
  (move seeker
        (fn [[x y]]
          (let [length (-> seeker :lines (nth y []) (.count))
                nx (f x)]
            (if (and (>= nx 0) (<= nx length))
              [nx y]
              [x y])))))

(defn move-y [seeker f]
  (move seeker
        (fn [[x y]]
          (let [height (-> seeker :lines count)
                ny (f y)]
            (if (and (>= ny 0) (< ny height))
              [x ny]
              [x y])))))

(defn rollback [seeker]
  (if (-> seeker :lines last empty?)
    (-> seeker
        (edit (comp vec drop-last))
        (move-y dec)
        (move-x (fn [_] (-> seeker :lines drop-last last count))))
    (move-x seeker dec)))

(defn auto-close [seeker parens]
  (-> seeker
      (slicel #(case parens
                 \( (conj % \( \))
                 \[ (conj % \[ \])
                 \{ (conj % \{ \})
                 %))
      (move-x inc)))

(def simple-delete (comp rollback displace-at))
(defn pair-delete [seeker]
  (-> seeker (slice #(concat (drop-last %1) (rest %2)))))

(defn auto-delete [seeker]
  (let [left (-> seeker (move-x dec) (sym-at))
        right (sym-at seeker)]
    (m/match [left right]
             [\( \)] (-> seeker pair-delete (move-x dec))
             [\[ \]] (-> seeker pair-delete (move-x dec))
             [\{ \}] (-> seeker pair-delete (move-x dec))
             :else (simple-delete seeker))))

(defn auto-insert [seeker key]
  (m/match [key (sym-at seeker)]
           [\) \)] (move-x seeker inc)
           [\] \]] (move-x seeker inc)
           [\} \}] (move-x seeker inc)
           [(:or \( \[ \{) _] (auto-close seeker key)
           :else (-> seeker
                     (slicel #(conj % key))
                     (move-x inc))))

;; FIXME: Going overreaching the line when going right or left does not jump to the next line
;; FIXME: enter does not split the string when pressing inside of string
(defn inputs [seeker key]
  (m/match [key]
           [:left] (move-x seeker dec)
           [:right] (move-x seeker inc)
           [:up] (move-y seeker dec)
           [:down] (move-y seeker inc)
           [:backspace] (auto-delete seeker)
           [:enter] (-> seeker
                        (edit #(conj % []))
                        (move-y inc)
                        (move-x (fn [_] 0)))
           :else (auto-insert seeker key)))
