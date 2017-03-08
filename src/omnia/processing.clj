(ns omnia.processing
  (:gen-class)
  (require [clojure.core.match :as m]))

(defrecord Seeker [lines cursor])

(def empty-seeker (Seeker. [] [0 0]))

(defn edit [seeker f]
  (update-in seeker [:lines] #(f % (:cursor seeker))))

(defn line
  ([seeker]
   (line seeker (:cursor seeker)))
  ([seeker [x y]]
   (-> seeker :lines (nth y []))))

(defn sym-at [seeker]
  (let [[x y] (:cursor seeker)]
    (-> seeker line (nth x nil))))

(defn peer [seeker f]
  (edit seeker
        (fn [lines [_ y]]
              (->> lines (split-at y) (map vec) (apply f) (vec)))))

(defn slice [seeker f]
  (let [[x _] (:cursor seeker)]
    (peer seeker (fn [l [line & r]]
                   (let [nl (->> line (split-at x) (map vec) (apply f))]
                     (concat (conj l nl) r))))))

;; returns a seeker
(defn slicel [seeker f]
  (slice seeker (fn [l r] (concat (f l) r))))

;; returns a seeker
(defn slicer [seeker f]
  (slice seeker (fn [l r] (concat l (f r)))))

(defn move [seeker f]
  (update-in seeker [:cursor] f))

(defn displace-at [seeker]
  (slicel seeker drop-last))

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

(defn end [seeker]
  (move seeker (fn [[_ y]] [(-> seeker line count) y])))

(defn- advance-with [seeker f]
  (let [[_ y] (:cursor seeker)
        h (-> seeker :lines count dec)
        w (-> seeker :lines (nth y []) count)]
    (m/match [(:cursor seeker)]
             [[w h]] seeker
             [[w _]] (-> seeker (move-y inc) (move-x (fn [_] 0)) f)
             :else (move-x seeker inc))))

(defn- regress-with [seeker f]
  (m/match [(:cursor seeker)]
           [[0 0]] seeker
           [[0 _]] (-> seeker
                       (move-y dec)
                       (move (fn [[x y]] [(-> seeker (line [x y]) count) y]))
                       f)
           :else (move-x seeker dec)))

(defn merge-lines [seeker]
  (peer seeker (fn [l [a b & t]]
                 (-> l
                     (conj (vec (concat a b)))
                     (concat t)))))

(defn rollback [seeker]
  (regress-with seeker merge-lines))

(defn break [seeker]
  (-> seeker
      (slice #(vector %1 %2))
      (peer (fn [l [h & t]] (concat (conj l (first h) (second h)) t)))
      (move-y inc)
      (move-x (fn [_] 0))))

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

(defn climb [seeker]
  (let [offset (move-y seeker dec)]
    (if (sym-at offset)
      offset
      (-> seeker
          (move-x (fn [_] 0))
          (regress-with identity)))))

(defn fall [seeker]
  (let [offset (move-y seeker inc)]
    (if (sym-at offset)
      offset
      (-> seeker
          (move-y inc)
          (move (fn [[x y]] [(-> seeker (line [x y]) count) y]))))))

;; FIXME: The same with (move-_ (fn [_] 0)). Perhaps define a function that can move the cursor at the beginning and end of a line respectively
;; FIXME: String character matching. Actually, better yet, configuration based character completion
(defn inputs [seeker key]
  (m/match [key]
           [:left] (regress-with seeker identity)
           [:right] (advance-with seeker identity)
           [:up] (climb seeker)
           [:down] (fall seeker)
           [:backspace] (auto-delete seeker)
           [:enter] (break seeker)
           :else (auto-insert seeker key)))
