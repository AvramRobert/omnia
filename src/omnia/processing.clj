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
  ([seeker [y]]
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

(defn end-x [seeker]
  (move seeker (fn [[_ y]] [(-> seeker line count) y])))

(defn start-x [seeker]
  (move-x seeker (fn [_] 0)))

(defn- advance-with [seeker f]
  (let [[_ y] (:cursor seeker)
        h (-> seeker :lines count dec)
        w (-> seeker :lines (nth y []) count)]
    (m/match [(:cursor seeker)]
             [[w h]] seeker
             [[w _]] (-> seeker (move-y inc) (start-x) f)
             :else (move-x seeker inc))))

(defn- regress-with [seeker f]
  (m/match [(:cursor seeker)]
           [[0 0]] seeker
           [[0 _]] (-> seeker (move-y dec) (end-x) f)
           :else (move-x seeker dec)))

(defn regress [seeker] (regress-with seeker identity))

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
      (start-x)))

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

(defn simple-insert [seeker value]
  (-> seeker (slicel #(conj % value)) (move-x inc)))

(defn auto-delete [seeker]
  (let [left (-> seeker (move-x dec) (sym-at))
        right (sym-at seeker)]
    (m/match [left right]
             [\( \)] (-> seeker pair-delete (move-x dec))
             [\[ \]] (-> seeker pair-delete (move-x dec))
             [\{ \}] (-> seeker pair-delete (move-x dec))
             :else (simple-delete seeker))))

(defn pair-insert [seeker key]
  (m/match [key (sym-at seeker)]
           [\) \)] (move-x seeker inc)
           [\] \]] (move-x seeker inc)
           [\} \}] (move-x seeker inc)
           [(:or \( \[ \{) _] (auto-close seeker key)
           :else (simple-insert seeker key)))

(defn climb [seeker]
  (let [offset (move-y seeker dec)]
    (if (sym-at offset)
      offset
      (-> seeker (start-x) (regress)))))

(defn fall [seeker]
  (let [offset (move-y seeker inc)]
    (if (sym-at offset)
      offset
      (-> seeker (move-y inc) (end-x)))))

;; FIXME: String character matching. Actually, better yet, configuration based character completion
(defn inputs [seeker key]
  (m/match [key]
           [:left] (regress-with seeker identity)
           [:right] (advance-with seeker identity)
           [:up] (climb seeker)
           [:down] (fall seeker)
           [:backspace] (auto-delete seeker)
           [:enter] (break seeker)
           :else (pair-insert seeker key)))
(comment
  "Idea: The way you want this configuration to work is by having a function that accepts some input and
then, based on that input, composes the appropriate functions the user wants for his repl session.
This then returns a function that handles the inputs based on that composition. ")
