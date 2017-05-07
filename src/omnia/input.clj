(ns omnia.input
  (:gen-class)
  (require [clojure.core.match :as m]
           [clojure.string :as s]
           [omnia.more :refer [do-until]]))

(comment
  "Enhancements:
      0. Give selections a cardinality. You should be able to select more than one thing.
      1. Add line width limit and truncation.
      2. Try out transients to improve performance.")

(defrecord Seeker [lines cursor height selection clipboard])
(defrecord Select [start end dir])

(def empty-vec [])
(def empty-seeker (Seeker. empty-vec [0 0] (delay 0) nil nil))

(def matching-rules {\{ \}
                     \[ \]
                     \( \)
                     \" \"})

(defn resize [seeker]
  (assoc seeker :height (delay (-> seeker :lines count))))

(defn seeker
  ([] (seeker []))
  ([lines] (-> empty-seeker (assoc :lines lines) (resize)))
  ([lines height] (-> empty-seeker (assoc :lines lines) (assoc :height (delay height)))))

(defn join-lines [& lines]
  (vec (apply concat lines)))

(defn str->lines [string]
  (letfn [(char-vec [xs] (vec (.toCharArray xs)))
          (newlines [xs] (if (empty? xs)
                           (->> string
                                (char-vec)
                                (map (fn [_] []))
                                (vec))
                           xs))]
    (->> string
         (s/split-lines)
         (map char-vec)
         (newlines)
         (vec))))

(defn line
  ([seeker]
   (line seeker (:cursor seeker)))
  ([seeker [x y]]
   (-> seeker :lines (nth y []))))

(defn sym-at
  ([seeker]
    (sym-at seeker (:cursor seeker)))
  ([seeker [x y]]
   (-> seeker (line [x y]) (nth x nil))))

(defn height [seeker]
  @(:height seeker))

(defn rebase [seeker f]
  (-> seeker (update :lines (comp vec f)) (resize)))

(defn peer [seeker f]
  (let [[_ y] (:cursor seeker)]
    (rebase seeker #(->> (split-at y %)
                         (map vec)
                         (apply f)
                         (map vec)))))

(defn split [seeker f]
  (let [[x _] (:cursor seeker)]
    (peer seeker (fn [l [line & r]]
                   (let [lines (->> line (split-at x) (map vec) (apply f))]
                     (concat l lines r))))))

(defn slice [seeker f]
  (split seeker (fn [l r] [(f l r)])))

(defn slicel [seeker f]
  (slice seeker (fn [l r] (concat (f l) r))))

(defn slicer [seeker f]
  (slice seeker (fn [l r] (concat l (f r)))))

(defn move [seeker f]
  (update seeker :cursor f))

(defn displace [seeker]
  (slicel seeker drop-last))

(defn move-x [seeker f]
  (move seeker
        (fn [[x y]]
          (let [length (-> seeker line count)
                nx (f x)]
            (if (and (>= nx 0) (<= nx length))
              [nx y]
              [x y])))))

(defn move-y [seeker f]
  (move seeker
        (fn [[x y]]
          (let [height (height seeker)
                ny (f y)]
            (if (and (>= ny 0) (< ny height))
              [x ny]
              [x y])))))

(defn end-x [seeker]
  (move seeker (fn [[_ y]] [(-> seeker line count) y])))

(defn start-x [seeker]
  (move-x seeker (fn [_] 0)))

(defn start-y [seeker]
  (move-y seeker (fn [_] 0)))

(defn end-y [seeker]
  (move seeker (fn [[x _]] [x (height seeker)])))

(defn left
  ([seeker]
   (left seeker identity))
  ([seeker f]
   (-> seeker (move-x dec) (sym-at) f)))

(defn right
  ([seeker]
   (right seeker identity))
  ([seeker f]
   (-> seeker sym-at f)))

(defn selection? [seeker]
  (-> seeker :selection nil? not))

(defn reselect
  ([seeker f]
    (reselect seeker f identity))
  ([seeker f g]
   (update seeker :selection #(if (empty? %) (g %) (f %)))))

(defn select [seeker]
  (reselect seeker identity (constantly (:cursor seeker))))

(defn deselect [seeker]
  (reselect seeker (constantly nil)))

(defn selection [seeker]
  (let [cursor (:cursor seeker)
        init (-> seeker (select) :selection)
        [start end] (sort-by (juxt second first) [init cursor])]
    {:start start
     :end end}))

(defn join [this-seeker that-seeker]
  "Until I add multiple selections, always keep the selections of the latest seeker"
  (let [[x y] (:cursor that-seeker)
        ths (height this-seeker)
        tht (height that-seeker)]
    (-> this-seeker
        (update :lines #(join-lines % (:lines that-seeker)))
        (assoc :height (delay (+ ths tht)))
        (assoc :selection (:selection that-seeker))
        (reselect (fn [[xs ys]] [xs (+ ys ths)]))
        (move (fn [[_ oy]] [x (+ y oy)])))))

(defn- advance-with [seeker f]
  (let [[_ y] (:cursor seeker)
        h (-> seeker height dec)
        w (-> seeker line count)]
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
(defn advance [seeker] (advance-with seeker identity))

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

(defn merge-lines [seeker]
  (peer seeker (fn [l [a b & t]]
                 (-> (conj l (concat a b))
                     (concat t)))))

(defn rollback [seeker]
  (regress-with seeker merge-lines))

(defn break [seeker]
  (-> seeker
      (split vector)
      (move-y inc)
      (start-x)))

(def simple-delete (comp rollback displace))

(defn pair-delete [seeker]
  (-> seeker
      (slice #(concat (drop-last %1) (rest %2)))
      (move-x dec)))

(defn chunk-delete [seeker]
  (let [{start :start
         end   :end} (selection seeker)]
    (-> seeker
        (move (fn [_] end))
        (do-until simple-delete #(-> % :cursor (= start))))))

(defn pair? [seeker rules]
  (some-> seeker (left #(get rules %)) (= (right seeker))))

(defn delete
  ([seeker] (delete seeker matching-rules))
  ([seeker rules]
   (cond
     (selection? seeker) (chunk-delete seeker)
     (pair? seeker rules) (pair-delete seeker)
     :else (simple-delete seeker))))

(defn simple-insert [seeker value]
  (-> seeker (slicel #(conj % value)) (move-x inc)))

(defn pair-insert [seeker [key pair]]
  (-> seeker (slicel #(conj % key pair)) (move-x inc)))

(defn insert
  ([seeker key]
   (insert seeker key matching-rules))
  ([seeker key rules]
   (let [pair (get rules key)
         rematched (-> rules (clojure.set/map-invert) (get key))]
     (cond
       (and (nil? pair) (nil? rematched)) (simple-insert seeker key)
       (and (nil? pair) (right seeker #(= % key))) (move-x seeker inc)
       (nil? rematched) (pair-insert seeker [key pair])
       (and (= key rematched) (right seeker #(= % key))) (move-x seeker inc)
       (= key rematched) (pair-insert seeker [key key])
       :else (simple-insert seeker key)))))

(defn start? [seeker]
  (-> seeker :cursor first zero?))

(defn jump [seeker f]
  (do-until seeker f #(or (start? %)
                          (nil? (sym-at %))
                          (= (sym-at %) \space))))

(defn munch
  ([seeker] (munch seeker matching-rules))
  ([seeker rules]
   (let [x (advance seeker)]
     (cond
       (= (:cursor seeker) (:cursor x)) seeker
       (pair? seeker rules) (-> seeker (pair-delete) (regress))
       :else (delete x rules)))))

(defn stringify [seeker]
  (->> (repeat "\n")
       (take (height seeker))
       (interleave (:lines seeker))
       (map #(apply str %))
       (s/join)))

(defn is-empty? [seeker]
  (= (:lines seeker) (:lines empty-seeker)))

(defn char-key? [stroke]
  (char? (:key stroke)))

(defn extract [seeker]
  (let [{start :start
         end   :end} (selection seeker)]
    (-> seeker
        (end-y)
        (end-x)
        (do-until simple-delete #(-> % :cursor (= end)))
        (move (fn [_] start))
        (do-until simple-delete #(-> % :cursor (= [0 0]))))))

(defn copy [seeker]
  (->> seeker (extract) (assoc seeker :clipboard)))

(defn cut [seeker]
  (cond-> (copy seeker)
          (selection? seeker) (delete)))

(defn paste [seeker]
  (let [copied (some-> seeker :clipboard (end-y) (move-y dec) (end-x))
        [x y] (some-> copied :cursor)]
    (m/match [copied]
             [{:lines [a]}] (-> seeker
                                  (split #(vector (concat %1 a %2)))
                                  (move-y #(+ % y))
                                  (move-x #(+ % x)))
             [{:lines [a b]}] (-> seeker
                                    (split #(vector (concat %1 a) (concat b %2)))
                                    (move-y #(+ % y))
                                    (move-x (constantly x)))
             [{:lines [a & b]}] (-> seeker
                                      (split #(concat [(concat %1 a)]
                                                      (drop-last b)
                                                      [(concat (last b) %2)]))
                                      (move-y #(+ % y))
                                      (move-x (constantly x)))
             :else seeker)))

(defn select-all [seeker]
  (-> seeker (start-y) (start-x) (select) (end-y) (move-y dec) (end-x)))

(defn print-seeker [seeker]
  (->> seeker
       (:lines)
       (map #(apply str %))
       (map println)
       (doall)))

(defn inputs [seeker stroke]
  (m/match [stroke]
           [{:key \a :ctrl true}] (-> seeker (select-all))
           [{:key \v :alt true}] (-> seeker (paste) (deselect))
           [{:key \c :alt true}] (-> seeker (copy) (deselect))
           [{:key \x :alt true}] (-> seeker (cut) (deselect))
           [{:key :up :shift true}] (-> seeker (select) (climb))
           [{:key :down :shift true}] (-> seeker (select) (fall))
           [{:key :left :ctrl true :shift true}] (-> seeker (select) (jump regress))
           [{:key :right :ctrl true :shift true}] (-> seeker (select) (jump advance))
           [{:key :left :shift true}] (-> seeker (select) (regress))
           [{:key :right :shift true}] (-> seeker (select) (advance))
           [{:key :left :ctrl true}] (-> seeker (deselect) (jump regress))
           [{:key :right :ctrl true}] (-> seeker (deselect) (jump advance))
           [{:key :left}] (-> seeker (deselect) (regress))
           [{:key :right}] (-> seeker (deselect) (advance))
           [{:key :up}] (-> seeker (deselect) (climb))
           [{:key :down}] (-> seeker (deselect) (fall))
           [{:key :backspace}] (-> seeker (delete) (deselect))
           [{:key :delete}] (-> seeker (munch) (deselect))
           [{:key :enter}] (-> seeker (deselect) (break))
           [_ :guard char-key?] (-> seeker (deselect) (insert (:key stroke)))
           :else seeker))
