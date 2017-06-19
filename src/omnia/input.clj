(ns omnia.input
  (require [clojure.core.match :as m]
           [clojure.string :as s]
           [clojure.set :refer [union]]
           [omnia.more :refer [do-until]]))

(defrecord Seeker [lines cursor height expansion selection clipboard])

(def empty-vec [])
(def empty-seeker (Seeker. empty-vec [0 0] (delay 0) :word nil nil))

(def open-exps #{\( \[ \{})
(def closing-exps #{\) \] \}})
(def exprs (union open-exps closing-exps #{\"}))

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

(defn from-string [s]
  (-> s (str->lines) (seeker)))

(defn blank? [character]
  (= \space character))

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
                nx     (f x)]
            (if (<= 0 nx length)
              [nx y]
              [x y])))))

(defn move-y [seeker f]
  (move seeker
        (fn [[x y]]
          (let [height (height seeker)
                ny     (f y)]
            (if (<= 0 ny (dec height))
              [x ny]
              [x y])))))

(defn reset-x [seeker value]
  (move-x seeker (fn [_] value)))

(defn reset-y [seeker value]
  (move-y seeker (fn [_] value)))

(defn end-x [seeker]
  (move seeker (fn [[_ y]] [(-> seeker line count) y])))

(defn start-x [seeker]
  (reset-x seeker 0))

(defn start-y [seeker]
  (reset-y seeker 0))

(defn end-y [seeker]
  (move seeker (fn [[x _]] [x (height seeker)])))

(defn start [seeker]
  (-> seeker (start-y) (start-x)))

(defn end [seeker]
  (-> seeker (end-y) (move-y dec) (end-x)))

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

(defn left
  ([seeker]
   (left seeker identity))
  ([seeker f]
   (-> seeker (regress) (sym-at) f)))

(defn right
  ([seeker]
   (right seeker identity))
  ([seeker f]
   (-> seeker (advance) (sym-at) f)))

(defn center
  ([seeker] (center seeker identity))
  ([seeker f] (-> seeker (sym-at) (f))))

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
  (-> seeker
      (reselect (constantly nil))
      (assoc :expansion :word)))

(defn selection [seeker]
  (let [cursor (:cursor seeker)
        init   (-> seeker (select) :selection)
        [start end] (sort-by (juxt second first) [init cursor])]
    {:start start
     :end   end}))

(defn join [this-seeker that-seeker]
  (let [[x y] (:cursor that-seeker)
        ths (height this-seeker)
        tht (height that-seeker)]
    (-> this-seeker
        (update :lines #(join-lines % (:lines that-seeker)))
        (assoc :height (delay (+ ths tht)))
        (assoc :selection (:selection that-seeker))
        (reselect (fn [[xs ys]] [xs (+ ys ths)]))
        (move (fn [[_ oy]] [x (+ y oy)])))))

(defn join-many [& seekers]
  (reduce join seekers))

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

(defn pair? [seeker]
  (m/match [(left seeker) (center seeker)]
           [\( \)] true
           [\[ \]] true
           [\{ \}] true
           [\" \"] true
           :else false))

(defn expr? [seeker]
  (contains? exprs (center seeker)))

(defn delete [seeker]
  (cond
    (selection? seeker) (chunk-delete seeker)
    (pair? seeker) (pair-delete seeker)
    (expr? (regress seeker)) (regress seeker)
    :else (simple-delete seeker)))

(defn simple-insert [seeker value]
  (-> seeker (slicel #(conj % value)) (move-x inc)))

(defn pair-insert [seeker [key pair]]
  (-> seeker (slicel #(conj % key pair)) (move-x inc)))

(defn insert [seeker input]
  (m/match [input (center seeker)]
           [\) \)] (move-x seeker inc)
           [\] \]] (move-x seeker inc)
           [\} \}] (move-x seeker inc)
           [\" \"] (move-x seeker inc)
           [\( _] (pair-insert seeker [\( \)])
           [\[ _] (pair-insert seeker [\[ \]])
           [\{ _] (pair-insert seeker [\{ \}])
           [\) _] (pair-insert seeker [\( \)])
           [\] _] (pair-insert seeker [\[ \]])
           [\} _] (pair-insert seeker [\{ \}])
           [\" _] (pair-insert seeker [\" \"])
           :else (simple-insert seeker input)))

(defn jump [seeker f]
  (letfn [(beginning? [s] (-> s :cursor first zero?))]
    (do-until (f seeker) f #(or (beginning? %)
                                (nil? (center %))
                                (blank? (left %))
                                (blank? (center %))
                                (expr? (regress %))
                                (expr? %)))))

(defn munch [seeker]
  (let [x (advance seeker)]
    (cond
      (= (:cursor seeker) (:cursor x)) seeker
      (pair? seeker) (-> seeker (pair-delete) (regress))
      :else (delete x))))

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
  (let [copied (some-> seeker :clipboard (end))
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
  (-> seeker (deselect) (start-y) (start-x) (select) (end)))

(defn- expansion [seeker progress bump? end?]
  (letfn [(limit? [s] (or (= (end seeker) s)
                          (= (start seeker) s)))]
    (-> seeker
        (progress)
        (vector 0)
        (do-until (fn [[s i]]
                    (cond
                      (neg? i) [s i]
                      (bump? s) [(progress s) (inc i)]
                      (end? s) [(progress s) (dec i)]
                      :else [(progress s) i]))
                  (fn [[s i]] (or (and (end? s) (zero? i))
                                  (limit? s))))
        (first))))


(defn expand-left [seeker]
  (expansion seeker regress
             #(->> % (center) (contains? closing-exps))
             #(->> % (center) (contains? open-exps))))

(defn expand-right [seeker]
  (expansion seeker advance
             #(->> % (left) (contains? open-exps))
             #(->> % (left) (contains? closing-exps))))

(defn expand-expr [seeker]
  (let [l (-> seeker (expand-left) :cursor)
        r (-> seeker (expand-right) :cursor)]
    (-> seeker
        (deselect)
        (move (fn [_] r))
        (assoc :selection l)
        (assoc :expansion :expr))))

(defn expand-word [seeker]
  (m/match [(left seeker) (center seeker)]
           [\( \)] (expand-expr seeker)
           [\[ \]] (expand-expr seeker)
           [\{ \}] (expand-expr seeker)
           [\" \"] (expand-expr seeker)
           [\space \space] (expand-expr seeker)
           [\space (:or \) \] \})] (expand-expr seeker)
           [(:or \( \[ \{) \space] (expand-expr seeker)
           [_ (:or \( \[ \{)] (-> seeker (advance) (expand-expr))
           [(:or \) \] \}) _] (-> seeker (regress) (expand-expr))
           [(:or \( \[ \{) _] (-> seeker (select) (jump advance))
           [(:or \space \") _] (-> seeker (select) (jump advance))
           :else (-> seeker (jump regress) (select) (jump advance))))

(defn expand [seeker]
  (case (:expansion seeker)
    :word (-> seeker (expand-word) (assoc :expansion :expr))
    :expr (expand-expr seeker)
    seeker))

(defn stringify [seeker]
  (->> (repeat "\n")
       (take (height seeker))
       (interleave (:lines seeker))
       (map #(apply str %))
       (s/join)))

(defn print-seeker [seeker]
  (->> seeker
       (:lines)
       (map #(apply str %))
       (run! println)))

(defn inputs [seeker stroke]
  (m/match [stroke]
           [{:key \w :ctrl true}] (-> seeker expand)
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
