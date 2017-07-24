(ns omnia.input
  (require [clojure.core.match :as m]
           [clojure.string :as s]
           [clojure.set :refer [union]]
           [omnia.more :refer [do-until]]))

(defrecord Event [action key])
(defrecord Seeker [lines cursor height expansion selection clipboard]
  Object
  (toString [this]
    (str {:lines lines
          :height height
          :cursor cursor
          :expansion expansion
          :selection selection
          :clipboard clipboard})))

(def empty-vec [])
(def empty-seeker (Seeker. empty-vec [0 0] 0 :word nil nil))

(def open-pairs {\( \) \[ \] \{ \}})
(def closed-pairs (clojure.set/map-invert open-pairs))
(def open-tokens (set (keys open-pairs)))
(def closed-tokens (set (vals open-pairs)))
(def parens (union open-tokens closed-tokens))
(def tokens (union parens #{\"}))

(defn apair? [this that]
  (or (= (open-pairs this) that)
      (= (closed-pairs this) that)))

(defn resize [seeker]
  (assoc seeker :height (-> seeker :lines count)))

(defn seeker
  ([] (seeker []))
  ([lines] (-> empty-seeker (assoc :lines lines) (resize))))

(defn from-string [string]
  (letfn [(char-vec [xs] (vec (.toCharArray xs)))
          (newlines [xs] (if (empty? xs)
                           (->> string
                                (char-vec)
                                (mapv (fn [_] [])))
                           xs))]
    (->> string
         (s/split-lines)
         (mapv char-vec)
         (newlines)
         (seeker))))

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
  (:height seeker))

(defn rebase [seeker f]
  (-> seeker (update :lines (comp vec f)) (resize)))

(defn peer [seeker f]
  "Morphism: Lines => Lines
  Looks between the lines specified by the current line number (y-position) and
  applies a binary function `f` on the lines that come before it (left), together with the ones that
  come `after` (right).
  The left parameter of `f` contains the lines that come before the current line.
  The right parameter of `f` contains the current line and the rest that come after.

  `f` is expected to return valid `lines` of text.
  These then replace the previous lines on the seeker.
  Returns a new seeker that includes the transformation of `f`."
  (let [[_ y] (:cursor seeker)]
    (rebase seeker #(->> (split-at y %)
                         (map vec)
                         (apply f)
                         (map vec)))))

(defn split [seeker f]
  "Morphism: Line => Lines
  Looks within the line specified by the current line number and cursor position (x:y position).
  Applies a binary function `f` on the characters that come `before` the current cursor position (left),
  together with the ones that come `after` (right).
  The left parameter of `f` contains the characters that come before the cursor position at the
  current line.
  The right parameter of `f` contains the current character and the rest that come after at the
  current line.

  `f` is expected to return valid `lines` of text.
  These then replace the one line on which `f` was applied and get merged with the rest.
  Returns a new seeker that includes the transformation of `f`."
  (let [[x _] (:cursor seeker)]
    (peer seeker (fn [l [line & r]]
                   (let [lines (->> line (split-at x) (map vec) (apply f))]
                     (concat l lines r))))))

(defn slice [seeker f]
  "Morphism: Line => Line
  Looks within the line specified by the current line number and cursor position (x:y postion).
  Applies a binary function `f` on the characters that come `before` the current cursor position (left),
  together with the ones that come `after` (right).
  The left parameter of `f` contains the characters that come before the cursor position at the
  current line.
  The right parameter of `f` contains the current character and the rest that come after at the
  current line.

  `f` is expected to return one valid `line` of text.
  This then replaces the line on which `f` was applied.
  Returns a new seeker that includes the transformation of `f`."
  (split seeker (fn [l r] [(f l r)])))

(defn slicel [seeker f]
  (slice seeker (fn [l r] (concat (f l) r))))

(defn slicer [seeker f]
  (slice seeker (fn [l r] (concat l (f r)))))

(defn move [seeker f]
  (update seeker :cursor f))

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

(defn end-y [{:keys [height] :as seeker}]
  (let [y-max (if (zero? height) 0 (dec height))]
    (move seeker (fn [[x _]] [x y-max]))))

(defn start [seeker]
  (-> seeker (start-y) (start-x)))

(defn end [seeker]
  (-> seeker (end-y) (end-x)))

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
        ths (height this-seeker)]
    (-> this-seeker
        (rebase #(concat % (:lines that-seeker)))
        (assoc :selection (:selection that-seeker))
        (reselect (fn [[xs ys]] [xs (+ ys ths)]))
        (move (fn [_] [x (+ y ths)])))))

(defn join-many [& seekers]
  (reduce join empty-seeker seekers))

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

(defn break [seeker]
  (-> seeker
      (split vector)
      (move-y inc)
      (start-x)))

(defn simple-delete [seeker]
  (-> seeker
      (slicel drop-last)
      (regress-with merge-lines)))

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

(defn delete [seeker]
  (cond
    (selection? seeker) (chunk-delete seeker)
    (pair? seeker) (pair-delete seeker)
    (tokens (left seeker)) (regress seeker)
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

(defn jump [seeker f look]
  (letfn [(blanks? [s] (blank? (look s)))
          (lits? [s] (not (blanks? s)))
          (tokens? [s] (tokens (look s)))
          (bounds? [s] (or (-> s (:cursor) (first) (zero?))
                           (nil? (look s))))
          (go [pred] (do-until (f seeker) f pred))]
    (cond
      (blanks? seeker) (go #(or (bounds? %) (tokens? %) (lits? %)))
      (tokens? seeker) (go #(or (bounds? %) (lits? %)))
      :else (go #(or (bounds? %) (tokens? %) (blanks? %))))))

(defn jump-left [seeker]
  (jump seeker regress left))

(defn jump-right [ seeker]
  (jump seeker advance center))

(defn munch [seeker]
  (let [x (advance seeker)]
    (cond
      (= (:cursor seeker) (:cursor x)) seeker
      (pair? seeker) (-> seeker (pair-delete) (regress))
      :else (delete x))))

(defn is-empty? [seeker]
  (= (:lines seeker) (:lines empty-seeker)))

(defn extract [{:keys [height] :as seeker}]
  (let [{[sx sy] :start
         [ex ey] :end} (selection seeker)
        y (if (= ey height) ey (inc ey))]
    (-> seeker
        (rebase #(subvec % sy y))
        (end)
        (slicel #(subvec % 0 ex))
        (start)
        (slicer #(drop sx %)))))

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
  (-> seeker (deselect) (start) (select) (end)))

(defrecord IStack [stack touched?])
(def istack (IStack. (list) false))
(defn used? [{:keys [stack touched?]}]
  (and (empty? stack) touched?))
(defn touch [istack]
  (update istack :touched? #(if % % true)))
(defn ipush [istack v]
  (-> (update istack :stack conj v) (touch)))
(defn ipop [istack]
  (update istack :stack rest))
(defn top [{:keys [stack]}]
  (first stack))

(defn balance [seeker]
  (letfn [(expr? [s] (parens (center s)))
          (balanced [s] [:balanced s])
          (unbalanced [s] [:unbalanced s])]
    (loop [stack istack
           prev nil
           current seeker]
      (let [value (center current)
            pushed (top stack)]
        (cond
          (used? stack)  (if (expr? seeker) (balanced current) (unbalanced current))
          (= prev current) (unbalanced current)
          (open-pairs value) (recur (ipush stack value) current (advance current))
          (closed-pairs value) (if (apair? value pushed)
                                 (recur (ipop stack) current (advance current))
                                 (unbalanced current))
          :else (recur stack current (advance current)))))))

(defn nearest [seeker]
  (loop [stack (list)
         prev nil
         current (regress seeker)]
    (let [value (center current)
          pushed (first stack)]
      (cond
        (= prev current) current
        (and (open-pairs value) (empty? stack)) current
        (and (open-pairs value) (apair? value pushed)) (recur (rest stack) current (regress current))
        (closed-pairs value) (recur (conj stack value) current (regress current))
        :else (recur stack current (regress current))))))


(defn expand-expr [seeker]
  (if (open-pairs (center seeker))
    (-> (deselect seeker) (select) (balance) (second) (assoc :expansion :expr))
    (-> (deselect seeker) (nearest) (expand-expr))))

(defn expand-word [seeker]
  (m/match [(left seeker) (center seeker)]
           [\( \)] (expand-expr seeker)
           [\[ \]] (expand-expr seeker)
           [\{ \}] (expand-expr seeker)
           [\space \space] (expand-expr seeker)
           [\space (:or \) \] \})] (expand-expr seeker)
           [(:or \( \[ \{) \space] (expand-expr seeker)
           [_ (:or \( \[ \{)] (expand-expr seeker)
           [(:or \) \] \}) _] (-> seeker (regress) (expand-expr))
           [(:or \( \[ \{) _] (-> seeker (select) (jump-right))
           [(:or \space \") _] (-> seeker (select) (jump-right))
           [nil _] (-> seeker (select) (jump-right))
           [_ nil] (-> seeker (select) (jump-left))
           :else (-> seeker (jump-left) (select) (jump-right))))

(defn expand [seeker]
  (case (:expansion seeker)
    :word (-> seeker (expand-word) (assoc :expansion :expr))
    :expr (expand-expr seeker)
    seeker))

(defn find-pair [seeker]
  (letfn [(f [[balance s]] (when (= :balanced balance)
                             (selection (regress s))))]
    (cond
      (open-pairs (center seeker)) (-> (deselect seeker) (select) (balance) (f))
      (closed-pairs (center seeker)) (-> (deselect seeker) (nearest) (select) (balance) (f))
      :else nil)))

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

(defn process [seeker event]
  (case (:action event)
    :expand (-> seeker expand)
    :select-all (-> seeker select-all)
    :paste (-> seeker (paste) (deselect))
    :copy (-> seeker (copy) (deselect))
    :cut (-> seeker (cut) (deselect))
    :up (-> seeker (climb) (deselect))
    :down (-> seeker (fall) (deselect))
    :left (-> seeker (regress) (deselect))
    :right (-> seeker (advance) (deselect))
    :jump-left (-> seeker (jump-left) (deselect))
    :jump-right (-> seeker (jump-right) (deselect))
    :select-up (-> seeker (select) (climb))
    :select-down (-> seeker (select) (fall))
    :select-left (-> seeker (select) (regress))
    :select-right (-> seeker (select) (advance))
    :jump-select-left (-> seeker (select) (jump-left))
    :jump-select-right (-> seeker (select) (jump-right))
    :backspace (-> seeker (delete) (deselect))
    :delete (-> seeker (munch) (deselect))
    :enter (-> seeker (break) (deselect))
    :char (-> seeker (insert (:key event)) (deselect))
    seeker))