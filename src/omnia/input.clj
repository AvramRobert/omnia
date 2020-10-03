(ns omnia.input
  (:require [clojure.core.match :as m]
            [clojure.string :as s]
            [schema.core :as schema]
            [omnia.event :as e]
            [clojure.set :refer [union map-invert]]
            [omnia.more :refer [do-until Point Region]]))

(def Lines
  [[Character]])

(def Expansion
  (schema/enum :word :expr))

(schema/defrecord Seeker
  [lines     :- Lines
   cursor    :- Point
   expansion :- Expansion
   selection :- (schema/maybe Point)    ;; this represents the starting point of the selection, the end is the cursor
   height    :- schema/Int
   clipboard :- (schema/maybe Seeker)   ;; this is a seeker, question is, does it need to be one?
   history   :- [Seeker]
   rhistory  :- [Seeker]]
  Object
  (toString [_]
    (str {:lines     lines
          :height    height
          :cursor    cursor
          :expansion expansion
          :selection selection
          :clipboard clipboard
          :history   history
          :rhistory  rhistory})))

(def empty-map {})
(def empty-vec [])
(def empty-list '())
(def empty-seeker
  (map->Seeker {:lines     empty-vec
                :cursor    [0 0]
                :height    0
                :expansion :word
                :selection nil
                :clipboard nil
                :history   empty-list
                :rhistory  empty-list}))

(def open-pairs {\( \) \[ \] \{ \}})
(def closed-pairs (map-invert open-pairs))
(def open-tokens (set (keys open-pairs)))
(def closed-tokens (set (vals open-pairs)))
(def parens (union open-tokens closed-tokens))
(def tokens (union parens #{\"}))

(defn resize [seeker]
  (assoc seeker :height (-> seeker :lines count)))

(defn seeker
  ([] (seeker []))
  ([lines] (-> empty-seeker (assoc :lines lines) (resize))))

(def empty-line (seeker [empty-vec]))

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
  ([seeker [_ y]]
   (-> seeker :lines (nth y []))))

(defn sym-at
  ([seeker]
   (sym-at seeker (:cursor seeker)))
  ([seeker [x y]]
   (-> seeker (line [x y]) (nth x nil))))

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
                         (mapv vec)
                         (apply f)
                         (mapv vec)))))

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
                   (let [lines (->> line (split-at x) (mapv vec) (apply f))]
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

(defn reset-to [seeker cursor]
  (move seeker (constantly cursor)))

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
          (let [height (:height seeker)
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

(defn- progress-with [seeker f]
  (let [h (-> seeker :height dec)
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
(defn progress [seeker] (progress-with seeker identity))

(defn left [seeker]
  (when (not= [0 0] (:cursor seeker))
    (-> seeker (regress) (sym-at))))

(defn right [seeker]
  (sym-at seeker))

(defn selection? [seeker]
  (-> seeker :selection nil? not))

(defn reselect
  ([seeker f]
   (reselect seeker f identity))
  ([seeker f g]
   (update seeker :selection #(if (empty? %) (g %) (f %)))))

(defn soft-select [seeker]
  (reselect seeker identity (constantly (:cursor seeker))))

(defn select [seeker]
  (let [f (constantly (:cursor seeker))]
    (-> seeker (assoc :expansion :word) (reselect f f))))

(defn deselect [seeker]
  (-> seeker
      (reselect (constantly nil))
      (assoc :expansion :word)))

(defn selection [seeker]
  (let [cursor (:cursor seeker)
        init   (-> seeker (soft-select) :selection)
        [start end] (sort-by (juxt second first) [init cursor])]
    {:start start
     :end   end}))

(defn join [this-seeker that-seeker]
  (let [[x y] (:cursor that-seeker)
        ths   (:height this-seeker)]
    (-> this-seeker
        (rebase #(concat % (:lines that-seeker)))
        (assoc :selection (:selection that-seeker))
        (reselect (fn [[xs ys]] [xs (+ ys ths)]))
        (reset-to [x (+ y ths)]))))

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
        (reset-to end)
        (do-until simple-delete #(-> % :cursor (= start))))))

(defn pair? [seeker]
  (m/match [(left seeker) (right seeker)]
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

(defn- overwrite [seeker]
  (cond-> seeker
          (selection? seeker) delete))

(defn insert [seeker input]
  (m/match [input (right (overwrite seeker))]
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
           [\space _] (simple-insert seeker input)
           :else (-> (overwrite seeker) (simple-insert input))))

(defn- jump [seeker f look]
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

(defn jump-right [seeker]
  (jump seeker progress right))

(defn munch [seeker]
  (let [x (progress seeker)]
    (cond
      (= (:cursor seeker) (:cursor x)) seeker
      (pair? seeker) (-> seeker (pair-delete) (regress))
      :else (delete x))))

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
             [{:lines [a]}] (-> (overwrite seeker)
                                (split #(vector (concat %1 a %2)))
                                (move-y #(+ % y))
                                (move-x #(+ % x)))
             [{:lines [a b]}] (-> (overwrite seeker)
                                  (split #(vector (concat %1 a) (concat b %2)))
                                  (move-y #(+ % y))
                                  (reset-x x))
             [{:lines [a & b]}] (-> (overwrite seeker)
                                    (split #(concat [(concat %1 a)]
                                                    (drop-last b)
                                                    [(concat (last b) %2)]))
                                    (move-y #(+ % y))
                                    (reset-x x))
             :else seeker)))

(defn select-all [seeker]
  (-> seeker (start) (select) (end)))

(defn- a-pair? [this that]
  (or (= (open-pairs this :none) that)
      (= (closed-pairs this :none) that)))

(defn open-expand [seeker]
  "Note: This assumes that the right-hand side of the seeker starts with an open parens"
  (let [l (right seeker)
        ending (end seeker)]
    (loop [seen 1
           current (-> seeker (select) (progress))]
      (let [r (right current)]
        (cond
          (zero? seen) [:matched current]
          (= (:cursor ending) (:cursor current)) [:unmatched current]
          (a-pair? l r) (recur (dec seen) (progress current))
          (= l r) (recur (inc seen) (progress current))
          :else (recur seen (progress current)))))))

(defn closed-expand [seeker]
  "Note: This assumes that the left-hand side of the seeker starts with a closed parens"
  (let [r (left seeker)
        beginning (start seeker)
        switch #(assoc % :cursor    (:cursor seeker)
                         :selection (:cursor %))]
    (loop [seen 1
           current (-> seeker (select) (regress))]
      (let [l (left current)]
        (cond
          (zero? seen) [:matched (switch current)]
          (= (:cursor beginning) (:cursor current)) [:unmatched (switch current)]
          (a-pair? l r) (recur (dec seen) (regress current))
          (= l r) (recur (inc seen) (regress current))
          :else (recur seen (regress current)))))))

(defn near-expand [seeker]
  "Note: This assumes that the seeker isn't neighbouring parens"
  (let [beginning (start seeker)]
    (loop [seen ()
           current seeker]
      (let [l (left current) r (first seen)]
        (cond
          (and (empty? seen) (open-pairs l)) (-> current (regress) (open-expand))
          (a-pair? l r) (recur (rest seen) (regress current))
          (= (:cursor beginning) (:cursor current)) [:unmatched (-> current (select) (end))]
          (closed-pairs l) (recur (conj seen l) (regress current))
          :else (recur seen (regress current)))))))

(defn expand [seeker]
  (-> (m/match [(:expansion seeker) (left seeker) (right seeker)]
               [:word \( \)] (-> seeker (near-expand) (second))
               [:word \[ \]] (-> seeker (near-expand) (second))
               [:word \{ \}] (-> seeker (near-expand) (second))
               [:word \space \space] (-> seeker (near-expand) (second))
               [:word (:or \" \space) (:or \) \] \})] (-> seeker (progress) (closed-expand) (second))
               [:word (:or \) \] \}) _] (-> seeker (closed-expand) (second))
               [(:or :word :expr) _ (:or \( \[ \{)] (-> seeker (open-expand) (second))
               [:word (:or \( \[ \{ \" \space nil) _] (-> seeker (select) (jump-right))
               [:word _ _] (-> seeker (jump-left) (select) (jump-right))
               :else (-> seeker (near-expand) (second)))
      (assoc :expansion :expr)))

(defn find-pair [seeker]
  (letfn [(choose [[m s]]
            (when (= :matched m) (-> s (regress) (selection))))]
    (cond
      (open-pairs (right seeker)) (-> seeker (open-expand) (choose))
      (closed-pairs (left seeker)) (-> seeker (closed-expand) (choose))
      :else (-> seeker (near-expand) (choose)))))

(defn forget [seeker]
  (assoc seeker :history empty-list
                :rhistory empty-list))

(defn remember
  ([seeker]
   (remember seeker seeker))
  ([seeker that]
   (let [history (:history seeker)]
     (->> history
          (cons (forget that))
          (take 50)
          (assoc seeker :history)))))

(defn undo [{:keys [history rhistory clipboard] :as seeker}]
  (if (empty? history)
    seeker
    (-> history
        (first)
        (assoc :clipboard clipboard)
        (assoc :history (rest history))
        (assoc :rhistory (-> seeker (forget) (cons rhistory))))))

(defn redo [{:keys [history rhistory clipboard] :as seeker}]
  (if (empty? rhistory)
    seeker
    (-> rhistory
        (first)
        (assoc :clipboard clipboard)
        (assoc :rhistory (rest rhistory))
        (assoc :history (-> seeker (forget) (cons history))))))

(defn stringify [seeker]
  (->> (repeat "\n")
       (take (:height seeker))
       (interleave (:lines seeker))
       (map #(apply str %))
       (s/join)))

(defn print-seeker [seeker]
  (->> seeker
       (:lines)
       (map #(apply str %))
       (run! println)))

(schema/defn process [seeker :- Seeker
                      event  :- e/InputEvent] :- Seeker
  (condp = (:action event)
    e/expand            (-> seeker (expand))
    e/select-all        (-> seeker (select-all))
    e/copy              (-> seeker (copy) (deselect))
    e/cut               (-> seeker (remember) (cut) (deselect))
    e/paste             (-> seeker (remember) (paste) (deselect))
    e/up                (-> seeker (climb) (deselect))
    e/down              (-> seeker (fall) (deselect))
    e/left              (-> seeker (regress) (deselect))
    e/right             (-> seeker (progress) (deselect))
    e/jump-left         (-> seeker (jump-left) (deselect))
    e/jump-right        (-> seeker (jump-right) (deselect))
    e/select-up         (-> seeker (soft-select) (climb))
    e/select-down       (-> seeker (soft-select) (fall))
    e/select-left       (-> seeker (soft-select) (regress))
    e/select-right      (-> seeker (soft-select) (progress))
    e/jump-select-left  (-> seeker (soft-select) (jump-left))
    e/jump-select-right (-> seeker (soft-select) (jump-right))
    e/backspace         (-> seeker (remember) (delete) (deselect))
    e/delete            (-> seeker (remember) (munch) (deselect))
    e/break             (-> seeker (remember) (break) (deselect))
    e/undo              (-> seeker (undo) (deselect))
    e/redo              (-> seeker (redo) (deselect))
    e/character         (-> seeker (insert (:value event)) (deselect))
    seeker))