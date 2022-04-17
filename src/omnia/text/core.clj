(ns omnia.text.core
  (:require [clojure.core.match :as m]
            [schema.core :as s]
            [omnia.config.components.event :as e]
            [clojure.string :as string]
            [clojure.set :refer [union map-invert]]
            [omnia.util.schema :refer [=> Point Region]]
            [omnia.util.arithmetic :refer [inc<]]
            [omnia.util.collection :refer [do-until reduce-idx dissoc-idx]]))

(def Line [Character])

(def ExpansionScope (s/enum :word :expr))

(def Selection {:region (s/maybe Region)})

(def Seeker
  {:lines     [Line]
   ;; position in text, the cursor is placed at the index where a character can be input
   :cursor    Point
   :size      s/Int
   :expansion ExpansionScope
   :history   [(s/recursive #'Seeker)]
   :rhistory  [(s/recursive #'Seeker)]
   ;; range of text selected. Inclusive start and exclusive in end
   :selection (s/maybe Region)
   :clipboard (s/maybe (s/recursive #'Seeker))})

(def empty-seeker
  {:lines     []
   :cursor    [0 0]
   :size      0
   :expansion :word
   :selection nil
   :clipboard nil
   :history   '()
   :rhistory  '()})

(def open-pairs {\( \) \[ \] \{ \}})
(def closed-pairs (map-invert open-pairs))
(def paired-tokens (union #{\"}
                          (set (keys open-pairs))
                          (set (keys closed-pairs))))

(s/defn resize :- Seeker
        [seeker :- Seeker]
        (assoc seeker :size (-> seeker :lines count)))

(s/defn seeker :- Seeker
  ([lines :- [Line]]
   (seeker lines [0 0]))
  ([lines :- [Line]
    cursor :- Point]
   (-> empty-seeker (assoc :lines (vec lines) :cursor cursor) (resize))))

(s/def empty-line :- Seeker
  (seeker [[]]))

(s/defn from-string :- Seeker
  "Examples:
    1. 'ab\nc' => [[a b] [c]]
    2. '1\n\n' => [[a] []];
    3. '\n\n' => [[] []]"
  [input :- s/Str]
  (loop [lines  []
         rem    input]
    (if (empty? rem)
      (seeker lines)
      (recur (->> rem
                  (take-while #(not= % \newline))
                  (vec)
                  (conj lines))
             (->> rem
                  (drop-while #(not= % \newline))
                  (drop 1))))))

(defn from-marked-text [strings]
  "Reads inputs strings into a seeker.
   The strings are allowed markers specifying a cursor position and a possible selection range.

   The cursor is identified through the character: `|`
   The selection is identified through:
      a) Start: `<`
      b) End: `> (inclusive)
   Examples:
    1. (from-marked-text \"hel|lo\" \"world\")
        => [[h e l l o] [w o r l d]] : [3 0] : nil

    2. (from-marked-text \"he<ll|o\" \"worl>d\")
        => [[h e l l o] [w o r l d]] : [4 0] : { :start [2 0] :end [3 1] }

    3. (from-marked-text \"he<ll|o\" \">world\")
        => [[h e l l o] []] : [4 0] : { :start [2 0] :end [0 1] }"
  (letfn [(point [{:keys [y find remove latest]} line]
            (->> line
                 (filter #(not (contains? remove %)))
                 (reduce-idx (fn [x p c] (if (= c find) [x, y] p)) latest)))]
    (loop [[s & ss] strings
           lines  []
           cursor [0 0]
           start  nil
           end    nil]
      (if (some? s)
        (recur
          ss
          (->> s (filter #(not (contains? #{\| \< \>} %))) (vec) (conj lines))
          (point {:remove #{\< \>}
                  :find   \|
                  :latest cursor
                  :y      (count lines)} s)
          (point {:remove #{\| \>}
                  :find   \<
                  :y      (count lines)
                  :latest start} s)
          (point {:remove #{\| \<}
                  :find   \>
                  :latest end
                  :y      (count lines)} s))
        (cond-> (seeker lines cursor)
                (and start end (not= start end)) (assoc :selection {:start start :end end}))))))

(s/defn from-cursored-string :- Seeker
  "Examples:
    1. 'ab|c\nd' => [[a b c] [d]], [2 0]
    2. 'abc|\nd' => [[a b c] [d]], [3 0]"
  [input :- s/Str]
  (loop [lines  []
         rem    input
         cursor nil]
    (if (empty? rem)
      (seeker lines cursor)
      (let [line?  #(not= % \newline)
            caret? #(= % \|)
            lit?   #(not (caret? %))
            line   (take-while line? rem)]
        (recur (->> line (filter lit?) (vec) (conj lines))
               (->> rem (drop-while line?) (drop 1))
               (or cursor
                   (->> line
                        (map-indexed (fn [idx c]
                                       (when (caret? c)
                                         [idx (count lines)])))
                        (some #(when % %)))))))))

(s/defn space? :- s/Bool
  [character :- Character]
  (= \space character))

(s/defn rebase :- Seeker
  [seeker :- Seeker f]
  (-> seeker (update :lines (comp vec f)) (resize)))

(s/defn line-at :- Line
  [seeker :- Seeker, y :- s/Int]
  (-> seeker (:lines) (nth y [])))

(s/defn current-line :- Line
  [seeker :- Seeker]
  (let [y (-> seeker (:cursor) (nth 1))]
    (line-at seeker y)))

(s/defn char-at :- (s/maybe Character)
  [seeker :- Seeker
   [x y] :- Point]
  (-> seeker (line-at y) (nth x nil)))

(s/defn peer :- Seeker
 "Looks between the lines specified by the cursor's `y` coordinate.
  Applies a binary function `f`.
  left-hand side = the lines that came before the y'th coordinate
  right-hand side = the lines that came after the y'th coordinate (including it)

  `f` is expected to return valid `lines` of text.
  These then replace the initial lines on the seeker."
  [seeker :- Seeker
   f      :- (=> [Line] [Line] [Line])]
  (let [[_ y] (:cursor seeker)]
    (rebase seeker #(->> (split-at y %)
                         (mapv vec)
                         (apply f)
                         (mapv vec)))))

(s/defn split :- Seeker
 "Looks at the line where the cursor is currently placed.
  Applies a binary function `f`.
  left-hand side = the characters that come before the cursor
  right-hand side = the characters that come after the cursor

  `f` is expected to return valid `lines` of text.
  These then replace the one line on which `f` was applied and get merged with the rest."
  [seeker :- Seeker
   f      :- (=> Line Line [Line])]
  (let [[x _] (:cursor seeker)]
    (peer seeker (fn [l [line & r]]
                   (let [lines (->> line (split-at x) (mapv vec) (apply f))]
                     (concat l lines r))))))

(s/defn enrich :- Seeker
  "Looks at the current line. Applies a function `f` on it
  that should return more than one line."
  [seeker :- Seeker
   f      :- (=> Line [Line])]
  (split seeker (fn [l r] (f (vec (concat l r))))))

(s/defn switch :- Seeker
  "Looks at the line where the cursor is currently.
  Applies a function `f` on that line.
  `f` is expected to return a new line of text and
  replaces the one line on which it was applied."
  [seeker :- Seeker
   f :- (=> Line (s/maybe Line))]
  (let [[_ y] (:cursor seeker)]
    (if-let [line' (f (current-line seeker))]
      (rebase seeker #(assoc % y (vec line')))
      (rebase seeker #(dissoc-idx y %)))))

(s/defn slice :- Seeker
 "Looks at the line where the cursor is currently placed.
  Applies a binary function `f`.
  left-hand side = the characters that come before the cursor
  right-hand side = the characters that come after the cursor

  `f` is expected to return one valid line of text.
  This then replaces the line on which `f` was applied."
  [seeker :- Seeker
   f      :- (=> Line Line Line)]
  (split seeker (fn [l r] [(f l r)])))

(s/defn slicel :- Seeker
  [seeker :- Seeker
   f      :- (=> Line Line)]
  (slice seeker (fn [l r] (concat (f l) r))))

(s/defn slicer :- Seeker
  [seeker :- Seeker
   f      :- (=> Line Line)]
  (slice seeker (fn [l r] (concat l (f r)))))

;; FIXME: There should be only 1 `reset-cursor` function
;; That function should be use in every `move-*`
;; No more higher order functions: as much as we can
;; The reset function should check bounds.
;; THATS THE ONLY THING THAT SHOULD
(s/defn move :- Seeker
  [seeker :- Seeker
   f      :- (=> Point Point)]
  (update seeker :cursor f))

(s/defn reset-to :- Seeker
  [seeker :- Seeker
   cursor :- Point]
  (move seeker (constantly cursor)))

(s/defn move-x :- Seeker
  [seeker :- Seeker
   f      :- (=> s/Int s/Int)]
  (move seeker
        (fn [[x y]]
          (let [length (-> seeker current-line count)
                nx     (f x)]
            (if (<= 0 nx length)
              [nx y]
              [x y])))))

(s/defn move-y :- Seeker
  [seeker :- Seeker
   f      :- (=> s/Int s/Int)]
  (move seeker
        (fn [[x y]]
          (let [height (:size seeker)
                ny     (f y)]
            (if (<= 0 ny (dec height))
              [x ny]
              [x y])))))

(s/defn reset-x :- Seeker
  [seeker :- Seeker
   value  :- s/Int]
  (move-x seeker (fn [_] value)))

(s/defn reset-y :- Seeker
  [seeker :- Seeker
   value  :- s/Int]
  (move-y seeker (fn [_] value)))

(s/defn end-x :- Seeker
  [seeker :- Seeker]
  (move seeker (fn [[_ y]] [(-> seeker current-line count) y])))

(s/defn start-x :- Seeker
  [seeker :- Seeker]
  (reset-x seeker 0))

(s/defn start-y :- Seeker
  [seeker :- Seeker]
  (reset-y seeker 0))

(s/defn end-y :- Seeker
  [seeker :- Seeker]
  (let [height (:size seeker)
        y-max (if (zero? height) 0 (dec height))]
    (move seeker (fn [[x _]] [x y-max]))))

(s/defn start :- Seeker
  [seeker :- Seeker]
  (-> seeker (start-y) (start-x)))

(s/defn end :- Seeker
  [seeker :- Seeker]
  (-> seeker (end-y) (end-x)))

(s/defn move-right-with :- Seeker
  [seeker :- Seeker
   f :- (=> Seeker Seeker)]
  (let [h (-> seeker :size dec)
        w (-> seeker current-line count)]
    (m/match [(:cursor seeker)]
             [[w h]] seeker
             [[w _]] (-> seeker (move-y inc) (start-x) (f))
             :else (move-x seeker inc))))

(s/defn move-left-with :- Seeker
  [seeker :- Seeker
   f :- (=> Seeker Seeker)]
  (m/match [(:cursor seeker)]
           [[0 0]] seeker
           [[0 _]] (-> seeker (move-y dec) (end-x) (f))
           :else (move-x seeker dec)))

(s/defn move-left :- Seeker
  [seeker :- Seeker]
  (move-left-with seeker identity))

(s/defn move-right :- Seeker
  [seeker :- Seeker]
  (move-right-with seeker identity))

(s/defn move-up :- Seeker
  [seeker :- Seeker]
  (let [[x y] (:cursor seeker)
        y'    (dec y)]
    (if (< y' 0)
      seeker
      (let [x' (-> seeker (line-at y') (count))]
        (if (> x x')
          (reset-to seeker [x' y'])
          (reset-to seeker [x y']))))))

(s/defn move-down :- Seeker
  [seeker :- Seeker]
  (let [[x y] (:cursor seeker)
        y'   (inc y)
        size (:size seeker)]
    (if (>= y' size)
      seeker
      (let [x' (-> seeker (line-at y') (count))]
        (if (> x x')
          (reset-to seeker [x' y'])
          (reset-to seeker [x y']))))))

(s/defn current-char :- (s/maybe Character)
  [seeker :- Seeker]
  (char-at seeker (:cursor seeker)))

(s/defn previous-char :- (s/maybe Character)
  [seeker :- Seeker]
  (when (not= [0 0] (:cursor seeker))
    (-> seeker (move-left) (current-char))))

(s/defn next-char :- (s/maybe Character)
  [seeker :- Seeker]
  (let [next (move-right seeker)]
    (when (not= (:cursor seeker) (:cursor next))
      (current-char next))))

(s/defn jump :- Seeker
  [seeker :- Seeker
   move   :- (=> Seeker Seeker)
   look   :- (=> Seeker (s/maybe Character))]
  (letfn [(blank? [s] (some-> s (look) (space?)))
          (literal? [s] (not (blank? s)))
          (paired-token? [s] (paired-tokens (look s)))
          (bound? [s] (nil? (look s)))]
    (cond
      (bound? seeker)         (-> seeker (do-until move #(or (bound? %) (paired-token? %) (literal? %))))
      (blank? seeker)         (-> seeker (do-until move #(or (bound? %) (paired-token? %) (literal? %))))
      (paired-token? seeker)  (-> seeker (do-until move #(or (bound? %) (literal? %))))
      :else                   (-> seeker (do-until move #(or (bound? %) (paired-token? %) (blank? %)))))))

(s/defn jump-left :- Seeker
  [seeker :- Seeker]
  (jump seeker move-left previous-char))

(s/defn jump-right :- Seeker
  [seeker :- Seeker]
  (jump seeker move-right current-char))

(s/defn reset-expansion :- Seeker
  [seeker :- Seeker
   scope :- ExpansionScope]
  (assoc seeker :expansion scope))

(s/defn selecting? :- s/Bool
  [seeker :- Seeker]
  (-> seeker :selection some?))

(s/defn reset-selection :- Seeker
  [seeker :- Seeker
   region :- (s/maybe Selection)]
  (assoc seeker :selection region))

(s/defn reset-clipboard :- Seeker
  [seeker :- Seeker
   content :- (s/maybe Seeker)]
  (assoc seeker :clipboard content))

(s/defn reset-history :- Seeker
  [seeker :- Seeker
   undo-history :- [Seeker]
   redo-history :- [Seeker]]
  (assoc seeker :history undo-history :rhistory redo-history))

(s/defn distance :- s/Int
  "Function that quantises the distance and direction of point
   Ve = [xe, ye] relative to point Vs = [xs, ys] in the text space.
   Positive numbers represent a distance after Vs, whilst negative ones represent a distance before Vs."
  [[xs ys] :- Point
   [xe ye] :- Point
   text :- Seeker]
  (letfn [(chars-between [[xs ys] [xe ye]]
            (let [lower     xe
                  upper     (-> text (line-at ys) (count) (- xs))
                  middle    (->> (range (inc ys) ye)
                                 (reduce (fn [sum y] (-> text (line-at y) (count) (+ sum))) 0))
                  new-lines (- ye ys)]
              (+ upper lower middle new-lines)))]
    (cond
      (= ys ye) (- xe xs)
      (> ys ye) (- (chars-between [xe ye] [xs ys]))
      :else     (chars-between [xs ys] [xe ye]))))

(s/defn adjust-against :- (s/maybe Region)
  "Algorithm for figuring out new region.
   Given
      Cc = current cursor
      Cd = displaced cursor
      Cs = region start cursor
      Ce = region end cursor

   a. Determine which region cursor (Cs or Ce) is going to move and be replaced by the displaced cursor (Cd).
      The coordinate (Cm) closest to the current cursor (Cc) is the one move:

        Cm = min (abs (distance (Cs, Cc)), abs(distance (Ce, Cc)))

   b. Determine which coordinate hasn't moved.
      The remainder coordinate (Ck) after subtracting the moved coordinate (Cm) from the region coordinate set:

        Ck = {Cs, Ce} - Cr

   c. Determine if the cursor moved backwards or forwards.
      The sign of the distance (dm) between the moved coordinate (Cm) and the displaced coordinate (Cd):

        dm = distance (Cm, Cd) { left, if dm < 0
                               { right, if dm > 0

   d. Determine new region area.
"

  [text :- Seeker
   displaced :- Seeker]
  (let [Cc (:cursor text)
        Cd (:cursor displaced)
        Cs (-> text (:selection) (:start))
        Ce (-> text (:selection) (:end))
        Cm (min-key #(Math/abs ^long (distance % Cc text)) Cs Ce)
        Ck (if (= Cm Cs) Ce Cs)
        [start end] (sort-by (juxt second first) [Cd Ck])]
    (if (= start end)
      nil
      {:start start :end end})))

(s/defn initial-region :- (s/maybe Region)
  [text :- Seeker
   displaced :- Seeker]
  (let [Cc (:cursor text)
        Cd (:cursor displaced)
        [start end] (sort-by (juxt second first) [Cc Cd])]
    (if (= start end)
      nil
      {:start start
       :end   end})))

#_(s/defn adjust-against :- (s/maybe Region)
  "Algorithm for figuring out new region.
   Given
      Cc = current cursor
      Cd = displaced cursor
      Cs = region start cursor
      Ce = region end cursor

   a. Determine which region cursor (Cs or Ce) is going to move and be replaced by the displaced cursor (Cd).
      The coordinate (Cm) closest to the current cursor (Cc) is the one move:

        Cm = min (abs (distance (Cs, Cc)), abs(distance (Ce, Cc)))

   b. Determine which coordinate hasn't moved.
      The remainder coordinate (Ck) after subtracting the moved coordinate (Cm) from the region coordinate set:

        Ck = {Cs, Ce} - Cr

   c. Determine if the cursor moved backwards or forwards.
      The sign of the distance (dm) between the moved coordinate (Cm) and the displaced coordinate (Cd):

        dm = distance (Cm, Cd) { left, if dm < 0
                               { right, if dm > 0

   d. Determine if the moved coordinate exceeded the unmoved coordinate.
      The sign of the distance (dkm) between the unmoved coordinate (Ck) and the displaced coordinate (Cd):

        dmk = distance (Ck, Cd) { true, if dm < 0 && Cm = Cs
                                { true, if dm > 0 && Cm = Ce

   e. Determine new region area.

        if Cm = Cs && dm > 0 && dmk < 0               ; start overlapped end
           let start = Ce
               end   = move-left Cd
               if start = end                         ; start cancelled-out end
                  nil
                  {:start start :end end}

        if Cm = Cs && dm > 0                          ; start going towards end (shrinking)
           {:start Cd :end Ce}

        if Cm = Cs && dm < 0                          ; start going away from end (increasing)
           {:start Cd :end Ce}

        if Cm = Ce && dm <= 0 && dmk > 0              ; end overlapped start
           let start = Cd
               end   = move-left Cs
               if start = end                         ; end cancelled-out start
                  nil
                  {:start start :end end}

        if Cm = Ce && dm <= 0                         ; end going towards start (shrinking)
           {:start Cs :end (move-left Cd)}

        if Cm = Ce && dm > 0                          ; end going away from start (increasing)
           {:start Cs :end (move-left Cd)}

        else nil"

  [text :- Seeker
   displaced :- Seeker]
  (let [Cc (:cursor text)
        Cd (:cursor displaced)
        Cs (-> text (:selection) (:start))
        Ce (-> text (:selection) (:end))
        Cm (min-key #(Math/abs ^long (distance % Cc text)) Cs Ce)
        Ck (if (= Cm Cs) Ce Cs)
        dm (distance Cm Cd text)
        dk (distance Cd Ck text)
        di (distance Cs Cd text)
        M  (cond
             (and (= Cs Ce) (=> di 0)) :end
             (and (= Cs Ce) (< di 0)) :start
             (= Cm Cs)                :start
             (= Cm Ce)                :end)]
    (cond
      (and (= M :start) (> dm 0) (= dk 0))
      {:start Cd :end Cd}

      (and (= M :start) (> dm 0) (< dk 0))
      (let [Cs' (-> text (reset-to Ce) (move-right) (:cursor))]
        (if (= Cs' Cd)
          nil
          {:start Cs'
           :end   (-> text (reset-to Cd) (move-left) (:cursor))}))

      (and (= M :start) (> dm 0))
      {:start Cd :end Ce}

      (and (= M :start) (< dm 0))
      {:start Cd :end Ce}

      (and (= M :end) (<= dm 0) (>= dk 0))
      (let [Ce' (-> text (reset-to Cs) (move-left) (:cursor))]
        (if (= Cs Cd)
          nil
          {:start Cd
           :end   Ce'}))

      (and (= M :end) (<= dm 0))
      {:start Cs :end (-> text (reset-to Cd) (move-left) (:cursor))}

      (and (= M :end) (> dm 0))
      (let [r (distance Cc Ce text)]
        (if (= r 0)
          nil
          {:start Cs :end (-> text (reset-to Cd) (move-left) (:cursor))}))

      :else {:start Cs :end Ce})))

#_(s/defn initial-region :- (s/maybe Region)
  [text :- Seeker
   displaced :- Seeker]
  (let [Cc (:cursor text)
        Cd (:cursor displaced)]
    (when (not= Cc Cd)
      (let [[start end] (sort-by (juxt second first) [Cc Cd])]
        {:start start
         :end   (-> text (reset-to end) (move-left) (:cursor))}))))

(comment
  "If I can accurately figure out which end moved and in which direction. In ALL circumstances, then I can
  fairly simply figure out how to replace it.

  I've said previously that the coordinate nearest the cursor is the one to move.
  Previously, this approach had a couple of flaws given that the start and the end
  could, at some point, be the exact same coordinate and represent a valid region.
  Now, start and end can never be equal. If they are, then that region gets cancelled out.
  So theoretically, the assumption made above, namely that the nearest one is going to move, might hold
  for everything")
(s/defn select-with :- Seeker
  [seeker :- Seeker
   f :- (=> Seeker Seeker)]
  (let [seeker' (f seeker)]
    (if (selecting? seeker)
      (reset-selection seeker' (adjust-against seeker seeker'))
      (reset-selection seeker' (initial-region seeker seeker')))))

(s/defn select-right :- Seeker
  [seeker :- Seeker]
  (select-with seeker move-right))

(s/defn select-left :- Seeker
  [seeker :- Seeker]
  (select-with seeker move-left))

(s/defn select-up :- Seeker
  [seeker :- Seeker]
  (select-with seeker move-up))

(s/defn select-down :- Seeker
  [seeker :- Seeker]
  (select-with seeker move-down))

(s/defn select-jump-right :- Seeker
  [seeker :- Seeker]
  (select-with seeker jump-right))

(s/defn select-jump-left :- Seeker
  [seeker :- Seeker]
  (select-with seeker jump-left))

(s/defn deselect :- Seeker
  [seeker :- Seeker]
  (-> seeker
      (reset-selection nil)
      (reset-expansion :word)))

(s/defn append :- Seeker
  [seeker :- Seeker, & seekers :- [Seeker]]
  (-> (fn [this that]
        (rebase this #(concat % (:lines that))))
      (reduce seeker seekers)))

(s/defn join :- Seeker
  [this-seeker :- Seeker
   that-seeker :- Seeker]
  (let [ths       (:size this-seeker)
        move      (fn [[x y]] [x (+ y ths)])
        cursor    (:cursor that-seeker)
        selection (:selection that-seeker)]
    (-> (append this-seeker that-seeker)
        (reset-to (move cursor))
        (reset-selection (when-let [{start :start end :end} selection]
                           {:start (move start)
                            :end   (move end)})))))

(s/defn join-many :- Seeker
  [seeker :- Seeker, & seekers :- [Seeker]]
  (reduce join seeker seekers))

(s/defn joined :- Seeker
  [seekers :- [Seeker]]
  (reduce join empty-seeker seekers))

(s/defn merge-lines :- Seeker
  [seeker :- Seeker]
  (peer seeker (fn [l [a b & t]]
                 (-> (conj l (concat a b))
                     (concat t)))))

(s/defn new-line :- Seeker
  [seeker :- Seeker]
  (-> seeker
      (split vector)
      (move-y inc)
      (start-x)))

(s/defn simple-delete :- Seeker
        [seeker :- Seeker]
        (-> seeker
            (slicel drop-last)
            (move-left-with merge-lines)))

(s/defn pair-delete :- Seeker
  [seeker :- Seeker]
  (-> seeker
      (slice #(concat (drop-last %1) (rest %2)))
      (move-x dec)))

;; FIXME: Don't use simple delete, do a dedicated transformation on the lines
(s/defn chunk-delete :- Seeker
  [seeker :- Seeker]
  (let [start     (-> seeker (:selection) (:start))
        end       (-> seeker (:selection) (:end))]
    (-> seeker
        (reset-to end)
        (move-right)
        (do-until simple-delete #(= (:cursor %) start)))))

(s/defn pair? :- s/Bool
  [seeker :- Seeker]
  (m/match [(previous-char seeker) (current-char seeker)]
           [\( \)] true
           [\[ \]] true
           [\{ \}] true
           [\" \"] true
           :else false))

(s/defn at-end? :- s/Bool
  [seeker :- Seeker]
  (let [[x y]     (:cursor seeker)
        text-size (:size seeker)
        line-size (-> seeker (current-line) (count))]
    (and (= x line-size) (= y (dec text-size)))))

(s/defn delete-previous :- Seeker
  [seeker :- Seeker]
  (cond
    (selecting? seeker) (chunk-delete seeker)
    (pair? seeker) (pair-delete seeker)
    (paired-tokens (previous-char seeker)) (move-left seeker)
    :else (simple-delete seeker)))

(s/defn delete-current :- Seeker
  [seeker :- Seeker]
  (cond
    (selecting? seeker) (chunk-delete seeker)
    (pair? seeker) (-> seeker (pair-delete) (move-left))
    (paired-tokens (current-char seeker)) seeker
    (at-end? seeker) seeker
    :else (-> seeker (move-right) (simple-delete))))

(s/defn simple-insert :- Seeker
  [seeker :- Seeker
   value  :- Character]
  (-> seeker (slicel #(conj % value)) (move-x inc)))

(s/defn pair-insert :- Seeker
  [seeker :- Seeker
   [l r]  :- [Character]]
  (-> seeker (slicel #(conj % l r)) (move-x inc)))

(s/defn overwrite :- Seeker
  [seeker :- Seeker]
  (if (selecting? seeker)
    (delete-previous seeker)
    seeker))

(s/defn insert :- Seeker
  [seeker :- Seeker
   input  :- Character]
  (let [overwritten (overwrite seeker)]
    (m/match [input (current-char overwritten)]
             [\) \)]    (move-x seeker inc)
             [\] \]]    (move-x seeker inc)
             [\} \}]    (move-x seeker inc)
             [\" \"]    (move-x seeker inc)
             [\( _]     (pair-insert seeker [\( \)])
             [\[ _]     (pair-insert seeker [\[ \]])
             [\{ _]     (pair-insert seeker [\{ \}])
             [\) _]     (pair-insert seeker [\( \)])
             [\] _]     (pair-insert seeker [\[ \]])
             [\} _]     (pair-insert seeker [\{ \}])
             [\" _]     (pair-insert seeker [\" \"])
             [\space _] (simple-insert seeker input)
             :else      (simple-insert overwritten input))))

(s/defn extract :- (s/maybe Seeker)
  [seeker :- Seeker]
  (when (selecting? seeker)
    (let [[xs ys] (-> seeker (:selection) (:start))
          [xe ye] (-> seeker (:selection) (:end))]
      (-> seeker
          (reset-selection nil)
          (rebase (fn [lines]
                    (->> lines (take (inc ye)) (drop ys))))
          (end)
          (enrich (fn [line] [(take xe line)]))
          (start)
          (switch (fn [line]
                    (let [entire-line?        (= xs (count line))
                          start-of-next-line? (and (= xe 0) (= (- ye ys 1)))]
                      (if (and entire-line? start-of-next-line?)
                        nil
                        (drop xs line)))))))))

(s/defn copy :- Seeker
  [seeker :- Seeker]
  (if (selecting? seeker)
    (->> seeker (extract) (reset-clipboard seeker))
    seeker))

(s/defn cut :- Seeker
  [seeker :- Seeker]
  (if (selecting? seeker)
    (-> seeker (copy) (delete-previous))
    seeker))

(s/defn paste :- Seeker
  [seeker :- Seeker]
  (let [copied (some-> seeker (:clipboard) (end))
        [x y]  (some-> copied (:cursor))]
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

(s/defn select-all :- Seeker
  [seeker :- Seeker]
  (-> seeker
      (reset-selection {:start [0 0]
                        :end   (-> seeker (end) (move-left) (:cursor))})
      (end)))

(defn- pairs? [this that]
  (or (= (get open-pairs this :none) that)
      (= (get closed-pairs this :none) that)))

(s/defn open-paren-match :- (s/maybe Region)
  [seeker :- Seeker]
  "This assumes that the cursor is currently facing an open paren, i.e: (current-char seeker) = open-paren
   Moves forward to match the open paren: | <-> (func.."
  (let [init-char       (current-char seeker)
        init-cursor     (:cursor seeker)
        text-end-cursor (:cursor (end seeker))]
    (loop [open-parens 1
           current     (move-right seeker)]
      (let [char (current-char current)]
        (cond
          (zero? open-parens)                    {:start init-cursor :end (:cursor current)}
          (pairs? init-char char)                (recur (dec open-parens) (move-right current))
          (= text-end-cursor (:cursor current))  nil
          (= init-char char)                     (recur (inc open-parens) (move-right current))
          :else                                  (recur open-parens (move-right current)))))))

(s/defn closed-paren-match :- (s/maybe Region)
  [seeker :- Seeker]
  "This assumes that the cursor is behind  a closed paren, i.e: (current-char seeker) = closed-paren
   Moves backward to match the closing paren: ..on | <-> )"
  (let [init-char         (current-char seeker)
        init-cursor       (:cursor seeker)
        text-start-cursor (:cursor (start seeker))]
    (loop [closed-parens 1
           end-cursor    nil
           current       (move-left seeker)]
      (let [char (current-char current)]
        (cond
          (zero? closed-parens)                   {:start end-cursor :end init-cursor}
          (pairs? init-char char)                 (recur (dec closed-parens) (:cursor current) (move-left current))
          (= text-start-cursor (:cursor current)) nil
          (= init-char char)                      (recur (inc closed-parens) nil (move-left current))
          :else                                   (recur closed-parens nil (move-left current)))))))

(s/defn find-pair :- (s/maybe Region)
  [seeker :- Seeker]
  (cond
    (contains? open-pairs (current-char seeker))    (-> seeker (open-paren-match))
    (contains? open-pairs (previous-char seeker))   (-> seeker (move-left) (open-paren-match))
    (contains? closed-pairs (current-char seeker))  (-> seeker (closed-paren-match))
    (contains? closed-pairs (previous-char seeker)) (-> seeker (move-left) (closed-paren-match))
    :else                                           nil))

(s/defn word-expansion-right :- Region
  "This assumes the cursor is after an open paren.
   Jumps forward to encapsulate the word it was facing: ( <- | -> func"
  [seeker :- Seeker]
  {:start (:cursor seeker)
   :end   (:cursor (jump-right seeker))})

(s/defn word-expansion-left :- Region
  "This assumes the cursor is facing a closed paren.
  Jumps backward to encapsulate the word it was back to: tion <- | -> )"
  [seeker :- Seeker]
  (-> seeker (jump-left) (word-expansion-right)))

(s/defn free-expansion :- (s/maybe Region)
  [seeker :- Seeker]
  "This assumes that the cursor isn't neighbouring any parens.
   It moves backwards until it finds an open parens and proceeds with an opened-parens-expansion."
  (let [init-cursor (:cursor (start seeker))
        expand-from (-> seeker (:selection) (:start) (or (:cursor seeker)))]
    (loop [seen-chars ()
           current    (reset-to seeker expand-from)]
      (let [char      (previous-char current)
            last-seen (first seen-chars)]
        (cond
          (and (empty? seen-chars)
               (contains? open-pairs char))   (-> current (move-left) (open-paren-match))
          (pairs? char last-seen)             (recur (rest seen-chars) (move-left current))
          (= init-cursor (:cursor current))   nil
          (contains? closed-pairs char)       (recur (cons char seen-chars) (move-left current))
          :else                               (recur seen-chars (move-left current)))))))

(s/defn derive-expansion :- (s/maybe Region)
  [seeker :- Seeker]
  (let [expansion (:expansion seeker)
        previous (previous-char seeker)
        current  (current-char seeker)]
    (m/match [expansion previous current]
             [:word \( \)]                          (free-expansion seeker)
             [:word \[ \]]                          (free-expansion seeker)
             [:word \{ \}]                          (free-expansion seeker)
             [:word _ \space]                       (free-expansion seeker)
             [:word (:or \" \space) (:or \) \] \})] (closed-paren-match (move-right seeker))
             [:word (:or \) \] \}) _]               (closed-paren-match seeker)
             [(:or :word :expr) _ (:or \( \[ \{)]   (open-paren-match seeker)
             [:word (:or \( \[ \{ \" \space nil) _] (word-expansion-right seeker)
             [:word _ _]                            (word-expansion-left seeker)
             :else                                  (free-expansion seeker))))

(s/defn expand :- Seeker
  [seeker :- Seeker]
  (let [expansion (if-let [expansion (derive-expansion seeker)]
                    expansion
                    {:start (:cursor (start seeker))
                     :end   (:cursor (end seeker))})]
    (-> seeker
        (reset-selection expansion)
        (reset-expansion :expr))))

(s/defn forget-everything :- Seeker
  [seeker :- Seeker]
  (reset-history seeker '() '()))

(s/defn remember :- Seeker
  [seeker :- Seeker]
  (let [history (:history seeker)]
    (->> history
         (cons (forget-everything seeker))
         (take 50)
         (assoc seeker :history))))

(s/defn undo :- Seeker
  [seeker :- Seeker]
  (let [history   (:history seeker)
        rhistory  (:rhistory seeker)
        clipboard (:clipboard seeker)]
    (if (empty? history)
      seeker
      (-> history
          (first)
          (assoc :clipboard clipboard)
          (assoc :history (rest history))
          (assoc :rhistory (-> seeker (forget-everything) (cons rhistory)))))))

(s/defn redo [seeker :- Seeker]
  (let [history   (:history seeker)
        rhistory  (:rhistory seeker)
        clipboard (:clipboard seeker)]
    (if (empty? rhistory)
      seeker
      (-> rhistory
          (first)
          (assoc :clipboard clipboard)
          (assoc :rhistory (rest rhistory))
          (assoc :history (-> seeker (forget-everything) (cons history)))))))

(s/defn auto-complete :- Seeker
  [seeker :- Seeker, input :- [Character]]
  (if (empty? input)
    seeker
    (-> seeker
        (expand)
        (delete-previous)
        (slicer #(concat input %))
        (move-x #(+ % (count input))))))

(s/defn stringify :- s/Str
  [seeker :- Seeker]
  (->> seeker
       (:lines)
       (mapv #(apply str %))
       (string/join "\n")))

(s/defn debug-string :- String
  [seeker :- Seeker]
  (-> seeker
      (slice (fn [l r] (vec (concat l "|" r))))
      (stringify)))

(s/defn printed :- nil
  [seeker :- Seeker]
  (println (debug-string seeker)))

(s/defn indent :- Seeker
  [seeker :- Seeker
   amount :- s/Int]
  (let [padding (repeat amount \space)]
    (rebase seeker (fn [lines] (mapv #(vec (concat padding %)) lines)))))

(s/defn equivalent? :- s/Bool
  [this :- Seeker
   that :- Seeker]
  (= (:lines this) (:lines that)))

(s/defn process :- Seeker
  [seeker :- Seeker
   event :- e/TextEvent]
  (condp = (:action event)
    e/expand            (-> seeker (expand))
    e/select-all        (-> seeker (select-all))
    e/copy              (-> seeker (copy) (deselect))
    e/cut               (-> seeker (remember) (cut) (deselect))
    e/paste             (-> seeker (remember) (paste) (deselect))
    e/up                (-> seeker (move-up) (deselect))
    e/down              (-> seeker (move-down) (deselect))
    e/left              (-> seeker (move-left) (deselect))
    e/right             (-> seeker (move-right) (deselect))
    e/jump-left         (-> seeker (jump-left) (deselect))
    e/jump-right        (-> seeker (jump-right) (deselect))
    e/select-up         (-> seeker (select-up))
    e/select-down       (-> seeker (select-down))
    e/select-left       (-> seeker (select-left))
    e/select-right      (-> seeker (select-right))
    e/select-jump-left  (-> seeker (select-jump-left))
    e/select-jump-right (-> seeker (select-jump-right))
    e/backspace         (-> seeker (remember) (delete-previous) (deselect))
    e/delete            (-> seeker (remember) (delete-current) (deselect))
    e/break             (-> seeker (remember) (new-line) (deselect))
    e/undo              (-> seeker (undo) (deselect))
    e/redo              (-> seeker (redo) (deselect))
    e/character         (-> seeker (remember) (insert (:value event)) (deselect))
    seeker))