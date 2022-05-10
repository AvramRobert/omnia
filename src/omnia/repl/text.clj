(ns omnia.repl.text
  (:require [schema.core :as s]
            [clojure.core.match :as m]
            [clojure.string :as string]
            [clojure.set :refer [union map-invert]]
            [omnia.schema.text :refer [Text Line Expansion]]
            [omnia.schema.common :refer [=> Point Region Pair]]
            [omnia.util.collection :refer [do-until dissoc-idx]]))

(def empty-text
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

(s/defn resize :- Text
  [text :- Text]
  (assoc text :size (-> text :lines count)))

(s/defn create-text :- Text
  ([lines :- [Line]]
   (create-text lines [0 0]))
  ([lines :- [Line]
    cursor :- Point]
   (-> empty-text (assoc :lines (vec lines) :cursor cursor) (resize))))

(s/def empty-line :- Text
  (create-text [[]]))

(s/defn from-string :- Text
  "Examples:
    1. 'ab\nc' => [[a b] [c]]
    2. '1\n\n' => [[a] []];
    3. '\n\n' => [[] []]"
  [input :- s/Str]
  (loop [lines []
         rem   input]
    (if (empty? rem)
      (create-text lines)
      (recur (->> rem
                  (take-while #(not= % \newline))
                  (vec)
                  (conj lines))
             (->> rem
                  (drop-while #(not= % \newline))
                  (drop 1))))))

(s/defn space? :- s/Bool
  [character :- Character]
  (= \space character))

(s/defn rebase :- Text
  [text :- Text f]
  (-> text (update :lines (comp vec f)) (resize)))

(s/defn line-at :- Line
  [text :- Text, y :- s/Int]
  (-> text (:lines) (nth y [])))

(s/defn current-line :- Line
  [text :- Text]
  (let [y (-> text (:cursor) (nth 1))]
    (line-at text y)))

(s/defn char-at :- (s/maybe Character)
  [text :- Text
   [x y] :- Point]
  (-> text (line-at y) (nth x nil)))

(s/defn peer :- Text
  "Looks between the lines specified by the cursor's `y` coordinate.
   Applies a binary function `f`.
   left-hand side = the lines that came before the y'th coordinate
   right-hand side = the lines that came after the y'th coordinate (including it)

   `f` is expected to return valid `lines` of text.
   These then replace the initial lines on the text."
  [text :- Text
   f :- (=> [Line] [Line] [Line])]
  (let [[_ y] (:cursor text)]
    (rebase text #(->> (split-at y %)
                       (mapv vec)
                       (apply f)
                       (mapv vec)))))

(s/defn split :- Text
  "Looks at the line where the cursor is currently placed.
   Applies a binary function `f`.
   left-hand side = the characters that come before the cursor
   right-hand side = the characters that come after the cursor

   `f` is expected to return valid `lines` of text.
   These then replace the one line on which `f` was applied and get merged with the rest."
  [text :- Text
   f :- (=> Line Line [Line])]
  (let [[x _] (:cursor text)]
    (peer text (fn [l [line & r]]
                 (let [lines (->> line (split-at x) (mapv vec) (apply f))]
                   (concat l lines r))))))

(s/defn enrich :- Text
  "Looks at the current line. Applies a function `f` on it
  that should return more than one line."
  [text :- Text
   f :- (=> Line [Line])]
  (split text (fn [l r] (f (vec (concat l r))))))

(s/defn switch :- Text
  "Looks at the line where the cursor is currently.
  Applies a function `f` on that line.
  `f` is expected to return a new line of text and
  replaces the one line on which it was applied."
  [text :- Text
   f :- (=> Line (s/maybe Line))]
  (let [[_ y] (:cursor text)]
    (if-let [line' (f (current-line text))]
      (rebase text #(assoc % y (vec line')))
      (rebase text #(dissoc-idx y %)))))

(s/defn slice :- Text
  "Looks at the line where the cursor is currently placed.
   Applies a binary function `f`.
   left-hand side = the characters that come before the cursor
   right-hand side = the characters that come after the cursor

   `f` is expected to return one valid line of text.
   This then replaces the line on which `f` was applied."
  [text :- Text
   f :- (=> Line Line Line)]
  (split text (fn [l r] [(f l r)])))

(s/defn slicel :- Text
  [text :- Text
   f :- (=> Line Line)]
  (slice text (fn [l r] (concat (f l) r))))

(s/defn slicer :- Text
  [text :- Text
   f :- (=> Line Line)]
  (slice text (fn [l r] (concat l (f r)))))

;; FIXME: There should be only 1 `reset-cursor` function
;; That function should be use in every `move-*`
;; No more higher order functions: as much as we can
;; The reset function should check bounds.
;; THATS THE ONLY THING THAT SHOULD
(s/defn move :- Text
  [text :- Text
   f :- (=> Point Point)]
  (update text :cursor f))

(s/defn reset-to :- Text
  [text :- Text
   cursor :- Point]
  (move text (constantly cursor)))

(s/defn move-x :- Text
  [text :- Text
   f :- (=> s/Int s/Int)]
  (move text
        (fn [[x y]]
          (let [length (-> text current-line count)
                nx     (f x)]
            (if (<= 0 nx length)
              [nx y]
              [x y])))))

(s/defn move-y :- Text
  [text :- Text
   f :- (=> s/Int s/Int)]
  (move text
        (fn [[x y]]
          (let [height (:size text)
                ny     (f y)]
            (if (<= 0 ny (dec height))
              [x ny]
              [x y])))))

(s/defn reset-x :- Text
  [text :- Text
   value :- s/Int]
  (move-x text (fn [_] value)))

(s/defn reset-y :- Text
  [text :- Text
   value :- s/Int]
  (move-y text (fn [_] value)))

(s/defn end-x :- Text
  [text :- Text]
  (move text (fn [[_ y]] [(-> text current-line count) y])))

(s/defn start-x :- Text
  [text :- Text]
  (reset-x text 0))

(s/defn start-y :- Text
  [text :- Text]
  (reset-y text 0))

(s/defn end-y :- Text
  [text :- Text]
  (let [height (:size text)
        y-max  (if (zero? height) 0 (dec height))]
    (move text (fn [[x _]] [x y-max]))))

(s/defn start :- Text
  [text :- Text]
  (-> text (start-y) (start-x)))

(s/defn end :- Text
  [text :- Text]
  (-> text (end-y) (end-x)))

(s/defn move-right-with :- Text
  [text :- Text
   f :- (=> Text Text)]
  (let [h (-> text :size dec)
        w (-> text current-line count)]
    (m/match [(:cursor text)]
             [[w h]] text
             [[w _]] (-> text (move-y inc) (start-x) (f))
             :else (move-x text inc))))

(s/defn move-left-with :- Text
  [text :- Text
   f :- (=> Text Text)]
  (m/match [(:cursor text)]
           [[0 0]] text
           [[0 _]] (-> text (move-y dec) (end-x) (f))
           :else (move-x text dec)))

(s/defn do-move-left :- Text
  [text :- Text]
  (move-left-with text identity))

(s/defn do-move-right :- Text
  [text :- Text]
  (move-right-with text identity))

(s/defn do-move-up :- Text
  [text :- Text]
  (let [[x y] (:cursor text)
        y' (dec y)]
    (if (< y' 0)
      text
      (let [x' (-> text (line-at y') (count))]
        (if (> x x')
          (reset-to text [x' y'])
          (reset-to text [x y']))))))

(s/defn do-move-down :- Text
  [text :- Text]
  (let [[x y] (:cursor text)
        y'   (inc y)
        size (:size text)]
    (if (>= y' size)
      text
      (let [x' (-> text (line-at y') (count))]
        (if (> x x')
          (reset-to text [x' y'])
          (reset-to text [x y']))))))

(s/defn current-char :- (s/maybe Character)
  [text :- Text]
  (char-at text (:cursor text)))

(s/defn previous-char :- (s/maybe Character)
  [text :- Text]
  (when (not= [0 0] (:cursor text))
    (-> text (do-move-left) (current-char))))

(s/defn next-char :- (s/maybe Character)
  [text :- Text]
  (let [next (do-move-right text)]
    (when (not= (:cursor text) (:cursor next))
      (current-char next))))

(s/defn jump :- Text
  [text :- Text
   move :- (=> Text Text)
   look :- (=> Text (s/maybe Character))]
  (letfn [(blank? [s] (some-> s (look) (space?)))
          (literal? [s] (not (blank? s)))
          (paired-token? [s] (paired-tokens (look s)))
          (bound? [s] (nil? (look s)))]
    (cond
      (bound? text) (-> text (do-until move #(or (bound? %) (paired-token? %) (literal? %))))
      (blank? text) (-> text (do-until move #(or (bound? %) (paired-token? %) (literal? %))))
      (paired-token? text) (-> text (do-until move #(or (bound? %) (literal? %))))
      :else (-> text (do-until move #(or (bound? %) (paired-token? %) (blank? %)))))))

(s/defn do-jump-left :- Text
  [text :- Text]
  (jump text do-move-left previous-char))

(s/defn do-jump-right :- Text
  [text :- Text]
  (jump text do-move-right current-char))

(s/defn reset-expansion :- Text
  [text :- Text
   scope :- Expansion]
  (assoc text :expansion scope))

(s/defn selecting? :- s/Bool
  [text :- Text]
  (-> text :selection some?))

(s/defn reset-selection :- Text
  [text :- Text
   region :- (s/maybe Region)]
  (assoc text :selection region))

(s/defn reset-clipboard :- Text
  [text :- Text
   content :- (s/maybe Text)]
  (assoc text :clipboard content))

(s/defn reset-history :- Text
  [text :- Text
   undo-history :- [Text]
   redo-history :- [Text]]
  (assoc text :history undo-history :rhistory redo-history))

(s/defn distance :- s/Int
  "Amount of characters between two points in the text space."
  [[xs ys] :- Point
   [xe ye] :- Point
   text :- Text]
  (letfn [(chars-between [[xs ys] [xe ye]]
            (let [lower     xe
                  upper     (-> text (line-at ys) (count) (- xs))
                  middle    (->> (range (inc ys) ye)
                                 (reduce (fn [sum y] (-> text (line-at y) (count) (+ sum))) 0))
                  new-lines (- ye ys)]
              (+ upper lower middle new-lines)))]
    (cond
      (= ys ye) (Math/abs ^long (- xe xs))
      (> ys ye) (chars-between [xe ye] [xs ys])
      :else (chars-between [xs ys] [xe ye]))))

(s/defn select-with :- Text
  [text :- Text
   f    :- (=> Text Text)]
  "Algorithm for figuring out new region.
   Given
      Cc = current cursor
      Cd = displaced cursor

   If no selection already present:
      Ck = Cc
      Move to c)

   Else:
    let
        Cs = region start cursor
        Ce = region end cursor

    a. Determine which region cursor (Cs or Ce) is going to move and be replaced by the displaced cursor (Cd).
       The coordinate (Cm) closest to the current cursor (Cc) is the one move:

         Cm = min (abs (distance (Cs, Cc)), abs(distance (Ce, Cc)))

    b. Determine which coordinate hasn't moved.
       The remainder coordinate (Ck) after subtracting the moved coordinate (Cm) from the region coordinate set:

         Ck = {Cs, Ce} - Cr

    c. Determine new region area.
       Sort the remainder coordinate (Ck) and the displaced coordinate (Cd) ascendingly.
       The lower coordinate becomes start and the other the end. Equality implies a cancelled out-region:

         { start, end } = sort (Cd, Ck) { nil, iff start = end
                                        { {start, end}, iff start =/ end"
  (let [text'     (f text)
        Cc        (:cursor text)
        Cd        (:cursor text')
        Ck        (if (selecting? text)
                    (let [Cs (-> text (:selection) (:start))
                          Ce (-> text (:selection) (:end))
                          Cm (min-key #(distance % Cc text) Cs Ce)]
                      (if (= Cm Cs) Ce Cs))
                    Cc)
        [start end] (sort-by (juxt second first) [Cd Ck])
        selection (if (= start end)
                    nil
                    {:start start
                     :end   end})]
    (reset-selection text' selection)))

(s/defn select-right :- Text
  [text :- Text]
  (select-with text do-move-right))

(s/defn select-left :- Text
  [text :- Text]
  (select-with text do-move-left))

(s/defn select-up :- Text
  [text :- Text]
  (select-with text do-move-up))

(s/defn select-down :- Text
  [text :- Text]
  (select-with text do-move-down))

(s/defn do-jump-select-right :- Text
  [text :- Text]
  (select-with text do-jump-right))

(s/defn do-jump-select-left :- Text
  [text :- Text]
  (select-with text do-jump-left))

(s/defn deselect :- Text
  [text :- Text]
  (-> text
      (reset-selection nil)
      (reset-expansion :word)))

(s/defn append :- Text
  [text :- Text, & texts :- [Text]]
  (-> (fn [this that]
        (rebase this #(concat % (:lines that))))
      (reduce text texts)))

(s/defn join :- Text
  [this-text :- Text
   that-text :- Text]
  (let [ths       (:size this-text)
        move      (fn [[x y]] [x (+ y ths)])
        cursor    (:cursor that-text)
        selection (:selection that-text)]
    (-> (append this-text that-text)
        (reset-to (move cursor))
        (reset-selection (when-let [{start :start end :end} selection]
                           {:start (move start)
                            :end   (move end)})))))

(s/defn join-many :- Text
  [text :- Text, & texts :- [Text]]
  (reduce join text texts))

(s/defn joined :- Text
  [texts :- [Text]]
  (reduce join empty-text texts))

(s/defn merge-lines :- Text
  [text :- Text]
  (peer text (fn [l [a b & t]]
               (-> (conj l (concat a b))
                   (concat t)))))

(s/defn do-new-line :- Text
  [text :- Text]
  (-> text
      (split vector)
      (move-y inc)
      (start-x)))

(s/defn clean-history :- Text
  [text :- Text]
  (-> text
      (reset-history '() '())
      (reset-clipboard nil)))

(s/defn add-to-history :- Text
  [text :- Text]
  (let [history (:history text)]
    (->> history
         (cons (clean-history text))
         (take 50)
         (assoc text :history))))

(s/defn simple-delete :- Text
  [text :- Text]
  (-> text
      (slicel drop-last)
      (move-left-with merge-lines)))

(s/defn pair-delete :- Text
  [text :- Text]
  (-> text
      (slice #(concat (drop-last %1) (rest %2)))
      (move-x dec)))

;; FIXME: Don't use simple delete, do a dedicated transformation on the lines
(s/defn chunk-delete :- Text
  [text :- Text]
  (let [start (-> text (:selection) (:start))
        end   (-> text (:selection) (:end))]
    (-> text
        (reset-to end)
        (do-until simple-delete #(= (:cursor %) start)))))

(s/defn pair? :- s/Bool
  [text :- Text]
  (m/match [(previous-char text) (current-char text)]
           [\( \)] true
           [\[ \]] true
           [\{ \}] true
           [\" \"] true
           :else false))

(s/defn at-end? :- s/Bool
  [text :- Text]
  (let [[x y] (:cursor text)
        text-size (:size text)
        line-size (-> text (current-line) (count))]
    (and (= x line-size) (= y (dec text-size)))))

(s/defn do-delete-previous :- Text
  [text :- Text]
  (cond
    (selecting? text) (chunk-delete text)
    (pair? text) (pair-delete text)
    (paired-tokens (previous-char text)) (do-move-left text)
    :else (simple-delete text)))

(s/defn do-delete-current :- Text
  [text :- Text]
  (cond
    (selecting? text) (chunk-delete text)
    (pair? text) (-> text (pair-delete) (do-move-left))
    (paired-tokens (current-char text)) text
    (at-end? text) text
    :else (-> text (do-move-right) (simple-delete))))

(s/defn simple-insert :- Text
  [text :- Text
   value :- Character]
  (-> text (slicel #(conj % value)) (move-x inc)))

(s/defn pair-insert :- Text
  [text :- Text
   [l r] :- [Character]]
  (-> text (slicel #(conj % l r)) (move-x inc)))

(s/defn overwrite :- Text
  [text :- Text]
  (if (selecting? text)
    (do-delete-previous text)
    text))

(s/defn do-insert :- Text
  [text :- Text
   input :- Character]
  (let [overwritten (overwrite text)]
    (m/match [input (current-char overwritten)]
             [\) \)] (move-x text inc)
             [\] \]] (move-x text inc)
             [\} \}] (move-x text inc)
             [\" \"] (move-x text inc)
             [\( _] (pair-insert text [\( \)])
             [\[ _] (pair-insert text [\[ \]])
             [\{ _] (pair-insert text [\{ \}])
             [\) _] (pair-insert text [\( \)])
             [\] _] (pair-insert text [\[ \]])
             [\} _] (pair-insert text [\{ \}])
             [\" _] (pair-insert text [\" \"])
             [\space _] (simple-insert text input)
             :else (simple-insert overwritten input))))

(s/defn extract :- (s/maybe Text)
  [text :- Text]
  (when (selecting? text)
    (let [[xs ys] (-> text (:selection) (:start))
          [xe ye] (-> text (:selection) (:end))]
      (-> text
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

(s/defn do-copy :- Text
  [text :- Text]
  (if (selecting? text)
    (->> text (extract) (clean-history) (reset-clipboard text))
    text))

(s/defn do-cut :- Text
  [text :- Text]
  (if (selecting? text)
    (-> text (do-copy) (do-delete-previous))
    text))

(s/defn do-paste :- Text
  [text :- Text]
  (let [copied (some-> text (:clipboard) (end))
        [x y] (some-> copied (:cursor))]
    (m/match [copied]
             [{:lines [a]}] (-> (overwrite text)
                                (split #(vector (concat %1 a %2)))
                                (move-y #(+ % y))
                                (move-x #(+ % x)))
             [{:lines [a b]}] (-> (overwrite text)
                                  (split #(vector (concat %1 a) (concat b %2)))
                                  (move-y #(+ % y))
                                  (reset-x x))
             [{:lines [a & b]}] (-> (overwrite text)
                                    (split #(concat [(concat %1 a)]
                                                    (drop-last b)
                                                    [(concat (last b) %2)]))
                                    (move-y #(+ % y))
                                    (reset-x x))
             :else text)))

(s/defn do-select-all :- Text
  [text :- Text]
  (-> text
      (reset-selection {:start [0 0]
                        :end   (-> text (end) (:cursor))})
      (end)))

(defn- pairs? [this that]
  (or (= (get open-pairs this :none) that)
      (= (get closed-pairs this :none) that)))

(s/defn open-paren-expansion :- (s/maybe Region)
  [text :- Text]
  "This assumes that the cursor is currently facing an open paren, i.e: (current-char text) = open-paren
   Moves forward to match the open paren: | <-> (func.."
  (let [init-char       (current-char text)
        init-cursor     (:cursor text)
        text-end-cursor (:cursor (end text))]
    (loop [open-parens 1
           current     (do-move-right text)]
      (let [char (current-char current)]
        (cond
          (zero? open-parens) {:start init-cursor :end (:cursor current)}
          (pairs? init-char char) (recur (dec open-parens) (do-move-right current))
          (= text-end-cursor (:cursor current)) nil
          (= init-char char) (recur (inc open-parens) (do-move-right current))
          :else (recur open-parens (do-move-right current)))))))

(s/defn closed-paren-expansion :- (s/maybe Region)
  [text :- Text]
  "This assumes that the cursor is behind a closed paren (i.e: |<->),  (current-char text) = closed-paren)
   Moves backward to match the closing paren"
  (let [init-char         (current-char text)
        init-cursor       (-> text (do-move-right) (:cursor))
        text-start-cursor (:cursor (start text))]
    (loop [closed-parens 1
           end-cursor    nil
           current       (do-move-left text)]
      (let [char (current-char current)]
        (cond
          (zero? closed-parens) {:start end-cursor :end init-cursor}
          (pairs? init-char char) (recur (dec closed-parens) (:cursor current) (do-move-left current))
          (= text-start-cursor (:cursor current)) nil
          (= init-char char) (recur (inc closed-parens) nil (do-move-left current))
          :else (recur closed-parens nil (do-move-left current)))))))

(s/defn free-expansion :- (s/maybe Region)
  [text :- Text]
  "This assumes that the cursor isn't neighbouring any parens.
   It moves backwards until it finds an open parens and proceeds with an opened-parens-expansion."
  (let [init-cursor (:cursor (start text))
        expand-from (-> text (:selection) (:start) (or (:cursor text)))]
    (loop [seen-chars ()
           current    (reset-to text expand-from)]
      (let [char      (previous-char current)
            last-seen (first seen-chars)]
        (cond
          (and (empty? seen-chars)
               (contains? open-pairs char)) (-> current (do-move-left) (open-paren-expansion))
          (pairs? char last-seen) (recur (rest seen-chars) (do-move-left current))
          (= init-cursor (:cursor current)) nil
          (contains? closed-pairs char) (recur (cons char seen-chars) (do-move-left current))
          :else (recur seen-chars (do-move-left current)))))))

(s/defn word-expansion-right :- Region
  [text :- Text]
  {:start (:cursor text)
   :end   (:cursor (do-jump-right text))})

(s/defn word-expansion-left :- Region
  [text :- Text]
  (-> text (do-jump-left) (word-expansion-right)))

(s/defn derive-expansion :- (s/maybe Region)
  [text :- Text]
  (let [expansion (:expansion text)
        previous  (previous-char text)
        current   (current-char text)]
    (m/match [expansion previous current]
             [:word \( \)] (free-expansion text)
             [:word \[ \]] (free-expansion text)
             [:word \{ \}] (free-expansion text)
             [:word _ \space] (free-expansion text)
             [:word (:or \" \space) (:or \) \] \})] (closed-paren-expansion text)
             [:word (:or \) \] \}) _] (closed-paren-expansion (do-move-left text))
             [(:or :word :expr) _ (:or \( \[ \{)] (open-paren-expansion text)
             [:word (:or \( \[ \{ \" \space nil) _] (word-expansion-right text)
             [:word _ _] (word-expansion-left text)
             :else (free-expansion text))))

(s/defn find-pair :- (s/maybe Pair)
  [text :- Text]
  (letfn [(to-pair [region]
            (let [[xs ys] (:start region)
                  [xe ye] (:end region)]
              {:left  {:start [xs ys]
                       :end   [(inc xs) ys]}
               :right {:start [(dec xe) ye]
                       :end   [xe ye]}}))]
    (cond
      (contains? open-pairs (current-char text)) (some-> text (open-paren-expansion) (to-pair))
      (contains? open-pairs (previous-char text)) (some-> text (do-move-left) (open-paren-expansion) (to-pair))
      (contains? closed-pairs (current-char text)) (some-> text (closed-paren-expansion) (to-pair))
      (contains? closed-pairs (previous-char text)) (some-> text (do-move-left) (closed-paren-expansion) (to-pair))
      :else nil)))

(s/defn do-expand-select :- Text
  [text :- Text]
  (let [expansion (if-let [expansion (derive-expansion text)]
                    expansion
                    {:start (:cursor (start text))
                     :end   (:cursor (end text))})]
    (-> text
        (reset-selection expansion)
        (reset-expansion :expr))))

(s/defn do-undo :- Text
  [text :- Text]
  (let [history   (:history text)
        rhistory  (:rhistory text)
        clipboard (:clipboard text)]
    (if (empty? history)
      text
      (-> history
          (first)
          (assoc :clipboard clipboard)
          (assoc :history (rest history))
          (assoc :rhistory (-> text (clean-history) (cons rhistory)))))))

(s/defn do-redo [text :- Text]
  (let [history   (:history text)
        rhistory  (:rhistory text)
        clipboard (:clipboard text)]
    (if (empty? rhistory)
      text
      (-> rhistory
          (first)
          (assoc :clipboard clipboard)
          (assoc :rhistory (rest rhistory))
          (assoc :history (-> text (clean-history) (cons history)))))))

(s/defn auto-complete :- Text
  [text :- Text, input :- [Character]]
  (if (empty? input)
    text
    (-> text
        (do-expand-select)
        (do-delete-previous)
        (slicer #(concat input %))
        (move-x #(+ % (count input))))))

(s/defn as-string :- s/Str
  [text :- Text]
  (->> text
       (:lines)
       (mapv #(apply str %))
       (string/join "\n")))

(s/defn debug-string :- String
  [text :- Text]
  (-> text
      (slice (fn [l r] (vec (concat l "|" r))))
      (as-string)))

(s/defn indent :- Text
  [text :- Text
   amount :- s/Int]
  (let [padding (repeat amount \space)]
    (rebase text (fn [lines] (mapv #(vec (concat padding %)) lines)))))

(s/defn equivalent? :- s/Bool
  [this :- Text
   that :- Text]
  (= (:lines this) (:lines that)))

;; ------------ API -------------

(s/defn delete-previous :- Text
  [text :- Text]
  (-> text (add-to-history) (do-delete-previous) (deselect)))

(s/defn delete-current :- Text
  [text :- Text]
  (-> text (add-to-history) (do-delete-current) (deselect)))

(s/defn insert :- Text
  [text :- Text char :- Character]
  (-> text (add-to-history) (do-insert char) (deselect)))

(s/defn move-left :- Text
  [text :- Text]
  (-> text (do-move-left) (deselect)))

(s/defn move-right :- Text
  [text :- Text]
  (-> text (do-move-right) (deselect)))

(s/defn move-up :- Text
  [text :- Text]
  (-> text (do-move-up) (deselect)))

(s/defn move-down :- Text
  [text :- Text]
  (-> text (do-move-down) (deselect)))

(s/defn jump-left :- Text
  [text :- Text]
  (-> text (do-jump-left) (deselect)))

(s/defn jump-right :- Text
  [text :- Text]
  (-> text (do-jump-right) (deselect)))

(s/defn new-line :- Text
  [text :- Text]
  (-> text (add-to-history) (do-new-line) (deselect)))

(s/defn copy :- Text
  [text :- Text]
  (-> text (do-copy) (deselect)))

(s/defn cut :- Text
  [text :- Text]
  (-> text (add-to-history) (do-cut) (deselect)))

(s/defn paste :- Text
  [text :- Text]
  (-> text (add-to-history) (do-paste) (deselect)))

(s/defn select-all :- Text
  [text :- Text]
  (-> text (do-select-all)))

(s/defn expand-select :- Text
  [text :- Text]
  (-> text (do-expand-select)))

(s/defn jump-select-left :- Text
  [text :- Text]
  (-> text (do-jump-select-left)))

(s/defn jump-select-right :- Text
  [text :- Text]
  (-> text (do-jump-select-right)))

(s/defn undo :- Text
  [text :- Text]
  (-> text (do-undo)))

(s/defn redo :- Text
  [text :- Text]
  (-> text (do-redo)))
