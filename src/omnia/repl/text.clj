(ns omnia.repl.text
  (:require [schema.core :as s]
            [clojure.core.match :as m]
            [clojure.string :as string]
            [clojure.set :refer [union map-invert]]
            [omnia.schema.text :refer [Text Line Expansion]]
            [omnia.schema.common :refer [=> Point Region Pair]]
            [omnia.util.collection :refer [do-until dissoc-nth]]))

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

(s/defn create-text :- Text
  ([lines :- [Line]]
   (create-text lines [0 0]))
  ([lines :- [Line]
    cursor :- Point]
   (let [lines (vec lines)]
     {:lines     lines
      :cursor    cursor
      :size      (count lines)
      :expansion :word
      :clipboard nil
      :selection nil
      :history   '()
      :rhistory  '()})))

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

(s/defn line-at :- Line
  [text :- Text, y :- s/Int]
  (-> text (:lines) (nth y [])))

(s/defn reset-lines :- Text
  [text :- Text
   lines :- [Line]]
  (let [lines (vec lines)]
    (assoc text :lines lines :size (count lines))))

(s/defn reset-cursor :- Text
  [text  :- Text
   [x y] :- Point]
  (let [size   (-> text (:size) (dec))
        y'     (cond (< y 0)    0
                     (> y size) size
                     :else      y)
        length (-> text (line-at y') (count))
        x'     (cond (< x 0)      0
                     (> x length) length
                     :else        x)]
    (assoc text :cursor [x' y'])))

(s/defn reset-expansion :- Text
  [text :- Text
   scope :- Expansion]
  (assoc text :expansion scope))

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

(s/defn y :- s/Int
  [text :- Text]
  (-> text (:cursor) (nth 1)))

(s/defn x :- s/Int
  [text :- Text]
  (-> text (:cursor) (nth 0)))

(s/defn selecting? :- s/Bool
  [text :- Text]
  (-> text :selection some?))

(s/defn space? :- s/Bool
  [character :- Character]
  (= \space character))

(s/defn current-line :- Line
  [text :- Text]
  (line-at text (y text)))

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
   f    :- (=> [Line] [Line] [Line])]
  (let [[_ y] (:cursor text)
        [l r] (split-at y (:lines text))
        lines (concat (f (vec l) (vec r)))]
    (reset-lines text lines)))

(s/defn split :- Text
  "Looks at the line where the cursor is currently placed.
   Applies a binary function `f`.
   left-hand side = the characters that come before the cursor
   right-hand side = the characters that come after the cursor

   `f` is expected to return valid `lines` of text.
   These then replace the one line on which `f` was applied and get merged with the rest."
  [text :- Text
   f :- (=> Line Line [Line])]
  (let [[x y] (:cursor text)]
    (peer text (fn [l [line & r]]
                 (let [lines (->> line (split-at x) (mapv vec) (apply f))]
                   (concat l lines r))))))

(s/defn switch :- Text
  "Looks at the line where the cursor is currently.
  Applies a function `f` on that line.
  `f` can:
    - return a new line (replaces the old one)
    - return `nil` which deletes the current one"
  [text :- Text
   f :- (=> Line (s/maybe Line))]
  (let [[_ y] (:cursor text)
        lines (:lines text)]
    (if-let [line' (f (current-line text))]
      (reset-lines text (assoc lines y (vec line')))
      (reset-lines text (dissoc-nth lines y)))))

(s/defn slice :- Text
  "Looks at the line where the cursor is currently placed.
   Applies a binary function `f`.
   left-hand side = the characters that come before the cursor
   right-hand side = the characters that come after the cursor

   `f` is expected to return one valid line of text.
   This then replaces the line on which `f` was applied."
  [text :- Text
   f :- (=> Line Line Line)]
  (let [[x y] (:cursor text)
        lines (:lines text)
        line  (line-at text y)
        [l r] (split-at x line)
        line' (vec (f (vec l) (vec r)))]
    (reset-lines text (assoc lines y line'))))

(s/defn move-x :- Text
  [text :- Text
   f :- (=> s/Int s/Int)]
  (let [[x y] (:cursor text)]
    (reset-cursor text [(f x) y])))

(s/defn move-y :- Text
  [text :- Text
   f :- (=> s/Int s/Int)]
  (let [[x y] (:cursor text)]
    (reset-cursor text [x (f y)])))

(s/defn reset-x :- Text
  [text :- Text
   value :- s/Int]
  (reset-cursor text [value (y text)]))

(s/defn reset-y :- Text
  [text :- Text
   value :- s/Int]
  (reset-cursor text [(x text) value]))

(s/defn end-x :- Text
  [text :- Text]
  (let [[x y] (:cursor text)
        max   (-> text (line-at y) (count))]
    (reset-cursor text [max y])))

(s/defn end-y :- Text
  [text :- Text]
  (let [[x y] (:cursor text)
        size  (:size text)
        max   (dec size)]
    (reset-cursor text [x max])))

(s/defn start-x :- Text
  [text :- Text]
  (reset-x text 0))

(s/defn start-y :- Text
  [text :- Text]
  (reset-y text 0))

(s/defn start :- Text
  [text :- Text]
  (reset-cursor text [0 0]))

(s/defn end :- Text
  [text :- Text]
  (let [y (-> text (:size) (dec))
        x (-> text (line-at y) (count))]
    (reset-cursor text [x y])))

(s/defn do-move-left :- Text
  [text :- Text]
  (let [[x y] (:cursor text)
        y'     (dec y)
        length (-> text (line-at y') (count))]
    (cond
      (> x 0)         (reset-cursor text [(dec x) y])
      (and (= x 0)
           (>= y' 0)) (reset-cursor text [length y'])
      :else            text)))

(s/defn do-move-right :- Text
  [text :- Text]
  (let [[x y]  (:cursor text)
        size   (:size text)
        y'     (inc y)
        length (-> text (line-at y) (count))]
    (cond
      (< x length)      (reset-cursor text [(inc x) y])
      (and (= x length)
           (< y' size)) (reset-cursor text [0 y'])
      :else              text)))

(s/defn do-move-up :- Text
  [text :- Text]
  (move-y text dec))

(s/defn do-move-down :- Text
  [text :- Text]
  (move-y text inc))

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
      Cc = current cursor (:cursor text)
      Cd = displaced cursor (:cursor (f text))

   I. If no selection already present:
          Ck = Cc
          Move to II

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

   II. Determine new region area.
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
        (reset-lines text (concat (:lines this) (:lines that))))
      (reduce text texts)))

(s/defn join :- Text
  [text :- Text, & texts :- [Text]]
  (reduce (fn [this that]
            (let [ths       (:size this)
                  move      (fn [[x y]] [x (+ y ths)])
                  cursor    (:cursor that)
                  selection (:selection that)]
              (-> (append this that)
                  (reset-cursor (move cursor))
                  (reset-selection (when-let [{start :start end :end} selection]
                                     {:start (move start)
                                      :end   (move end)}))))) text texts))

(s/defn joined :- Text
  [texts :- [Text]]
  (reduce join empty-text texts))

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
  (if (> (x text) 0)
    (-> text
        (do-move-left)
        (slice (fn [l r] (concat l (vec (rest r))))))
    (-> text
        (do-move-left)
        (peer
          (fn [l [a b & r]]
            (concat (conj l (vec (concat a b))) r))))))

(s/defn pair-delete :- Text
  [text :- Text]
  (-> text
      (move-x dec)
      (slice (fn [l r] (concat l (drop 2 r))))))

(s/defn chunk-delete :- Text
  [text :- Text]
  (let [[xs ys] (-> text (:selection) (:start))
        [xe ye] (-> text (:selection) (:end))]
    (if (= ys ye)
      (-> text
          (reset-cursor [xs ys])
          (slice (fn [l r] (concat l (drop (- xe xs) r)))))
      (let [lines  (:lines text)
            left   (->> (line-at text ys) (take xs))
            right  (->> (line-at text ye) (drop xe))
            line   (concat left right)
            top    (take ys lines)
            bottom (->> lines (drop (inc ye)) (cons line))]
        (-> text
            (reset-cursor [xs ys])
            (reset-lines (concat top bottom)))))))

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
        max-y (-> text (:size) (dec))
        max-x (-> text (line-at y) (count))]
    (and (= x max-x) (= y max-y))))

(s/defn do-delete-previous :- Text
  [text :- Text]
  (cond
    (selecting? text)                    (chunk-delete text)
    (pair? text)                         (pair-delete text)
    (paired-tokens (previous-char text)) (do-move-left text)
    :else                                (simple-delete text)))

(s/defn do-delete-current :- Text
  [text :- Text]
  (cond
    (selecting? text)                   (chunk-delete text)
    (pair? text)                        (pair-delete text)
    (paired-tokens (current-char text)) (do-move-right text)
    (at-end? text)                      text
    :else                               (-> text (do-move-right) (simple-delete))))

(s/defn simple-insert :- Text
  [text :- Text
   value :- Character]
  (-> text
      (slice (fn [l r] (concat (conj l value) r)))
      (move-x inc)))

(s/defn pair-insert :- Text
  [text :- Text
   [pl pr] :- [Character]]
  (-> text
      (slice (fn [l r] (concat (conj l pl pr) r)))
      (move-x inc)))

(s/defn delete-selected :- Text
  [text :- Text]
  (if (selecting? text)
    (do-delete-previous text)
    text))

(s/defn do-insert :- Text
  [text :- Text
   input :- Character]
  (let [overwritten (delete-selected text)]
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
          [xe ye] (-> text (:selection) (:end))
          lines   (-> text (:lines))]
      (-> text
          (reset-lines (->> lines (take (inc ye)) (drop ys)))
          (reset-selection nil)
          (end)
          (switch (fn [line] (take xe line)))
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
        [x y]  (some-> copied (:cursor))]
    (m/match [copied]
      [{:lines [a]}] (-> (delete-selected text)
                         (slice #(concat %1 a %2))
                         (move-y #(+ % y))
                         (move-x #(+ % x)))
      [{:lines [a b]}] (-> (delete-selected text)
                           (split #(vector (concat %1 a) (concat b %2)))
                           (move-y #(+ % y))
                           (reset-x x))
      [{:lines [a & b]}] (-> (delete-selected text)
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
           current    (reset-cursor text expand-from)]
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
      [:word \( \)]                              (free-expansion text)
      [:word \[ \]]                              (free-expansion text)
      [:word \{ \}]                              (free-expansion text)
      [:word \space \space]                      (free-expansion text)
      [:word \space nil]                         (free-expansion text)
      [:word nil \space]                         (free-expansion text)
      [:word (:or \" \space) (:or \) \] \})]     (closed-paren-expansion text)
      [:word (:or \) \] \}) _]                   (closed-paren-expansion (do-move-left text))
      [_ _ (:or \( \[ \{)]                       (open-paren-expansion text)
      [:word (:or \( \[ \{ \") (:or \space nil)] (open-paren-expansion (do-move-left text))
      [:word (:or \( \[ \{ \" \space nil) _]     (word-expansion-right text)
      [:word _ _]                                (word-expansion-left text)
      :else                                      (free-expansion text))))

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

(s/defn do-auto-complete :- Text
  [text :- Text,
   input :- [Character]]
  (if (empty? input)
    text
    (-> text
        (do-expand-select)
        (do-delete-previous)
        (slice (fn [l r] (concat l input r)))
        (move-x #(+ % (count input))))))
;
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
  (let [padding (repeat amount \space)
        lines   (->> text (:lines) (mapv #(vec (concat padding %))))]
    (reset-lines text lines)))

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

(s/defn auto-complete :- Text
  [text :- Text
   value :- [Character]]
  (-> text (do-auto-complete value) (deselect)))
