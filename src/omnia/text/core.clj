(ns omnia.text.core
  (:require [clojure.core.match :as m]
            [schema.core :as s]
            [omnia.config.components.event :as e]
            [clojure.string :refer [join split-lines]]
            [clojure.set :refer [union map-invert]]
            [omnia.util.schema :refer [=> Point Region]]
            [omnia.util.collection :refer [firstv do-until]]))

(def Line [Character])

(def ExpansionScope (s/enum :word :expr))

(comment
  "General problem:
  The cursor on the x axis is currently off by 1, so to speak.
  The current character, in it's case, is the one _after_ the cursor
  Which means that in the x axis, length == last index, even though it starts from 0
  Theoretically, I should need to change it so the current character equeals the one _before_ the cursor.
  This should make the end basically unreachable.
  The only question is: can the cursor the actually go to the end?

  I should view the cursor as a box, instead of a line. I only render it as a line")
(def Seeker
  {:lines     [Line]
   ;; position in text, the cursor is placed at the index where a character can be input
   :cursor    Point
   :size      s/Int
   :expansion ExpansionScope
   :history   [(s/recursive #'Seeker)]
   :rhistory  [(s/recursive #'Seeker)]
   ;; range of text selected. Inclusive both in the start and end
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
  [lines :- [Line]]
  (-> empty-seeker (assoc :lines lines) (resize)))

(s/def empty-line :- Seeker
  (seeker [[]]))

(s/defn from-string :- Seeker
  "This has to create empty vectors from new lines.
   E.g: '1\n\n' => [[\1] []]; '\n\n' => [[] []]"
  [string :- s/Str]
  (if (empty? string)
    empty-seeker
    (loop [lines []
           rem   string]
      (if (empty? rem)
        (seeker lines)
        (recur (->> rem
                    (take-while #(not= % \newline))
                    (vec)
                    (conj lines))
               (->> rem
                    (drop-while #(not= % \newline))
                    (drop 1)))))))

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
  (let [y (-> seeker (:cursor) (nth 1))]                    ;; FIXME: why is it 1?
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

(s/defn switch :- Seeker
  "Looks at the line where the cursor is currently.
  Applies a binary function `f` on that line.
  `f` is expected to return a new line of text and
  replaces the one line on which it was applied."
  [seeker :- Seeker
   f :- (=> Line Line)]
  (let [[_ y] (:cursor seeker)
        line' (vec (f (current-line seeker)))]
    (rebase seeker #(assoc % y line'))))

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
  (let [[_ y] (:cursor seeker)]
    (if (zero? y)
      seeker
      (move-y seeker dec))))

(s/defn move-down :- Seeker
  [seeker :- Seeker]
  (let [[_ y] (:cursor seeker)
        size  (:size seeker)]
    (if (>= (inc y) size)
      seeker
      (move-y seeker inc))))

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

(s/defn selected? :- s/Bool
  [seeker :- Seeker]
  (-> seeker :selection some?))

(s/defn reset-selection :- Seeker
  [seeker :- Seeker
   region :- (s/maybe Region)]
  (assoc seeker :selection region))

(s/defn reset-selection-to-point :- Seeker
  [seeker :- Seeker
   point :- Point]
  (reset-selection seeker {:start point :end point}))

(s/defn selection :- Region
  [seeker :- Seeker]
  (:selection seeker))

(s/defn adjust-selection :- Seeker
  [seeker :- Seeker]
  (if (selected? seeker)
    (let [norm   (fn [[x y]] (+ x (* y 2)))
          cursor (:cursor seeker)
          start  (-> seeker :selection :start)
          end    (-> seeker :selection :end)
          region {:start (min-key norm cursor start end)
                  :end   (max-key norm cursor start end)}]
      (reset-selection seeker region))
    seeker))

(s/defn continue-selection :- Seeker
  [seeker :- Seeker]
  (if (selected? seeker)
    seeker
    (reset-selection-to-point seeker (:cursor seeker))))

(s/defn start-selection :- Seeker
  [seeker :- Seeker]
  (-> seeker
      (reset-selection-to-point (:cursor seeker))
      (reset-expansion :word)))

(s/defn select-left :- Seeker
        [seeker :- Seeker]
        (-> seeker (continue-selection) (move-left)))

(s/defn select-right :- Seeker
        [seeker :- Seeker]
        (-> seeker (continue-selection) (move-right)))

(s/defn select-up :- Seeker
        [seeker :- Seeker]
        (-> seeker (continue-selection) (move-up)))

(s/defn select-down :- Seeker
        [seeker :- Seeker]
        (-> seeker (continue-selection) (move-down)))

(s/defn select-jump-left :- Seeker
        [seeker :- Seeker]
        (-> seeker (continue-selection) (jump-left)))

(s/defn select-jump-right :- Seeker
        [seeker :- Seeker]
        (-> seeker (continue-selection) (jump-right)))

(s/defn deselect :- Seeker
  [seeker :- Seeker]
  (-> seeker
      (reset-selection nil)
      (reset-expansion :word)))

(s/defn adjoin' :- Seeker
  [this :- Seeker
   that :- Seeker]
  (rebase this #(concat % (:lines that))))

(s/defn conjoin' :- Seeker
  [this-seeker :- Seeker
   that-seeker :- Seeker]
  (let [[x y] (:cursor that-seeker)
        ths   (:size this-seeker)]
    (-> (adjoin' this-seeker that-seeker)
        (reset-to [x (+ y ths)])
        (reset-selection (:selection that-seeker)))))

(s/defn conjoin :- Seeker
  [seeker :- Seeker, & seekers :- [Seeker]]
  (reduce conjoin' seeker seekers))

(s/defn conjoined :- Seeker
  [seekers :- [Seeker]]
  (reduce conjoin' empty-seeker seekers))

(s/defn adjoin :- Seeker
  [seeker :- Seeker, & seekers :- [Seeker]]
  (reduce adjoin' seeker seekers))

(s/defn adjoined :- Seeker
  [seekers :- [Seeker]]
  (reduce adjoin' empty-seeker seekers))

(s/defn merge-lines :- Seeker
  [seeker :- Seeker]
  (peer seeker (fn [l [a b & t]]
                 (-> (conj l (concat a b))
                     (concat t)))))

(s/defn break :- Seeker
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

;; FIXME: This is wrong
(s/defn chunk-delete :- Seeker
  [seeker :- Seeker]
  (let [start (-> seeker (:selection) (:start))
        end   (-> seeker (:selection) (:end))]
    (-> seeker
        (reset-to end)
        (do-until simple-delete #(-> % (:cursor) (= start))))))

(s/defn pair? :- s/Bool
  [seeker :- Seeker]
  (m/match [(previous-char seeker) (current-char seeker)]
           [\( \)] true
           [\[ \]] true
           [\{ \}] true
           [\" \"] true
           :else false))

(s/defn backspace :- Seeker
  [seeker :- Seeker]
  (cond
    (selected? seeker)                     (chunk-delete seeker)
    (pair? seeker)                         (pair-delete seeker)
    (paired-tokens (previous-char seeker)) (move-left seeker)
    :else                                  (simple-delete seeker)))

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
  (cond-> seeker
          (selected? seeker) backspace))

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

(s/defn delete :- Seeker
  [seeker :- Seeker]
  (let [x (move-right seeker)]
    (cond
      (= (:cursor seeker) (:cursor x)) seeker
      (pair? seeker) (-> seeker (pair-delete) (move-left))
      :else (backspace x))))

(s/defn extract :- Seeker
  "The general formula is: drop M_start . take (M_end + 1)"
  [seeker :- Seeker]
  (let [[xs ys] (-> seeker (:selection) (:start))
        [xe ye] (-> seeker (:selection) (:end))]
    (-> seeker
        (rebase #(->> % (take (inc ye)) (drop ys)))
        (end)
        (switch #(take (inc xe) %))
        (start)
        (switch #(drop xs %)))))

(s/defn copy :- Seeker
  [seeker :- Seeker]
  (->> seeker (extract) (assoc seeker :clipboard)))

(s/defn cut :- Seeker
        [seeker :- Seeker]
        (cond-> (copy seeker)
                (selected? seeker) (backspace)))

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
  (-> seeker (start) (start-selection) (end) (adjust-selection)))

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
           end-cursor  nil
           current     (move-right seeker)]
      (let [char (current-char current)]
        (cond
          (zero? open-parens)                    {:start init-cursor :end end-cursor}
          (pairs? init-char char)                (recur (dec open-parens) (:cursor current) (move-right current))
          (= text-end-cursor (:cursor current))  nil
          (= init-char char)                     (recur (inc open-parens) nil (move-right current))
          :else                                  (recur open-parens nil (move-right current)))))))

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
   :end   (:cursor (-> seeker (jump-right) (move-left)))})

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

(s/defn forget :- Seeker
  [seeker :- Seeker]
  (assoc seeker :history '() :rhistory '()))

(s/defn remember :- Seeker
  [seeker :- Seeker]
  (let [history (:history seeker)]
    (->> history
         (cons (forget seeker))
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
          (assoc :rhistory (-> seeker (forget) (cons rhistory)))))))

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
          (assoc :history (-> seeker (forget) (cons history)))))))

(s/defn auto-complete :- Seeker
  [seeker :- Seeker, input :- [Character]]
  (if (empty? input)
    seeker
    (-> seeker
        (expand)
        (backspace)
        (slicer #(concat input %))
        (move-x #(+ % (count input))))))

(s/defn stringify :- s/Str
  [seeker :- Seeker]
  (->> seeker
       (:lines)
       (mapv #(apply str %))
       (join "\n")))

(s/defn debug-string :- String
  [seeker :- Seeker]
  (-> seeker
      (slicer (fn [[current-char & rest]]
                (vec (concat [\| current-char \|] rest))))
      (stringify)))

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
    e/select-up         (-> seeker (continue-selection) (move-up) (adjust-selection))
    e/select-down       (-> seeker (continue-selection) (move-down) (adjust-selection))
    e/select-left       (-> seeker (continue-selection) (move-left) (adjust-selection))
    e/select-right      (-> seeker (continue-selection) (move-right) (adjust-selection))
    e/jump-select-left  (-> seeker (continue-selection) (jump-left) (adjust-selection))
    e/jump-select-right (-> seeker (continue-selection) (jump-right) (adjust-selection))
    e/backspace         (-> seeker (remember) (backspace) (deselect))
    e/delete            (-> seeker (remember) (delete) (deselect))
    e/break             (-> seeker (remember) (break) (deselect))
    e/undo              (-> seeker (undo) (deselect))
    e/redo              (-> seeker (redo) (deselect))
    e/character         (-> seeker (remember) (insert (:value event)) (deselect))
    seeker))