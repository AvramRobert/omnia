(ns omnia.text.core
  (:require [clojure.core.match :as m]
            [schema.core :as s]
            [omnia.config.components.event :as e]
            [clojure.string :refer [join split-lines]]
            [clojure.set :refer [union map-invert]]
            [omnia.util.schema :refer [=> Point Region]]
            [omnia.util.collection :refer [do-until]]))

(def Line [Character])

(def Seeker
  {:lines     [Line]
   :cursor    Point
   :height    s/Int
   :expansion (s/enum :word :expr)
   :history   [(s/recursive #'Seeker)]
   :rhistory  [(s/recursive #'Seeker)]
   :selection (s/maybe Point)
   :clipboard (s/maybe (s/recursive #'Seeker))})

(def empty-seeker
  {:lines     []
   :cursor    [0 0]
   :height    0
   :expansion :word
   :selection nil
   :clipboard nil
   :history   '()
   :rhistory  '()})

(def open-pairs {\( \) \[ \] \{ \}})
(def closed-pairs (map-invert open-pairs))
(def open-tokens (set (keys open-pairs)))
(def closed-tokens (set (vals open-pairs)))
(def parens (union open-tokens closed-tokens))
(def tokens (union parens #{\"}))

(s/defn resize :- Seeker
  [seeker :- Seeker]
  (assoc seeker :height (-> seeker :lines count)))

(s/defn seeker :- Seeker
  [lines :- [Line]]
  (-> empty-seeker (assoc :lines lines) (resize)))

(s/def empty-line :- Seeker
  (seeker [[]]))

(s/defn from-string :- Seeker
  "This has to create empty vectors from new lines.
   E.g: '1\n\n' => [[\1] []]"
  [string :- s/Str]
  (let [division  #(if (= \newline %) :newline :other)
        aggregate #(let [[a _] (first %)]
                     (m/match [a (count %)]
                              [:newline 1] []
                              [:newline _] (vec (repeat (dec (count %)) []))
                              :else        (vector (mapv second %))))]
    (if (empty? string)
      empty-seeker
      (->> string
           (mapv (juxt division identity))
           (partition-by first)
           (mapcat aggregate)
           (vec)
           (seeker)))))

(s/defn space? :- s/Bool
  [character :- Character]
  (= \space character))

(s/defn line-at :- Line
  [seeker :- Seeker, y :- s/Int]
  (-> seeker (:lines) (nth y [])))

(s/defn current-line :- Line
  [seeker :- Seeker]
  (let [y (-> seeker (:cursor) (nth 1))]
    (line-at seeker y)))

(s/defn sym-at :- (s/maybe Character)
  [seeker :- Seeker
   [x y]  :- Point]
  (-> seeker (line-at y) (nth x nil)))

(s/defn current-sym :- (s/maybe Character)
  [seeker :- Seeker]
  (sym-at seeker (:cursor seeker)))

(s/defn rebase :- Seeker
  [seeker :- Seeker f]
  (-> seeker (update :lines (comp vec f)) (resize)))

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
          (let [height (:height seeker)
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
  (let [height (:height seeker)
        y-max (if (zero? height) 0 (dec height))]
    (move seeker (fn [[x _]] [x y-max]))))

(s/defn start :- Seeker
  [seeker :- Seeker]
  (-> seeker (start-y) (start-x)))

(s/defn end :- Seeker
  [seeker :- Seeker]
  (-> seeker (end-y) (end-x)))

(s/defn go-forward-with :- Seeker
  [seeker :- Seeker
   f      :- (=> Seeker Seeker)]
  (let [h (-> seeker :height dec)
        w (-> seeker current-line count)]
    (m/match [(:cursor seeker)]
             [[w h]] seeker
             [[w _]] (-> seeker (move-y inc) (start-x) (f))
             :else (move-x seeker inc))))

(s/defn go-back-with :- Seeker
  [seeker :- Seeker
   f      :- (=> Seeker Seeker)]
  (m/match [(:cursor seeker)]
           [[0 0]] seeker
           [[0 _]] (-> seeker (move-y dec) (end-x) (f))
           :else (move-x seeker dec)))

(s/defn go-back :- Seeker
  [seeker :- Seeker]
  (go-back-with seeker identity))

(s/defn go-forward :- Seeker
  [seeker :- Seeker]
  (go-forward-with seeker identity))

(s/defn left :- (s/maybe Character)
  [seeker :- Seeker]
  (when (not= [0 0] (:cursor seeker))
    (-> seeker (go-back) (current-sym))))

(s/defn right :- (s/maybe Character)
  [seeker :- Seeker]
  (current-sym seeker))

(s/defn prev-char :- (s/maybe Character)
  [seeker :- Seeker]
  (left seeker))

(s/defn current-char :- (s/maybe Character)
  [seeker :- Seeker]
  (right seeker))

(s/defn selected? :- s/Bool
  [seeker :- Seeker]
  (-> seeker :selection nil? not))

(s/defn reselect :- Seeker
  [seeker   :- Seeker
   f        :- (=> Point Point)]
  (update seeker :selection #(some-> % f)))

(s/defn select :- Seeker
  [seeker :- Seeker]
  (let [cursor (:cursor seeker)]
    (-> seeker
        (assoc :expansion :word)
        (assoc :selection cursor))))

(s/defn soft-select :- Seeker
  [seeker :- Seeker]
  (if (selected? seeker)
    seeker
    (select seeker)))

(s/defn deselect :- Seeker
  [seeker :- Seeker]
  (-> seeker
      (assoc :selection nil)
      (assoc :expansion :word)))

(s/defn selection :- Region
  [seeker :- Seeker]
  (let [cursor (:cursor seeker)
        init   (-> seeker (soft-select) (:selection))
        [start end] (sort-by (juxt second first) [init cursor])]
    {:start start
     :end   end}))

(s/defn adjoin' :- Seeker
  [this :- Seeker
   that :- Seeker]
  (rebase this #(concat % (:lines that))))

(s/defn conjoin' :- Seeker
  [this-seeker :- Seeker
   that-seeker :- Seeker]
  (let [[x y] (:cursor that-seeker)
        ths   (:height this-seeker)]
    (-> (adjoin' this-seeker that-seeker)
        (assoc :selection (:selection that-seeker))
        (reselect (fn [[xs ys]] [xs (+ ys ths)]))
        (reset-to [x (+ y ths)]))))

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

(s/defn go-up :- Seeker
        [seeker :- Seeker]
        (let [offset (move-y seeker dec)]
          (if (current-sym offset)
            offset
            (-> seeker (start-x) (go-back)))))

(s/defn go-down :- Seeker
        [seeker :- Seeker]
        (let [offset (move-y seeker inc)]
    (if (current-sym offset)
      offset
      (-> seeker (move-y inc) (end-x)))))

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
      (go-back-with merge-lines)))

(s/defn pair-delete :- Seeker
  [seeker :- Seeker]
  (-> seeker
      (slice #(concat (drop-last %1) (rest %2)))
      (move-x dec)))

(s/defn chunk-delete :- Seeker
  [seeker :- Seeker]
  (let [{start :start
         end   :end} (selection seeker)]
    (-> seeker
        (reset-to end)
        (do-until simple-delete #(-> % (:cursor) (= start))))))

(s/defn pair? :- s/Bool
  [seeker :- Seeker]
  (m/match [(left seeker) (right seeker)]
           [\( \)] true
           [\[ \]] true
           [\{ \}] true
           [\" \"] true
           :else false))

(s/defn backspace :- Seeker
  [seeker :- Seeker]
  (cond
    (selected? seeker)     (chunk-delete seeker)
    (pair? seeker)         (pair-delete seeker)
    (tokens (left seeker)) (go-back seeker)
    :else                  (simple-delete seeker)))

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

(s/defn jump :- Seeker
  [seeker :- Seeker
   f      :- (=> Seeker Seeker)
   look   :- (=> Seeker (s/maybe Character))]
  (letfn [(blanks? [s] (some-> s (look) (space?)))
          (literals? [s] (not (blanks? s)))
          (tokens? [s] (tokens (look s)))
          (bounds? [s] (or (-> s (:cursor) (nth 0) (zero?))
                           (nil? (look s))))
          (go [pred] (do-until (f seeker) f pred))]
    (cond
      (blanks? seeker) (go #(or (bounds? %) (tokens? %) (literals? %)))
      (tokens? seeker) (go #(or (bounds? %) (literals? %)))
      :else            (go #(or (bounds? %) (tokens? %) (blanks? %))))))

(s/defn jump-left :- Seeker
  [seeker :- Seeker]
  (jump seeker go-back left))

(s/defn jump-right :- Seeker
  [seeker :- Seeker]
  (jump seeker go-forward right))

(s/defn delete :- Seeker
  [seeker :- Seeker]
  (let [x (go-forward seeker)]
    (cond
      (= (:cursor seeker) (:cursor x)) seeker
      (pair? seeker) (-> seeker (pair-delete) (go-back))
      :else (backspace x))))

(s/defn extract :- Seeker
  [seeker :- Seeker]
  (let [height (:height seeker)
        {[xs ys] :start
         [xe ye] :end} (selection seeker)
        y (if (= ye height) ye (inc ye))]
    (-> seeker
        (rebase #(subvec % ys y))
        (end)
        (slicel #(subvec % 0 xe))
        (start)
        (slicer #(drop xs %)))))

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
  (-> seeker (start) (select) (end)))

(defn- any-pair? [this that]
  (or (= (open-pairs this :none) that)
      (= (closed-pairs this :none) that)))

(defn- expand-result [tuple]
  (nth tuple 0))

(defn- expand-value [tuple]
  (nth tuple 1))

(defn open-expand [seeker]
  "Note: This assumes that the right-hand side of the seeker starts with an open parens"
  (let [l      (right seeker)
        ending (end seeker)]
    (loop [seen    1
           current (-> seeker (select) (go-forward))]
      (let [r (right current)]
        (cond
          (zero? seen)                           [:matched current]
          (= (:cursor ending) (:cursor current)) [:unmatched current]
          (any-pair? l r)                        (recur (dec seen) (go-forward current))
          (= l r)                                (recur (inc seen) (go-forward current))
          :else                                  (recur seen (go-forward current)))))))

(defn closed-expand [seeker]
  "Note: This assumes that the left-hand side of the seeker starts with a closed parens"
  (let [r         (left seeker)
        beginning (start seeker)
        switch    #(assoc % :cursor (:cursor seeker)
                            :selection (:cursor %))]
    (loop [seen    1
           current (-> seeker (select) (go-back))]
      (let [l (left current)]
        (cond
          (zero? seen)                              [:matched (switch current)]
          (= (:cursor beginning) (:cursor current)) [:unmatched (switch current)]
          (any-pair? l r)                           (recur (dec seen) (go-back current))
          (= l r)                                   (recur (inc seen) (go-back current))
          :else                                     (recur seen (go-back current)))))))

(defn near-expand [seeker]
  "Note: This assumes that the seeker isn't neighbouring parens"
  (let [beginning (start seeker)]
    (loop [seen    ()
           current seeker]
      (let [l (left current) r (first seen)]
        (cond
          (and (empty? seen) (open-pairs l))        (-> current (go-back) (open-expand))
          (any-pair? l r)                           (recur (rest seen) (go-back current))
          (= (:cursor beginning) (:cursor current)) [:unmatched (-> current (select) (end))]
          (closed-pairs l)                          (recur (conj seen l) (go-back current))
          :else                                     (recur seen (go-back current)))))))

(s/defn expand :- Seeker
  [seeker :- Seeker]
  (-> (m/match [(:expansion seeker) (left seeker) (right seeker)]
               [:word \( \)]                          (-> seeker (near-expand) (expand-value))
               [:word \[ \]]                          (-> seeker (near-expand) (expand-value))
               [:word \{ \}]                          (-> seeker (near-expand) (expand-value))
               [:word \space \space]                  (-> seeker (near-expand) (expand-value))
               [:word (:or \" \space) (:or \) \] \})] (-> seeker (go-forward) (closed-expand) (expand-value))
               [:word (:or \) \] \}) _]               (-> seeker (closed-expand) (expand-value))
               [(:or :word :expr) _ (:or \( \[ \{)]   (-> seeker (open-expand) (expand-value))
               [:word (:or \( \[ \{ \" \space nil) _] (-> seeker (select) (jump-right))
               [:word _ _]                            (-> seeker (jump-left) (select) (jump-right))
               :else                                  (-> seeker (near-expand) (expand-value)))
      (assoc :expansion :expr)))

(s/defn find-pair :- (s/maybe Region)
  [seeker :- Seeker]
  (letfn [(choose [tuple]
            (when (= :matched (expand-result tuple))
              (-> tuple (expand-value) (go-back) (selection))))]
    (cond
      (open-pairs (right seeker))  (-> seeker (open-expand) (choose))
      (closed-pairs (left seeker)) (-> seeker (closed-expand) (choose))
      :else                        (-> seeker (near-expand) (choose)))))

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
  (-> seeker (slicel #(conj % \|)) (stringify)))

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
    e/up                (-> seeker (go-up) (deselect))
    e/down              (-> seeker (go-down) (deselect))
    e/left              (-> seeker (go-back) (deselect))
    e/right             (-> seeker (go-forward) (deselect))
    e/jump-left         (-> seeker (jump-left) (deselect))
    e/jump-right        (-> seeker (jump-right) (deselect))
    e/select-up         (-> seeker (soft-select) (go-up))
    e/select-down       (-> seeker (soft-select) (go-down))
    e/select-left       (-> seeker (soft-select) (go-back))
    e/select-right      (-> seeker (soft-select) (go-forward))
    e/jump-select-left  (-> seeker (soft-select) (jump-left))
    e/jump-select-right (-> seeker (soft-select) (jump-right))
    e/backspace         (-> seeker (remember) (backspace) (deselect))
    e/delete            (-> seeker (remember) (delete) (deselect))
    e/break             (-> seeker (remember) (break) (deselect))
    e/undo              (-> seeker (undo) (deselect))
    e/redo              (-> seeker (redo) (deselect))
    e/character         (-> seeker (remember) (insert (:value event)) (deselect))
    seeker))