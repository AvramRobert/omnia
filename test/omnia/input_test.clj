(ns omnia.input-test
  (require [omnia.input :as i]
           [clojure.test :refer [is]]
           [clojure.test.check.clojure-test :refer [defspec]]
           [clojure.test.check.properties :refer [for-all]]
           [clojure.test.check.generators :as gen]))


(defn rand-line [seeker]
  (-> seeker (i/height) (rand-int)))

(defn rand-pos
  ([seeker]
   (-> seeker (i/line) (count) (rand-int)))
  ([seeker y]
   (-> seeker (i/reset-y y) (i/line) (count) (rand-int))))

(defmacro <=> [this-seeker that-seeker]
  `(is (= (:lines ~this-seeker) (:lines ~that-seeker))))

(defmacro can-be [val & fs]
  `(do ~@(map (fn [f#] `(is (~f# ~val))) fs)))

(defn before? [this-seeker that-seeker]
  (let [[xt yt] (:cursor this-seeker)
        [xa ya] (:cursor that-seeker)]
    (if (= yt ya)
      (< xt xa)
      (< yt ya))))

(defn after? [this-seeker that-seeker]
  (not (before? this-seeker that-seeker)))

(defn there? [this-seeker that-seeker]
  (= (:cursor this-seeker)
     (:cursor that-seeker)))

(def gen-line (->> gen/char-ascii
                   (gen/vector)
                   (gen/such-that (comp not empty?))
                   (gen/vector)
                   (gen/such-that (comp not empty?))))

(def gen-seeker (->> gen-line
                     (gen/fmap i/seeker)
                     (gen/fmap #(let [y (rand-line %)
                                      x (rand-pos % y)]
                                  (i/move % (fn [_] [x y]))))))

;; I. Peering

(defn peer-start [seeker]
  (-> (i/start seeker)
      (i/peer (fn [_ b] b))
      (<=> seeker)))

(defn peer-end [seeker]
  (-> (i/end seeker)
      (i/peer (fn [a _] a))
      (<=> (i/rebase seeker #(drop-last %)))))

(defn peer-middle-insert-substitution [seeker]
  (let [[_ y] (:cursor seeker)
        some-text [[\a \b]]]
    (-> seeker
        (i/peer (fn [a b] (concat a some-text b)))
        (i/line)
        (= (first some-text))
        (is))))

(defn peer-middle-insert-invariance [seeker]
  (let [[_ y] (:cursor seeker)
        some-text [[\a \b]]]
    (-> seeker
        (i/peer (fn [a b] (concat a some-text b)))
        (i/peer (fn [a b] (concat a (drop 1 b))))
        (<=> seeker))))

(defn peer-middle-delete-left [seeker]
  (let [[_ y] (:cursor seeker)]
    (-> (i/peer seeker (fn [_ b] b))
        (<=> (i/rebase seeker #(drop y %))))))

(defn peer-middle-delete-right [seeker]
  (let [[_ y] (:cursor seeker)]
    (-> (i/peer seeker (fn [a _] a))
        (<=> (i/rebase seeker #(take y %))))))

(defn peer-empty-insert [seeker]
  (-> (i/seeker [])
      (i/peer (fn [a b] (concat a (:lines seeker) b)))
      (<=> seeker)))

(defspec peering
         100
         (for-all [seeker gen-seeker]
                  (peer-start seeker)
                  (peer-middle-insert-substitution seeker)
                  (peer-middle-insert-invariance seeker)
                  (peer-middle-delete-left seeker)
                  (peer-middle-delete-right seeker)
                  (peer-empty-insert seeker)
                  (peer-end seeker)))

;; II. Splitting

(defn split-reduce-left [seeker]
  (let [[x y] (:cursor seeker)
        expected (->> seeker (i/line) (drop x))]
    (-> seeker
        (i/split (fn [_ b] [b]))
        (i/line)
        (= expected)
        (is))))

(defn split-reduce-right [seeker]
  (let [[x y] (:cursor seeker)
        expected (->> seeker (i/line) (take x))]
    (-> seeker
        (i/split (fn [a _] [a]))
        (i/line)
        (= expected)
        (is))))

(defn split-enhance [seeker]
  (let [some-line [\a \b]]
    (-> seeker
        (i/split (fn [a b] [some-line (concat a b)]))
        (i/line)
        (= some-line)
        (is))))

(defn split-replace [seeker]
  (let [some-text [[\a \b] [\c \d]]]
    (-> seeker
        (i/split (fn [_ _] some-text))
        (i/line)
        (= (first some-text))
        (is))))

(defn split-empty-insert [seeker]
  (-> (i/seeker [])
      (i/split (fn [a b] [(concat a (i/line seeker) b)]))
      (i/line)
      (= (i/line seeker))
      (is)))

(defspec splitting
         100
         (for-all [seeker gen-seeker]
                  (split-reduce-left seeker)
                  (split-reduce-right seeker)
                  (split-enhance seeker)
                  (split-replace seeker)
                  (split-empty-insert seeker)))

;; III. Slicing

(defn slice-reduce-left [seeker]
  (let [[x _] (:cursor seeker)
        expected (->> seeker (i/line) (take x))]
    (-> seeker
        (i/slice (fn [a _] a))
        (i/line)
        (= expected)
        (is))))

(defn slice-reduce-right [seeker]
  (let [[x _] (:cursor seeker)
        expected (->> seeker (i/line) (drop x))]
    (-> seeker
        (i/slice (fn [_ b] b))
        (i/line)
        (= expected)
        (is))))

(defn slice-enhance [seeker]
  (let [some-line [\a \b]
        expected (->> seeker (i/line) (concat some-line))]
    (-> seeker
        (i/slice (fn [a b] (concat some-line a b)))
        (i/line)
        (= expected)
        (is))))

(defn slice-replace [seeker]
  (let [some-line [\a \b]]
    (-> seeker
        (i/slice (fn [_ _] some-line))
        (i/line)
        (= some-line)
        (is))))

(defn slice-empty-insert [seeker]
  (let [line (i/line seeker)]
    (-> (i/seeker [])
        (i/slice (fn [a b] (concat a line b)))
        (i/line)
        (= line)
        (is))))

(defspec slicing
         100
         (for-all [seeker gen-seeker]
                  (slice-reduce-left seeker)
                  (slice-reduce-right seeker)
                  (slice-enhance seeker)
                  (slice-replace seeker)
                  (slice-empty-insert seeker)))

;; IV. Moving

(defn bounded [seeker]
  (let [cursor (:cursor seeker)
        x-moved (i/move-x seeker #(+ 1000 %))
        y-moved (i/move-y seeker #(+ 1000 %))]
    (is (= cursor (:cursor x-moved)))
    (is (= cursor (:cursor y-moved)))))

(defn unbounded [seeker]
  (let [some-line [\a \b]]
    (-> seeker
        (i/split (fn [a b] [a some-line b]))
        (i/move-y inc)
        (i/start-x)
        (can-be #(-> % (i/line) (= some-line))
                #(-> % (i/center) (= \a))))))

(defspec moving
         100
         (for-all [seeker gen-seeker]
                  (bounded seeker)
                  (unbounded seeker)))

;; V. Retrieving

(defn previous [seeker]
  (let [some-text [[\a \b] [\c \d]]]
    (-> seeker
        (i/peer (fn [a b] (concat a some-text b)))
        (i/start-x)
        (i/move-y inc)
        (can-be #(-> % (i/regress) (i/left) (= \b))
                #(-> % (i/move-y dec) (i/move-x inc) (i/left) (= \a))))))

(defn current [seeker]
  (let [expected (-> (:lines seeker) (first) (first))]
    (-> (i/start seeker)
        (i/center)
        (= expected)
        (is))))

(defn following [seeker]
  (let [some-text [[\a \b] [\c \d]]]
    (-> (i/end seeker)
        (i/peer (fn [a b] (concat a b some-text)))
        (i/end)
        (i/start-x)
        (can-be #(-> % (i/move-y dec) (i/end-x) (i/right) (= \c))
                #(-> % (i/right) (= \d))))))

(defspec retrieving
         100
         (for-all [seeker gen-seeker]
                  (previous seeker)
                  (current seeker)
                  (following seeker)))


;; VI. Advancing

(defn increment [seeker]
  (let [some-line [\a \b]]
    (-> (i/start seeker)
        (i/split (fn [a b] [some-line a b]))
        (can-be #(-> % (i/advance) (after? %))))))

(defn increment-stop [seeker]
  (-> (i/end seeker)
      (can-be #(-> % (i/advance) (there? %)))))

(defn post-wrap [seeker]
  (let [some-text [[\a \b] [\c \d]]]
    (-> (i/start seeker)
        (i/peer (fn [a b] (concat some-text a b)))
        (i/end-x)
        (can-be #(-> % (i/advance) (after? %))))))

(defspec advancing
         100
         (for-all [seeker gen-seeker]
                  (increment seeker)
                  (increment-stop seeker)
                  (post-wrap seeker)))

;; VII. Regressing

(defn decrement [seeker]
  (let [some-line [\a \b]]
    (-> seeker
        (i/split (fn [a b] [a some-line b]))
        (can-be #(-> % (i/advance) (i/regress) (there? %))))))

(defn decrement-stop [seeker]
  (-> (i/start-x seeker)
      (i/regress)
      (there? seeker)))

(defn pre-wrap [seeker]
  (let [some-text [[\a \b] [\c \d]]]
    (-> seeker
        (i/peer (fn [a b] (concat a b some-text)))
        (i/end)
        (i/start-x)
        (can-be #(-> % (i/regress) (before? %))))))

(defspec regressing
         100
         (for-all [seeker gen-seeker]
                  (decrement seeker)
                  (decrement-stop seeker)
                  (pre-wrap seeker)))

;; VIII. Climbing

(defn ascend [seeker]
  (let [some-text [[\a \b] [\c \d]]]
    (-> seeker
        (i/peer (fn [a b] (concat a some-text b)))
        (i/move-y inc)
        (can-be #(-> % (i/climb) (before? %))))))

(defn ascend-reset [seeker]
  (let [some-text [[\a \b] [\a \b \c]]]
    (-> seeker
        (i/peer (fn [a b] (concat a some-text b)))
        (can-be #(-> % (i/move-y inc) (i/end-x) (i/climb) (there? (i/end-x %)))))))

(defspec climbing
         100
         (for-all [seeker gen-seeker]
                  (ascend seeker)
                  (ascend-reset seeker)))

;; IX. Falling

(defn descend [seeker]
  (let [some-text [[\a \b] [\c \d]]]
    (-> seeker
        (i/peer (fn [a b] (concat a some-text b)))
        (can-be #(-> % (i/fall) (after? %))))))

(defn descend-reset [seeker]
  (let [some-text [[\a \b \c] [\d \e]]]
    (-> seeker
        (i/peer (fn [a b] (concat a some-text b)))
        (i/move-y inc)
        (can-be #(-> % (i/move-y dec) (i/end-x) (i/fall) (there? (i/end-x %)))))))

(defspec falling
         100
         (for-all [seeker gen-seeker]
                  (descend seeker)
                  (descend-reset seeker)))

;; X. Deleting

(defn backward [seeker]
  (let [some-text [[\a \b]]]
    (-> seeker
        (i/peer (fn [a b] (concat a b some-text)))
        (i/end)
        (can-be #(-> % (i/delete) (i/left) (= \a))
                #(-> % (i/delete) (i/delete) (i/left) (nil?))
                #(-> % (i/delete) (i/delete) (i/delete) (<=> seeker))))))

(defn forward [seeker]
  (let [some-text [[\a \b]]]
    (-> seeker
        (i/peer (fn [a b] (concat some-text a b)))
        (i/start)
        (can-be #(-> % (i/munch) (i/center) (= \b))
                #(-> % (i/munch) (i/munch) (i/center) (nil?))
                #(-> % (i/munch) (i/munch) (i/munch) (<=> seeker))))))

(defn paired [seeker]
  (let [remove-line #(-> % (i/delete) (i/delete) (i/delete))
        round [[\( \)]]
        brackets [[\[ \]]]
        squiggly [[\{ \}]]
        quote [[\"\"]]]
    (-> seeker
        (i/peer (fn [a b] (concat a b round brackets squiggly quote))) ;; use split instead of peer to remove (vector) from tests
        (i/end)
        (can-be #(-> % (remove-line) (i/line) (vector) (= squiggly))
                #(-> % (remove-line) (remove-line) (i/line) (vector) (= brackets))
                #(-> % (remove-line) (remove-line) (remove-line) (i/line) (vector) (= round))
                #(-> % (remove-line) (remove-line) (remove-line) (remove-line) (<=> seeker))))))

(defn omitted [seeker]
  (letfn [(f [s c] (i/slicel s #(conj % c)))]
    (-> (i/end seeker)
        (can-be #(-> (f % \)) (i/advance) (i/delete) (<=> (f % \))))
                #(-> (f % \]) (i/advance) (i/delete) (<=> (f % \])))
                #(-> (f % \}) (i/advance) (i/delete) (<=> (f % \})))
                #(-> (f % \") (i/advance) (i/delete) (<=> (f % \")))))))

(defn selectively [seeker]
  (-> seeker
      (i/select-all)
      (i/delete)
      (<=> (i/seeker [[]]))))

(defspec deleting
         100
         (for-all [seeker gen-seeker]
                  (backward seeker)
                  (forward seeker)
                  (paired seeker)
                  (omitted seeker)
                  (selectively seeker)))

;; XI. Inserting

(defn literal [seeker c]
  (-> (i/insert seeker c)
      (i/left)
      (= c)
      (is)))

(defn pairs [seeker]
  (letfn [(f [s l r] (i/slicel s #(conj % l r)))]
    (-> (i/end seeker)
        (can-be
          #(-> % (i/insert \() (<=> (f % \( \))))
          #(-> % (i/insert \[) (<=> (f % \[ \])))
          #(-> % (i/insert \{) (<=> (f % \{ \})))
          #(-> % (i/insert \") (<=> (f % \" \")))))))

(defn ignored [seeker]
  (letfn [(f [s l r] (i/slicel s #(conj % l r)))]
    (-> (i/end seeker)
        (can-be
          #(-> % (i/insert \() (i/insert \)) (<=> (f % \( \))))
          #(-> % (i/insert \[) (i/insert \]) (<=> (f % \[ \])))
          #(-> % (i/insert \{) (i/insert \}) (<=> (f % \{ \})))
          #(-> % (i/insert \") (i/insert \") (<=> (f % \" \")))))))

;; FIXME
(defn overridingly [seeker] true)

(defspec inserting
         100
         (for-all [seeker gen-seeker
                   c gen/char-alpha-numeric]
                  (literal seeker c)
                  (pairs seeker)
                  (ignored seeker)
                  (overridingly seeker)))

;; XII. Jumping

(defn iso [seeker]
  (let [some-text [[\a \b]]]
    (-> seeker
        (i/peer (fn [a b] (concat a some-text b)))
        (i/start-x)
        (can-be #(-> % (i/jump-right) (i/jump-left) (there? %))))))

(defn until-spaces [seeker]
  (let [some-text [[\a \b \c \space \d \e]]]
    (-> seeker
        (i/peer (fn [a b] (concat a some-text b)))
        (can-be #(-> % (i/start-x) (i/jump-right) (i/center) (= \space))
                #(-> % (i/end-x) (i/jump-left) (i/left) (= \space))))))

(defn until-exprs [seeker]
  (letfn [(f [s c]
            (-> (i/peer s #(concat %1 [[\a \b c]] %2))
                (i/start-x)))]
    (can-be seeker
            #(-> (f % \() (i/jump-right) (i/center) (= \())
            #(-> (f % \[) (i/jump-right) (i/center) (= \[))
            #(-> (f % \{) (i/jump-right) (i/center) (= \{))
            #(-> (f % \") (i/jump-right) (i/center) (= \"))
            #(-> (f % \)) (i/jump-right) (i/center) (= \)))
            #(-> (f % \]) (i/jump-right) (i/center) (= \]))
            #(-> (f % \}) (i/jump-right) (i/center) (= \})))))

(defn over-lines [seeker]
  (let [some-text [[\a \b]]]
    (-> seeker
        (i/peer (fn [a b] (concat a some-text b)))
        (i/end-x)
        (can-be #(-> % (i/jump-right) (after? %))))))

;; FIXME
(defn over-spaces [seeker] true)

(defspec jumping
         100
         (for-all [seeker gen-seeker]
                  (iso seeker)
                  (until-spaces seeker)
                  (over-lines seeker)
                  (until-exprs seeker)
                  (over-spaces seeker)))

;; XIII. Selecting

(defn single [seeker]
  (let [some-text [[\a \b]]]
    (-> seeker
        (i/peer (fn [a b] (concat a some-text b)))
        (i/start-x)
        (i/select)
        (i/move-x inc)
        (i/selection)
        (can-be #(-> (:start %) (first) (zero?))
                #(-> (:end %) (first) (= 1))))))

(defn jumps [seeker]
  (-> seeker
      (i/start-x)
      (i/select)
      (i/end-x)
      (can-be #(-> (i/selection %) (:start) (= (:cursor (i/start-x %))))
              #(-> (i/selection %) (:end) (= (:cursor (i/end-x %)))))))

(defn lines [seeker]
  (let [some-text [[\a \b] [\c \d]]]
    (-> seeker
        (i/peer (fn [a b] (concat a some-text b)))
        (i/start-x)
        (i/select)
        (i/fall)
        (can-be #(-> (i/selection %) (:start) (= (:cursor (i/climb %))))
                #(-> (i/selection %) (:end) (= (:cursor %)))))))

(defn blocks [seeker]
  (-> seeker
      (i/select-all)
      (can-be #(-> (i/selection %) (:start) (= (:cursor (i/start %))))
              #(-> (i/selection %) (:end) (= (:cursor (i/end %)))))))

(defspec selecting
         100
         (for-all [seeker gen-seeker]
                  (single seeker)
                  (jumps seeker)
                  (lines seeker)
                  (blocks seeker)))

;; XVI. Merging

(defn additive [seeker1 seeker2]
  (-> (i/join seeker1 seeker2)
      (can-be #(<=> % (i/seeker (concat (:lines seeker1)
                                        (:lines seeker2))))
              #(-> (i/line %) (= (i/line seeker2))))))

(defn selective [seeker1 seeker2]
  (let [s1 (-> seeker1 (i/select) (i/end))
        s2 (-> seeker2 (i/select) (i/end))]
    (-> (i/join s1 s2)
        (can-be #(-> (i/selection %) (:end) (= (:cursor (i/end %))))))))

(defspec merging
         100
         (for-all [seeker1 gen-seeker
                   seeker2 gen-seeker]
                  (additive seeker1 seeker2)
                  (selective seeker1 seeker2)))

;; XV. Expanding

(defn word [seeker] true)
(defn expr [seeker] true)
(defn scoped-expr [seeker] true)
(defn break-out [seeker] true)

(defspec expanding
         100
         (for-all [seeker gen-seeker]
                  (word seeker)
                  (expr seeker)
                  (scoped-expr seeker)
                  (break-out seeker)))

;; XVI. Copying

(defn block-copy [seeker] true)

(defspec copying
         100
         (for-all [seeker gen-seeker]
                  (block-copy seeker)))

;; XVII. Cutting

(defn block-cut [seeker] true)

(defspec cutting
         100
         (for-all [seeker gen-seeker]
                  (block-cut seeker)))

;; XVIII. Pasting

(defn stand-alone [seeker] true)
(defn in-between [seeker] true)
(defn new-lined [seeker] true)
(defn concatenated [seeker] true)
(defn overriden [seeker] true)

(defspec pasting
         100
         (for-all [seeker gen-seeker]
                  (stand-alone seeker)
                  (in-between seeker)
                  (new-lined seeker)
                  (concatenated seeker)
                  (overriden seeker)))