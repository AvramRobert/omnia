(ns omnia.input-test
  (require [omnia.input :as i]
           [clojure.test :refer [is]]
           [clojure.test.check.clojure-test :refer [defspec]]
           [clojure.test.check.properties :refer [for-all]]
           [clojure.test.check.generators :as gen]))

(defn rand-y [seeker]
  (-> seeker (i/height) (rand-int)))

(defn rand-x
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

(defn just-one [generator] (rand-nth (gen/sample generator)))
(defn many
  ([generator] (many (rand-int 100)))
  ([generator n] (vec (repeatedly n #(just-one generator)))))
;; FIXME: more-than combinator?

(def gen-line (->> gen/char-alphanumeric
                   (gen/vector)
                   (gen/such-that (comp not empty?))))

(def gen-text (->> gen-line
                   (gen/vector)
                   (gen/such-that (comp not empty?))))

(def gen-seeker (->> gen-text
                     (gen/fmap i/seeker)
                     (gen/fmap #(let [y (rand-y %)
                                      x (rand-x % y)]
                                  (i/move % (fn [_] [x y]))))))

(def ^:dynamic *benchmarks* [])

(defn bench [f n]
  (letfn [(avg [a] (/ a n))]
    (->> (range 0 n)
         (mapv (fn [_]
                 (let [seeker (first (gen/sample gen-seeker))]
                   (->> (omnia.more/time-return (f seeker))
                        (first)
                        (drop-last 3)
                        (apply str)
                        (Double/parseDouble)))))
         (reduce +)
         (avg)
         (format "Avg time: %s ms"))))

(defmacro defbench [name f]
  (let [ns-f# (-> (ns-name *ns*)
                  (str "/" f)
                  (symbol)
                  (resolve))]
    (alter-var-root
      #'*benchmarks*
      #(conj % [ns-f# (str name)]))))

(defn bench-all! [n]
  (run!
    (fn [[f desc]]
      (let [_ (println "Benchmarking `" desc "`")
            result-string (bench f n)]
        (println result-string)
        (println))) *benchmarks*))

;; I. Peering

(defn peer-start [seeker]
  (-> (i/start seeker)
      (i/peer (fn [_ b] b))
      (<=> seeker)))

(defn peer-end [seeker]
  (-> (i/end seeker)
      (i/peer (fn [a _] a))
      (<=> (i/rebase seeker #(drop-last %)))))

(defn peer-middle-insert-substitution [seeker line]
  (let [[_ y] (:cursor seeker)]
    (-> seeker
        (i/peer (fn [a b] (concat a [line] b)))
        (i/line)
        (= line)
        (is))))

(defn peer-middle-insert-invariance [seeker line]
  (let [[_ y] (:cursor seeker)]
    (-> seeker
        (i/peer (fn [a b] (concat a [line] b)))
        (i/peer (fn [a [b & c]] (concat a c)))
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

(defn peering [seeker]
  (peer-start seeker)
  (peer-middle-insert-substitution seeker (just-one gen-line))
  (peer-middle-insert-invariance seeker (just-one gen-line))
  (peer-middle-delete-left seeker)
  (peer-middle-delete-right seeker)
  (peer-empty-insert seeker)
  (peer-end seeker))

(defspec peering-test
         100
         (for-all [seeker gen-seeker]
                  (peering seeker)))

(defbench peering-bench peering)

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

(defn split-enhance [seeker line]
  (-> seeker
      (i/split (fn [a b] [line (concat a b)]))
      (i/line)
      (= line)
      (is)))

(defn split-replace [seeker text]
  (-> seeker
      (i/split (fn [_ _] text))
      (i/line)
      (= (first text))
      (is)))

(defn split-empty-insert [seeker]
  (-> (i/seeker [])
      (i/split (fn [a b] [(concat a (i/line seeker) b)]))
      (i/line)
      (= (i/line seeker))
      (is)))

(defn splitting [seeker]
  (split-reduce-left seeker)
  (split-reduce-right seeker)
  (split-enhance seeker (just-one gen-line))
  (split-replace seeker (just-one gen-text))
  (split-empty-insert seeker))

(defspec splitting-test
         100
         (for-all [seeker gen-seeker]
                  (splitting seeker)))

(defbench splitting-bench splitting)

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

(defn slice-enhance [seeker line]
  (let [expected  (->> seeker (i/line) (concat line))]
    (-> seeker
        (i/slice (fn [a b] (concat line a b)))
        (i/line)
        (= expected)
        (is))))

(defn slice-replace [seeker line]
  (-> seeker
      (i/slice (fn [_ _] line))
      (i/line)
      (= line)
      (is)))

(defn slice-empty-insert [seeker]
  (let [line (i/line seeker)]
    (-> (i/seeker [])
        (i/slice (fn [a b] (concat a line b)))
        (i/line)
        (= line)
        (is))))

(defn slicing [seeker]
  (slice-reduce-left seeker)
  (slice-reduce-right seeker)
  (slice-enhance seeker (just-one gen-line))
  (slice-replace seeker (just-one gen-line))
  (slice-empty-insert seeker))

(defspec slicing-test
         100
         (for-all [seeker gen-seeker]
                  (slicing seeker)))

(defbench slicing-bench slicing)

;; IV. Moving

(defn bounded [seeker]
  (let [cursor  (:cursor seeker)
        x-moved (i/move-x seeker #(+ 1000 %))
        y-moved (i/move-y seeker #(+ 1000 %))]
    (is (= cursor (:cursor x-moved)))
    (is (= cursor (:cursor y-moved)))))

(defn unbounded [seeker line]
  (-> seeker
      (i/split (fn [a b] [a line b]))
      (i/move-y inc)
      (i/start-x)
      (can-be #(-> % (i/line) (= line))
              #(-> % (i/center) (= (first line))))))

(defn moving [seeker]
  (bounded seeker)
  (unbounded seeker (just-one gen-line)))

(defspec moving-test
         100
         (for-all [seeker gen-seeker]
                  (moving seeker)))

(defbench moving-bench moving)

;; V. Retrieving

(defn previous [seeker text]
  (let [[line1 line2] text]
    (-> seeker
        (i/peer (fn [a b] (concat a text b)))
        (i/start-x)
        (i/move-y inc)
        (can-be #(-> % (i/regress) (i/left) (= (last line1)))
                #(-> % (i/move-y dec) (i/move-x inc) (i/left) (= (first line1)))))))

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

(defn retrieving [seeker]
  (previous seeker (many gen-line 2))
  (current seeker)
  (following seeker))

(defspec retrieving-test
         100
         (for-all [seeker gen-seeker]
                  (retrieving seeker)))

(defbench retrieving-bench retrieving)

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

(defn advancing [seeker]
  (increment seeker)
  (increment-stop seeker)
  (post-wrap seeker))

(defspec advancing-test
         100
         (for-all [seeker gen-seeker]
                  (advancing seeker)))

(defbench advancing-bench advancing)

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

(defn regressing [seeker]
  (decrement seeker)
  (decrement-stop seeker)
  (pre-wrap seeker))

(defspec regressing-test
         100
         (for-all [seeker gen-seeker]
                  (regressing seeker)))

(defbench regressing-bench regressing)

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

(defn climbing [seeker]
  (ascend seeker)
  (ascend-reset seeker))

(defspec climbing-test
         100
         (for-all [seeker gen-seeker]
                  (climbing seeker)))

(defbench climbing-bench climbing)

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

(defn falling [seeker]
  (descend seeker)
  (descend-reset seeker))

(defspec falling-test
         100
         (for-all [seeker gen-seeker]
                  (falling seeker)))

(defbench falling-bench falling)

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
        round       [[\( \)]]
        brackets    [[\[ \]]]
        squiggly    [[\{ \}]]
        quote       [[\" \"]]]
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

(defn chunked [seeker]
  (-> seeker
      (i/select-all)
      (i/delete)
      (<=> (i/seeker [[]]))))

(defn deleting [seeker]
  (backward seeker)
  (forward seeker)
  (paired seeker)
  (omitted seeker)
  (chunked seeker))

(defspec deleting-test
         100
         (for-all [seeker gen-seeker]
                  (deleting seeker)))

(defbench deleting-bench deleting)

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

(defn inserting [seeker]
  (literal seeker (first (gen/sample gen/char-alpha-numeric)))
  (pairs seeker)
  (ignored seeker)
  (overridingly seeker))

(defspec inserting-test
         100
         (for-all [seeker gen-seeker]
                  (inserting seeker)))

(defbench inserting-bench inserting)

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

(defn jumping [seeker]
  (iso seeker)
  (until-spaces seeker)
  (over-lines seeker)
  (until-exprs seeker)
  (over-spaces seeker))

(defspec jumping-test
         100
         (for-all [seeker gen-seeker]
                  (jumping seeker)))

(defbench jumping-bench jumping)

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

(defn selecting [seeker]
  (single seeker)
  (jumps seeker)
  (lines seeker)
  (blocks seeker))

(defspec selecting-test
         100
         (for-all [seeker gen-seeker]
                  (selecting seeker)))


(defbench selecting-bench selecting)

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

(defn merging [seeker1]
  (let [seeker2 (first (gen/sample gen-seeker))]
    (additive seeker1 seeker2)
    (selective seeker1 seeker2)))

(defspec merging-test
         100
         (for-all [seeker gen-seeker]
                  (merging seeker)))

(defbench merging-bench merging)

;; XV. Expanding

(defn word [seeker]
  (let [start-word [\a \b]
        end-word [\c \d]
        some-text [(concat start-word [\space] end-word)]]
    (-> seeker
        (i/peer (fn [a b] (concat a some-text b)))
        (i/start-x)
        (can-be #(-> (i/expand %) (i/extract) (i/line) (= start-word))
                #(-> (i/jump-right %) (i/expand) (i/extract) (i/line) (= start-word))
                #(-> (i/end-x %) (i/expand) (i/extract) (i/line) (= end-word))
                #(-> (i/jump-right %) (i/jump-right) (i/expand) (i/extract) (i/line) (= end-word))))))

(defn expr [seeker]
  (reduce
    (fn [_ [l r]]
      (let [some-line [l l \a \b \space \space \c \d r r]]
        (-> seeker
            (i/peer (fn [a b] (concat a [some-line] b)))
            (can-be #(-> % (i/start-x) (i/expand) (i/extract) (i/line) (= some-line))
                    #(-> % (i/end-x) (i/expand) (i/extract) (i/line) (= some-line))
                    #_#(-> (i/start-x %)
                         (i/jump-right)
                         (i/jump-right)
                         (i/jump-right)
                         (i/expand)
                         (i/extract)
                         (i/line)
                         (= some-line))))))
    nil
    [[\( \)] [\[ \]] [\{ \}]]))

(defn scoped-expr [seeker]
  (reduce
    (fn [_ [l r]]
      (let [content [\a \b \c \d]
            some-line (concat [l] content [r])]
        (-> seeker
            (i/peer (fn [a b] (concat a [some-line] b)))
            (i/start-x)
            (i/move-x inc)
            (can-be #(-> % (i/expand) (i/extract) (i/line) (= content))
                    #(-> % (i/move-x inc) (i/move-x inc) (i/expand) (i/extract) (i/line) (= content))
                    #(-> % (i/jump-right) (i/expand) (i/extract) (i/line) (= content))))))
    nil
    [[\( \)] [\[ \]] [\{ \}]]))

(defn break-out [seeker]
  (let [parens [[\( \)] [\[ \]] [\{ \}]]]
    (reduce
      (fn [_ [ol or]]
        (reduce
          (fn [_ [il ir]]
            (let [inner-line [\c \d]
                  inner-expr (concat [il] inner-line [ir])
                  some-line (concat [ol] [\a \b] inner-expr [\e \f] [or])]
              (-> seeker
                  (i/peer (fn [a b] (concat a [some-line] b)))
                  (i/start-x)
                  (i/jump-right)
                  (i/jump-right)
                  (i/jump-right)
                  (i/jump-right)
                  (can-be #(-> (i/expand %) (i/extract) (i/line) (= inner-line))
                          #(-> (i/expand %) (i/expand) (i/extract) (i/line) (= inner-expr))
                          #(-> (i/expand %) (i/expand) (i/expand) (i/extract) (i/line) (= some-line))))))
          nil
          parens))
      nil
      parens)))

(defn expanding [seeker]
  (word seeker)
  (expr seeker)
  (scoped-expr seeker)
  (break-out seeker))

(defspec expanding-test
         100
         (for-all [seeker gen-seeker]
                  (expanding seeker)))


(defbench expanding-bench expanding)

;; XVI. Copying

(defn intra-line-copy [seeker]
  (let [left [\a \b]
        right [\space \c \d]
        some-text [(concat left right)]]
    (-> seeker
        (i/peer (fn [a b] (concat a some-text b)))
        (i/start-x)
        (i/select)
        (i/jump-right)
        (can-be #(-> % (i/copy) (:clipboard) (<=> (i/seeker [left])))))))

(defn inter-line-copy [seeker]
  (let [some-text [[\a \b] [\c \d]]]
    (-> seeker
        (i/peer (fn [a b] (concat a some-text b)))
        (i/start-x)
        (i/select)
        (i/fall)
        (i/end-x)
        (can-be #(-> % (i/copy) (:clipboard) (<=> (i/seeker some-text)))))))

(defn copying [seeker]
  (intra-line-copy seeker)
  (inter-line-copy seeker))

(defspec copying-test
         100
         (for-all [seeker gen-seeker]
                  (copying seeker)))

(defbench copying-bench copying)

;; XVII. Cutting

(defn intra-line-cut [seeker]
  (let [left [\a \b]
        right [\space \c \d]
        some-text [(concat left right)]]
    (-> seeker
        (i/peer (fn [a b] (concat a some-text b)))
        (i/start-x)
        (i/select)
        (i/jump-right)
        (can-be #(-> % (i/cut) (:clipboard) (<=> (i/seeker [left])))
                #(-> % (i/cut) (i/line) (= right))))))

(defn inter-line-cut [seeker]
  (let [some-text [[\a \b] [\c \d]]
        emptied (i/peer seeker #(concat %1 [[]] %2))]
    (-> seeker
        (i/peer (fn [a b] (concat a some-text b)))
        (i/start-x)
        (i/select)
        (i/fall)
        (i/end-x)
        (can-be #(-> % (i/cut) (:clipboard) (<=> (i/seeker some-text)))
                #(-> % (i/cut) (<=> emptied))))))

(defn cutting [seeker]
  (intra-line-cut seeker)
  (inter-line-cut seeker))

(defspec cutting-test
         100
         (for-all [seeker gen-seeker]
                  (cutting seeker)))

(defbench cutting-bench cutting)

;; XVIII. Pasting

(defn stand-alone [seeker]
  (let [some-line [\a \b]]
    (-> seeker
        (i/peer (fn [a b] (concat a [some-line] b)))
        (i/start-x)
        (i/select)
        (i/end-x)
        (i/copy)
        (can-be #(-> % (i/end-y) (i/start-x) (i/paste) (i/line) (= some-line))))))

(defn in-between [seeker]
  (let [some-line [\a \b]]
    (-> seeker
        (i/peer (fn [a b] (concat a [some-line] b)))
        (i/start-x)
        (i/select)
        (i/jump-right)
        (i/copy)
        (can-be #(-> (i/paste %) (i/line) (= (concat some-line some-line)))
                #(-> (i/paste %)
                     (i/regress)
                     (i/regress)
                     (i/paste)
                     (i/line) (= (concat some-line some-line some-line)))))))

(defn concatenated [seeker]
  (let [line-1 [\a \b]
        line-2 [\c \d]
        line-3 [\e \f]]
    (-> seeker
        (i/peer (fn [a b] (concat a [line-1 line-2 line-3] b)))
        (i/start-x)
        (i/select)
        (i/fall)
        (i/end-x)
        (i/copy)
        (can-be #(-> % (i/paste) (i/line) (= line-2))
                #(-> % (i/paste) (i/climb) (i/line) (= (concat line-2 line-1)))))))

;; FIXME
(defn overriden [seeker] true)

(defn pasting [seeker]
  (stand-alone seeker)
  (in-between seeker)
  (concatenated seeker)
  (overriden seeker))

(defspec pasting-test
         100
         (for-all [seeker gen-seeker]
                  (pasting seeker)))

(defbench pasting-bench pasting)

;; XIX. Pairing

(defn- chars-at [seeker {:keys [start end]}]
  (letfn [(f [there] (-> (i/move seeker (constantly there))
                         (i/center)))]
    [(f start)
     (f end)]))

(defn pair-outer [seeker]
  (let [parens [[\( \)] [\[ \]] [\{ \}]]]
    (reduce
      (fn [_ [l r]]
        (-> seeker
            (i/slice (fn [a b] (concat [l] a b [r])))
            (can-be #(->> (i/start-x %) (i/find-pair) (chars-at %) (= [l r]))
                    #(->> (i/end-x %) (i/regress) (i/find-pair) (chars-at %) (= [l r]))))) nil parens)))

(defn pair-inner [seeker line]
  (let [parens [[\( \)] [\[ \]] [\{ \}]]
        offset #(+ % 1 (count line))]
    (reduce
      (fn [_ [ol or]]
        (reduce
          (fn [_ [il ir]]
            (-> seeker
                (i/peer (fn [a b] (concat a [(concat [ol] [il] line [ir] [or])] b)))
                (i/start-x)
                (can-be #(->> (i/find-pair %) (chars-at %) (= [ol or]))
                        #(->> (i/advance %) (i/find-pair) (chars-at %) (= [il ir])))))
          nil parens))
      nil parens)))

(defn dont-pair [seeker]
  (let [parens [[\( \)] [\[ \]] [\{ \}]]]
    (reduce
      (fn [_ [l r]]
        (-> seeker
            (i/slice (fn [a b] (concat [l l l] a b [r r])))
            (i/start-x)
            (can-be #(->> (i/find-pair %) (nil?))
                    #(->> (i/advance %) (i/find-pair) (chars-at %) (= [l r]))
                    #(->> (i/advance %) (i/advance) (i/find-pair) (chars-at %) (= [l r]))))) nil parens)))

;; FIXME
(defn look-correctly [seeker] true)

(defn pairing [seeker]
  (pair-outer seeker)
  (pair-inner seeker (just-one gen-line))
  (dont-pair seeker)
  (look-correctly seeker))

(defspec pairing-test
         100
         (for-all [seeker gen-seeker]
                  (pairing seeker)))

(defbench pairing-bench pairing)

;; XX. Balancing

(clojure.test/deftest balancing
  (let [s1 (i/seeker [[\[ \[ \[ \4 \5 \] \]]])
        s2 (i/seeker [[\[ \( \a \{ \} \b \) \]]])
        s3 (i/seeker [[\[ \( \} \) \]]])
        s4 (i/seeker [[\a \b \4 \( \)]])]
    ;; s1
    (is (= :unbalanced (-> (i/balance s1) (first))))
    (is (= :balanced (-> (i/advance s1) (i/balance) (first))))
    (is (= :balanced (-> (i/advance s1) (i/advance) (i/balance) (first))))
    (is (= :unbalanced (-> (i/advance s1) (i/advance) (i/advance) (i/balance) (first))))
    (is (= :unbalanced (-> (i/end s1) (i/balance) (first))))
    (is (= :unbalanced (-> (i/start s1) (i/nearest) (i/balance) (first))))
    (is (= :unbalanced (-> (i/advance s1) (i/nearest) (i/balance) (first))))
    (is (= :unbalanced (-> (i/end-x s1) (i/nearest) (i/balance) (first))))
    (is (= :balanced (-> (i/advance s1) (i/advance) (i/nearest) (i/balance) (first))))
    (is (= :balanced (-> (i/end-x s1) (i/regress) (i/nearest) (i/balance) (first))))
    ;;;; s2
    (is (= :balanced (-> (i/balance s2) (first))))
    (is (= :balanced (-> (i/advance s2) (i/balance) (first))))
    (is (= :unbalanced (-> (i/advance s2) (i/advance) (i/balance) (first))))
    (is (= :balanced (-> (i/advance s2) (i/advance) (i/advance) (i/balance) (first))))
    (is (= :balanced (-> (i/end-x s2) (i/regress) (i/nearest) (i/balance) (first))))
    (is (= :balanced (-> (i/end-x s2) (i/regress) (i/regress) (i/nearest) (i/balance) (first))))
    ;;;; s3
    (is (= :unbalanced (-> (i/balance s3) (first))))
    (is (= :unbalanced (-> (i/advance s3) (i/balance) (first))))
    (is (= :unbalanced (-> (i/advance s3) (i/advance) (i/balance) (first))))
    (is (= :unbalanced (-> (i/advance s3) (i/advance) (i/advance) (i/balance) (first))))
    (is (= :unbalanced (-> (i/end-x s3) (i/regress) (i/balance) (first))))
    (is (= :unbalanced (-> (i/end-x s3) (i/regress) (i/regress) (i/balance) (first))))
    (is (= :unbalanced (-> (i/end-x s3) (i/regress) (i/regress) (i/regress) (i/balance) (first))))
    ;;; s4
    (is (= :unbalanced (-> (i/balance s4) (first))))
    (is (= :unbalanced (-> (i/advance s4) (i/balance) (first))))
    (is (= :unbalanced (-> (i/advance s4) (i/advance) (i/balance) (first))))
    (is (= :balanced (-> (i/advance s4) (i/advance) (i/advance) (i/balance) (first))))
    (is (= :balanced (-> (i/end-x s4) (i/regress) (i/nearest) (i/balance) (first))))
    (is (= :unbalanced (-> (i/end-x s4) (i/regress) (i/regress) (i/nearest) (i/balance) (first))))
    (is (= :unbalanced (-> (i/end-x s4) (i/regress) (i/regress) (i/regress) (i/nearest) (i/balance) (first))))))
