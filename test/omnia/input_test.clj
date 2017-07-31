(ns omnia.input-test
  (require [omnia.input :as i]
           [omnia.more :refer [time-return]]
           [clojure.test :refer [is]]
           [clojure.test.check.clojure-test :refer [defspec]]
           [clojure.test.check.properties :refer [for-all]]
           [clojure.test.check.generators :as gen]
           [omnia.test-utils :refer :all]))

(def ^:dynamic *benchmarks* [])

(defn bench [f n]
  (letfn [(avg [a] (/ a n))]
    (->> (range 0 n)
         (mapv (fn [_]
                 (let [seeker (first (gen/sample gen-seeker))]
                   (->> (time-return (f seeker))
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

;; I. Peering

(defn peer-at-start [seeker]
  (-> (i/start seeker)
      (i/peer (fn [_ b] b))
      (<=> seeker)))

(defn peer-at-end [seeker]
  (-> (i/end seeker)
      (i/peer (fn [a _] a))
      (<=> (i/rebase seeker #(drop-last %)))))

(defn peer-inserting-line [seeker line]
  (let [[_ y] (:cursor seeker)]
    (-> seeker
        (i/peer (fn [a b] (concat a [line] b)))
        (i/line)
        (= line)
        (is))))

(defn peer-inserting-and-removing-line [seeker line]
  (let [[_ y] (:cursor seeker)]
    (-> seeker
        (i/peer (fn [a b] (concat a [line] b)))
        (i/peer (fn [a [b & c]] (concat a c)))
        (<=> seeker))))

(defn peer-removing-previous-lines [seeker]
  (let [[_ y] (:cursor seeker)]
    (-> (i/peer seeker (fn [_ b] b))
        (<=> (i/rebase seeker #(drop y %))))))

(defn peer-removing-next-lines [seeker]
  (let [[_ y] (:cursor seeker)]
    (-> (i/peer seeker (fn [a _] a))
        (<=> (i/rebase seeker #(take y %))))))

(defn peer-creating-line-when-empty [seeker]
  (-> (i/seeker [])
      (i/peer (fn [a b] (concat a (:lines seeker) b)))
      (<=> seeker)))

(defn peering [seeker]
  (peer-at-start seeker)
  (peer-at-end seeker)
  (peer-inserting-line seeker (just-one gen-line))
  (peer-inserting-and-removing-line seeker (just-one gen-line))
  (peer-removing-previous-lines seeker)
  (peer-removing-next-lines seeker)
  (peer-creating-line-when-empty seeker))

(defspec peering-test
         100
         (for-all [seeker gen-seeker]
                  (peering seeker)))

(defbench peering-bench peering)

;; II. Splitting

(defn split-removing-line-start [seeker]
  (let [[x y] (:cursor seeker)
        expected (->> seeker (i/line) (drop x))]
    (-> seeker
        (i/split (fn [_ b] [b]))
        (i/line)
        (= expected)
        (is))))

(defn split-removing-line-end [seeker]
  (let [[x y] (:cursor seeker)
        expected (->> seeker (i/line) (take x))]
    (-> seeker
        (i/split (fn [a _] [a]))
        (i/line)
        (= expected)
        (is))))

(defn split-creating-new-line [seeker line]
  (-> seeker
      (i/split (fn [a b] [line (concat a b)]))
      (i/line)
      (= line)
      (is)))

(defn split-enhancing-line [seeker line]
  (-> seeker
      (i/split (fn [a b] [(concat a line b)]))
      (i/split (fn [a b] [(concat a (drop (count line) b))]))
      (<=> seeker)))

(defn split-replacing-line [seeker text]
  (-> seeker
      (i/split (fn [_ _] text))
      (i/line)
      (= (first text))
      (is)))

(defn split-creating-line-when-empty [seeker]
  (-> (i/seeker [])
      (i/split (fn [a b] [(concat a (i/line seeker) b)]))
      (i/line)
      (= (i/line seeker))
      (is)))

(defn splitting [seeker]
  (split-removing-line-start seeker)
  (split-removing-line-end seeker)
  (split-creating-new-line seeker (just-one gen-line))
  (split-enhancing-line seeker (just-one gen-line))
  (split-replacing-line seeker (just-one gen-text))
  (split-creating-line-when-empty seeker))

(defspec splitting-test
         100
         (for-all [seeker gen-seeker]
                  (splitting seeker)))

(defbench splitting-bench splitting)

;; III. Slicing

(defn slice-removing-left-of-line [seeker]
  (let [[x _] (:cursor seeker)
        expected (->> seeker (i/line) (take x))]
    (-> seeker
        (i/slice (fn [a _] a))
        (i/line)
        (= expected)
        (is))))

(defn slice-removing-right-of-line [seeker]
  (let [[x _] (:cursor seeker)
        expected (->> seeker (i/line) (drop x))]
    (-> seeker
        (i/slice (fn [_ b] b))
        (i/line)
        (= expected)
        (is))))

(defn slice-enhancing-line [seeker line]
  (let [expected  (->> seeker (i/line) (concat line))]
    (-> seeker
        (i/slice (fn [a b] (concat line a b)))
        (i/line)
        (= expected)
        (is))))

(defn slice-replacing-line [seeker line]
  (-> seeker
      (i/slice (fn [_ _] line))
      (i/line)
      (= line)
      (is)))

(defn slice-creating-line-when-empty [seeker]
  (let [line (i/line seeker)]
    (-> (i/seeker [])
        (i/slice (fn [a b] (concat a line b)))
        (i/line)
        (= line)
        (is))))

(defn slicing [seeker]
  (slice-removing-left-of-line seeker)
  (slice-removing-right-of-line seeker)
  (slice-enhancing-line seeker (just-one gen-line))
  (slice-replacing-line seeker (just-one gen-line))
  (slice-creating-line-when-empty seeker))

(defspec slicing-test
         100
         (for-all [seeker gen-seeker]
                  (slicing seeker)))

(defbench slicing-bench slicing)

;; IV. Moving

(defn bound-movement [seeker]
  (let [cursor  (:cursor seeker)
        x-moved (i/move-x seeker #(+ 1000 %))
        y-moved (i/move-y seeker #(+ 1000 %))]
    (is (= cursor (:cursor x-moved)))
    (is (= cursor (:cursor y-moved)))))

(defn unbound-movement [seeker line]
  (-> seeker
      (i/split (fn [a b] [a line b]))
      (i/move-y inc)
      (i/start-x)
      (can-be #(= (i/line %) line)
              #(= (i/center %) (first line)))))

(defn moving [seeker]
  (bound-movement seeker)
  (unbound-movement seeker (just-one gen-line)))

(defspec moving-test
         100
         (for-all [seeker gen-seeker]
                  (moving seeker)))

(defbench moving-bench moving)

;; V. Retrieving

(defn get-previous-char [seeker text]
  (let [[line1 line2] text]
    (-> seeker
        (i/peer (fn [a b] (concat a text b)))
        (i/start-x)
        (i/move-y inc)
        (can-be #(-> (i/regress %) (i/left) (= (last line1)))
                #(-> (i/move-y % dec) (i/move-x inc) (i/left) (= (first line1)))))))

(defn get-current-char [seeker]
  (let [expected (-> (:lines seeker) (first) (first))]
    (-> (i/start seeker)
        (i/center)
        (= expected)
        (is))))

(defn get-next-char [seeker line1 line2]
  (let [some-text [[\a \b] [\c \d]]]
    (-> (i/end seeker)
        (i/peer (fn [a b] (concat a [line1 line2] b)))
        (i/start-x)
        (can-be #(-> (i/end-x %) (i/right) (= (first line2)))
                #(= (i/right %) (second line1))))))

(defn retrieving [seeker]
  (get-previous-char seeker (many gen-line 2))
  (get-current-char seeker)
  (get-next-char seeker (just-one gen-line) (just-one gen-line)))

(defspec retrieving-test
         100
         (for-all [seeker gen-seeker]
                  (retrieving seeker)))

(defbench retrieving-bench retrieving)

;; VI. Advancing

(defn simple-advance [seeker line]
  (-> (i/start seeker)
      (i/split (fn [a b] [line a b]))
      (can-be #(-> % (i/advance) (after? %)))))

(defn stop-advance-when-text-ends [seeker]
  (-> (i/end seeker)
      (can-be #(-> % (i/advance) (there? %)))))

(defn wrap-when-line-ends [seeker text]
  (-> (i/start seeker)
      (i/peer (fn [a b] (concat text a b)))
      (i/end-x)
      (can-be #(-> % (i/advance) (after? %)))))

(defn advancing [seeker]
  (simple-advance seeker (just-one gen-line))
  (stop-advance-when-text-ends seeker)
  (wrap-when-line-ends seeker (just-one gen-text)))

(defspec advancing-test
         100
         (for-all [seeker gen-seeker]
                  (advancing seeker)))

(defbench advancing-bench advancing)

;; VII. Regressing

(defn simple-regress [seeker line]
  (-> seeker
      (i/split (fn [a b] [a line b]))
      (can-be #(-> % (i/advance) (i/regress) (there? %)))))

(defn stop-regress-when-start [seeker]
  (-> (i/start-x seeker)
      (i/regress)
      (there? seeker)))

(defn wrap-when-line-starts [seeker text]
  (-> seeker
      (i/peer (fn [a b] (concat a b text)))
      (i/end)
      (i/start-x)
      (can-be #(-> % (i/regress) (before? %)))))

(defn regressing [seeker]
  (simple-regress seeker (just-one gen-line))
  (stop-regress-when-start seeker)
  (wrap-when-line-starts seeker (just-one gen-text)))

(defspec regressing-test
         100
         (for-all [seeker gen-seeker]
                  (regressing seeker)))

(defbench regressing-bench regressing)

;; VIII. Climbing

(defn simple-ascent [seeker text]
  (-> seeker
      (i/peer (fn [a b] (concat a text b)))
      (i/move-y inc)
      (can-be #(-> (i/climb %) (before? %)))))

(defn ascent-to-end-of-line [seeker]
  "explicit data, because second line has to be larger than the first"
  (let [some-text [[\a \b] [\a \b \c]]]
    (-> seeker
        (i/peer (fn [a b] (concat a some-text b)))
        (can-be #(-> (i/move-y % inc) (i/end-x) (i/climb) (there? (i/end-x %)))))))

(defn climbing [seeker]
  (simple-ascent seeker (just-one gen-text))
  (ascent-to-end-of-line seeker))

(defspec climbing-test
         100
         (for-all [seeker gen-seeker]
                  (climbing seeker)))

(defbench climbing-bench climbing)

;; IX. Falling

(defn simple-descent [seeker text]
  (-> seeker
      (i/peer (fn [a b] (concat a text b)))
      (can-be #(-> % (i/fall) (after? %)))))

(defn descent-to-end-of-line [seeker]
  "explicit data, because first line has to be larger than the second"
  (let [some-text [[\a \b \c] [\d \e]]]
    (-> seeker
        (i/peer (fn [a b] (concat a some-text b)))
        (i/move-y inc)
        (can-be #(-> (i/move-y % dec) (i/end-x) (i/fall) (there? (i/end-x %)))))))

(defn falling [seeker]
  (simple-descent seeker (just-one gen-text))
  (descent-to-end-of-line seeker))

(defspec falling-test
         100
         (for-all [seeker gen-seeker]
                  (falling seeker)))

(defbench falling-bench falling)

;; X. Deleting

(defn delete-previous [seeker]
  (let [some-text [[\a \b]]]
    (-> seeker
        (i/peer (fn [a b] (concat a b some-text)))
        (i/end)
        (can-be #(-> % (i/delete) (i/left) (= \a))
                #(-> % (i/delete) (i/delete) (i/left) (nil?))
                #(-> % (i/delete) (i/delete) (i/delete) (<=> seeker))))))

(defn delete-next [seeker]
  (let [some-text [[\a \b]]]
    (-> seeker
        (i/peer (fn [a b] (concat some-text a b)))
        (i/start)
        (can-be #(-> % (i/munch) (i/center) (= \b))
                #(-> % (i/munch) (i/munch) (i/center) (nil?))
                #(-> % (i/munch) (i/munch) (i/munch) (<=> seeker))))))

(defn delete-pairs [seeker]
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

(defn delete-omitting-single-parens [seeker]
  (letfn [(f [s c] (i/slicel s #(conj % c)))]
    (-> (i/end seeker)
        (can-be #(-> (f % \)) (i/advance) (i/delete) (<=> (f % \))))
                #(-> (f % \]) (i/advance) (i/delete) (<=> (f % \])))
                #(-> (f % \}) (i/advance) (i/delete) (<=> (f % \})))
                #(-> (f % \") (i/advance) (i/delete) (<=> (f % \")))))))

(defn delete-selections [seeker]
  (-> seeker
      (i/select-all)
      (i/delete)
      (<=> (i/seeker [[]]))))

(defn deleting [seeker]
  (delete-previous seeker)
  (delete-next seeker)
  (delete-pairs seeker)
  (delete-omitting-single-parens seeker)
  (delete-selections seeker))

(defspec deleting-test
         100
         (for-all [seeker gen-seeker]
                  (deleting seeker)))

(defbench deleting-bench deleting)

;; XI. Inserting

(defn insert-literal [seeker c]
  (-> (i/insert seeker c)
      (i/left)
      (= c)
      (is)))

(defn insert-pairs [seeker]
  (letfn [(f [s l r] (i/slicel s #(conj % l r)))]
    (-> (i/end seeker)
        (can-be
          #(-> % (i/insert \() (<=> (f % \( \))))
          #(-> % (i/insert \[) (<=> (f % \[ \])))
          #(-> % (i/insert \{) (<=> (f % \{ \})))
          #(-> % (i/insert \") (<=> (f % \" \")))))))

(defn insert-ignoring-simple-parens [seeker]
  (letfn [(f [s l r] (i/slicel s #(conj % l r)))]
    (-> (i/end seeker)
        (can-be
          #(-> % (i/insert \() (i/insert \)) (<=> (f % \( \))))
          #(-> % (i/insert \[) (i/insert \]) (<=> (f % \[ \])))
          #(-> % (i/insert \{) (i/insert \}) (<=> (f % \{ \})))
          #(-> % (i/insert \") (i/insert \") (<=> (f % \" \")))))))

(defn insert-replacing-selection [seeker some-text c]
  (let [expected (i/peer seeker (fn [a b] (concat a [[c]] b)))]
    (-> seeker
        (i/peer (fn [a b] (concat a some-text b)))
        (i/start-x)
        (i/select)
        (i/move-y #(-> (count some-text) (dec) (+ %)))
        (i/end-x)
        (i/insert c)
        (<=> expected))))

(defn inserting [seeker]
  (insert-literal seeker (just-one gen/char-alphanumeric))
  (insert-pairs seeker)
  (insert-ignoring-simple-parens seeker)
  (insert-replacing-selection seeker
                              (just-one gen-text)
                              (just-one gen/char-alphanumeric)))

(defspec inserting-test
         100
         (for-all [seeker gen-seeker]
                  (inserting seeker)))

(defbench inserting-bench inserting)

;; XII. Jumping

(defn jump-over-words [seeker text]
  (-> seeker
      (i/peer (fn [a b] (concat a text b)))
      (i/start-x)
      (can-be #(-> % (i/jump-right) (i/jump-left) (there? %)))))

(defn jump-until-spaces [seeker line1 line2]
  (-> seeker
      (i/peer (fn [a b] (concat a [(concat line1 [\space] line2)] b)))
      (can-be #(-> % (i/start-x) (i/jump-right) (i/center) (= \space))
              #(-> % (i/end-x) (i/jump-left) (i/left) (= \space)))))

(defn jump-until-expr-ends [seeker line]
  (letfn [(f [s c]
            (-> (i/peer s #(concat %1 [(conj line c)] %2))
                (i/start-x)))]
    (can-be seeker
            #(-> (f % \() (i/jump-right) (i/center) (= \())
            #(-> (f % \[) (i/jump-right) (i/center) (= \[))
            #(-> (f % \{) (i/jump-right) (i/center) (= \{))
            #(-> (f % \") (i/jump-right) (i/center) (= \"))
            #(-> (f % \)) (i/jump-right) (i/center) (= \)))
            #(-> (f % \]) (i/jump-right) (i/center) (= \]))
            #(-> (f % \}) (i/jump-right) (i/center) (= \})))))

(defn jump-between-lines [seeker]
  (let [some-text [[\a \b]]]
    (-> seeker
        (i/peer (fn [a b] (concat a some-text b)))
        (i/end-x)
        (can-be #(-> % (i/jump-right) (after? %))))))

(defn jump-over-spaces [seeker line1 line2]
  (let [spaces [\space \space \space]]
    (-> seeker
        (i/peer (fn [a b] (concat a [(concat line1 spaces line2)] b)))
        (i/start-x)
        (can-be #(-> (i/center %) (= (first line1)))
                #(-> (i/jump-right %) (i/jump-right) (i/center) (= (first line2)))
                #(-> (i/end-x %) (i/jump-left) (i/center) (= (first line2)))
                #(-> (i/end-x %) (i/jump-left) (i/jump-left) (i/left) (= (last line1)))
                #(-> (i/end-x %) (i/jump-left) (i/jump-left) (i/jump-left) (i/center) (= (first line1)))))))

(defn jumping [seeker]
  (jump-over-words seeker (just-one gen-text))
  (jump-between-lines seeker)
  (jump-until-expr-ends seeker (just-one gen-line))
  (jump-until-spaces seeker
                     (just-one gen-line)
                     (just-one gen-line))
  (jump-over-spaces seeker
                    (just-one gen-line)
                    (just-one gen-line)))

(defspec jumping-test
         100
         (for-all [seeker gen-seeker]
                  (jumping seeker)))

(defbench jumping-bench jumping)

;; XIII. Selecting

(defn select-single-chars [seeker text]
  (-> seeker
      (i/peer (fn [a b] (concat a text b)))
      (i/start-x)
      (i/select)
      (i/move-x inc)
      (i/selection)
      (can-be #(-> (:start %) (first) (zero?))
              #(-> (:end %) (first) (= 1)))))

(defn select-when-jumping [seeker]
  (-> seeker
      (i/start-x)
      (i/select)
      (i/end-x)
      (can-be #(-> (i/selection %) (:start) (= (:cursor (i/start-x %))))
              #(-> (i/selection %) (:end) (= (:cursor (i/end-x %)))))))

(defn select-lines [seeker text]
  (-> seeker
      (i/peer (fn [a b] (concat a text b)))
      (i/start-x)
      (i/select)
      (i/fall)
      (can-be #(-> (i/selection %) (:start) (= (:cursor (i/climb %))))
              #(-> (i/selection %) (:end) (= (:cursor %))))))

(defn select-blocks [seeker]
  (-> seeker
      (i/select-all)
      (can-be #(-> (i/selection %) (:start) (= (:cursor (i/start %))))
              #(-> (i/selection %) (:end) (= (:cursor (i/end %)))))))

(defn selecting [seeker]
  (select-single-chars seeker (just-one gen-text))
  (select-when-jumping seeker)
  (select-lines seeker (just-one gen-text))
  (select-blocks seeker))

(defspec selecting-test
         100
         (for-all [seeker gen-seeker]
                  (selecting seeker)))


(defbench selecting-bench selecting)

;; XVI. Joining

(defn join-texts [seeker1 seeker2]
  (-> (i/join seeker1 seeker2)
      (can-be #(<=> % (i/seeker (concat (:lines seeker1)
                                        (:lines seeker2))))
              #(-> (i/line %) (= (i/line seeker2))))))

(defn join-texts-with-selections [seeker1 seeker2]
  (let [s1 (-> seeker1 (i/select) (i/end))
        s2 (-> seeker2 (i/select) (i/end))]
    (-> (i/join s1 s2)
        (can-be #(-> (i/selection %) (:end) (= (:cursor (i/end %))))))))

(defn joining [seeker1]
  (let [seeker2 (first (gen/sample gen-seeker))]
    (join-texts seeker1 seeker2)
    (join-texts-with-selections seeker1 seeker2)))

(defspec joining-test
         100
         (for-all [seeker gen-seeker]
                  (joining seeker)))

(defbench joining-bench joining)

;; XV. Expanding

(defn expand-to-words [seeker line1 line2]
  (let [some-text [(concat line1 [\space] line2)]]
    (-> seeker
        (i/peer (fn [a b] (concat a some-text b)))
        (i/start-x)
        (can-be #(-> (i/expand %) (i/extract) (i/line) (= line1))
                #(-> (i/jump-right %) (i/expand) (i/extract) (i/line) (= line1))
                #(-> (i/end-x %) (i/expand) (i/extract) (i/line) (= line2))
                #(-> (i/jump-right %) (i/jump-right) (i/expand) (i/extract) (i/line) (= line2))))))

(defn expand-from-words-to-expr [seeker]
  (reduce
    (fn [_ [l r]]
      (let [some-line [l l \a \b \space \space \c \d r r]]
        (-> seeker
            (i/peer (fn [a b] (concat a [some-line] b)))
            (can-be #(-> % (i/start-x) (i/expand) (i/extract) (i/line) (= some-line))
                    #(-> % (i/end-x) (i/expand) (i/extract) (i/line) (= some-line))
                    #(-> (i/start-x %)
                         (i/jump-right)
                         (i/jump-right)
                         (i/jump-right)
                         (i/advance)
                         (i/expand)
                         (i/expand)
                         (i/extract)
                         (i/line)
                         (= some-line))))))
    nil
    [[\( \)] [\[ \]] [\{ \}]]))

(defn expand-over-exprs [seeker line]
  (reduce
    (fn [_ [l r]]
      (let [some-line (concat [l] line [r])]
        (-> seeker
            (i/peer (fn [a b] (concat a [some-line] b)))
            (i/start-x)
            (i/move-x inc)
            (can-be #(-> % (i/expand) (i/extract) (i/line) (= line))
                    #(-> % (i/jump-right) (i/expand) (i/extract) (i/line) (= line))))))
    nil
    [[\( \)] [\[ \]] [\{ \}]]))

(defn expand-from-inner-to-outer-expr [seeker line1 line2 line3]
  (let [parens [[\( \)] [\[ \]] [\{ \}]]]
    (reduce
      (fn [_ [ol or]]
        (reduce
          (fn [_ [il ir]]
            (let [inner-expr (concat [il] line1 [ir])
                  some-line (concat [ol] line2 inner-expr line3 [or])]
              (-> seeker
                  (i/peer (fn [a b] (concat a [some-line] b)))
                  (i/start-x)
                  (i/jump-right)
                  (i/jump-right)
                  (i/jump-right)
                  (can-be #(-> (i/expand %) (i/extract) (i/line) (= line1))
                          #(-> (i/expand %) (i/expand) (i/extract) (i/line) (= inner-expr))
                          #(-> (i/expand %) (i/expand) (i/expand) (i/extract) (i/line) (= some-line))))))
          nil
          parens))
      nil
      parens)))

(defn expanding [seeker]
  (expand-to-words seeker (just-one gen-line) (just-one gen-line))
  (expand-from-words-to-expr seeker)
  (expand-over-exprs seeker (just-one gen-line))
  (expand-from-inner-to-outer-expr seeker
                                   (just-one gen-line)
                                   (just-one gen-line)
                                   (just-one gen-line)))

(defspec expanding-test
         100
         (for-all [seeker gen-seeker]
                  (expanding seeker)))


(defbench expanding-bench expanding)

;; XVI. Copying

(defn copy-within-line [seeker line1 line2]
  (let [some-text [(concat line1 [\space] line2)]]
    (-> seeker
        (i/peer (fn [a b] (concat a some-text b)))
        (i/start-x)
        (i/select)
        (i/jump-right)
        (can-be #(-> (i/copy %) (:clipboard) (<=> (i/seeker [line1])))))))

(defn copy-lines [seeker some-text]
  (-> seeker
      (i/peer (fn [a b] (concat a some-text b)))
      (i/start-x)
      (i/select)
      (i/move-y #(-> (count some-text) (dec) (+ %)))
      (i/end-x)
      (can-be #(-> (i/copy %) (:clipboard) (<=> (i/seeker some-text))))))

(defn copying [seeker]
  (copy-within-line seeker (just-one gen-line) (just-one gen-line))
  (copy-lines seeker (just-one gen-text)))

(defspec copying-test
         100
         (for-all [seeker gen-seeker]
                  (copying seeker)))

(defbench copying-bench copying)

;; XVII. Cutting

(defn cut-within-line [seeker line1 line2]
  (let [some-text [(concat line1 [\space] line2)]]
    (-> seeker
        (i/peer (fn [a b] (concat a some-text b)))
        (i/start-x)
        (i/select)
        (i/jump-right)
        (can-be #(-> (i/cut %) (:clipboard) (<=> (i/seeker [line1])))
                #(<=> (i/cut %) (i/delete %))))))

(defn cut-lines [seeker some-text]
  (-> seeker
      (i/peer (fn [a b] (concat a some-text b)))
      (i/start-x)
      (i/select)
      (i/move-y #(-> (count some-text) (dec) (+ %)))
      (i/end-x)
      (can-be #(-> (i/cut %) (:clipboard) (<=> (i/seeker some-text)))
              #(<=> (i/cut %) (i/delete %)))))

(defn cutting [seeker]
  (cut-within-line seeker (just-one gen-line) (just-one gen-line))
  (cut-lines seeker (just-one gen-text)))

(defspec cutting-test
         100
         (for-all [seeker gen-seeker]
                  (cutting seeker)))

(defbench cutting-bench cutting)

;; XVIII. Pasting

(defn paste-new-line [seeker line]
  (-> seeker
      (i/peer (fn [a b] (concat a [line] b)))
      (i/start-x)
      (i/select)
      (i/end-x)
      (i/copy)
      (can-be #(-> % (i/end) (i/break) (i/paste) (i/line) (= line)))))

(defn paste-within-line [seeker line]
  (-> seeker
      (i/peer (fn [a b] (concat a [line] b)))
      (i/start-x)
      (i/select)
      (i/jump-right)
      (i/copy)
      (i/deselect)
      (can-be #(-> (i/paste %) (i/line) (= (concat line line)))
              #(-> (i/paste %)
                   (i/jump-left)
                   (i/paste)
                   (i/line) (= (concat line line line))))))

(defn paste-concatenating-lines [seeker line1 line2 line3]
  (-> seeker
      (i/peer (fn [a b] (concat a [line1 line2 line3] b)))
      (i/start-x)
      (i/select)
      (i/fall)
      (i/end-x)
      (i/copy)
      (i/deselect)
      (can-be #(-> % (i/paste) (i/line) (= line2))
              #(-> % (i/paste) (i/climb) (i/line) (= (concat line2 line1))))))

(defn paste-overriding-selection [seeker some-text line2]
  (let [expected (i/peer seeker (fn [a b] (concat a [line2] b)))]
    (-> seeker
        (assoc :clipboard (i/seeker [line2]))
        (i/peer (fn [a b] (concat a some-text b)))
        (i/start-x)
        (i/select)
        (i/move-y #(-> (count some-text) (dec) (+ %)))
        (i/end-x)
        (i/paste)
        (<=> expected))))

(defn pasting [seeker]
  (paste-new-line seeker (just-one gen-line))
  (paste-within-line seeker (just-one gen-line))
  (paste-overriding-selection seeker
                              (just-one gen-text)
                              (just-one gen-line))
  (paste-concatenating-lines seeker
                             (just-one gen-line)
                             (just-one gen-line)
                             (just-one gen-line)))

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

(defn pair-outer-parens [seeker]
  (let [parens [[\( \)] [\[ \]] [\{ \}]]]
    (reduce
      (fn [_ [l r]]
        (-> seeker
            (i/slice (fn [a b] (concat [l] a b [r])))
            (can-be #(->> (i/start-x %) (i/find-pair) (chars-at %) (= [l r]))
                    #(->> (i/end-x %) (i/regress) (i/find-pair) (chars-at %) (= [l r]))))) nil parens)))

(defn pair-inner-parens [seeker line]
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

(defn dont-pair-unbalanced-parens [seeker]
  (let [parens [[\( \)] [\[ \]] [\{ \}]]]
    (reduce
      (fn [_ [l r]]
        (-> seeker
            (i/slice (fn [a b] (concat [l l l] a b [r r])))
            (i/start-x)
            (can-be #(->> (i/find-pair %) (nil?))
                    #(->> (i/advance %) (i/find-pair) (chars-at %) (= [l r]))
                    #(->> (i/advance %) (i/advance) (i/find-pair) (chars-at %) (= [l r]))))) nil parens)))

(defn pairing [seeker]
  (pair-outer-parens seeker)
  (pair-inner-parens seeker (just-one gen-line))
  (dont-pair-unbalanced-parens seeker))

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

;; XX1. Extracting

(defn extract-empty []
  (-> (i/expand i/empty-seeker)
      (i/extract)
      (i/line)
      (= i/empty-vec)
      (is)))

(defn extract-bounded [seeker]
  (-> (i/start seeker)
      (i/select)
      (i/end)
      (i/extract)
      (<=> seeker)))

(defn extract-selected [seeker]
  (let [expected (-> seeker
                     (i/split (fn [_ b] [b]))
                     (i/peer (fn [_ b] b)))]
    (-> (i/select seeker)
        (i/end)
        (i/extract)
        (<=> expected))))

(defn extracting [seeker]
  (extract-empty)
  (extract-bounded seeker)
  (extract-selected seeker))

(defspec extracting-test
         100
         (for-all [seeker gen-seeker]
                  (extracting seeker)))

(defbench extracting-bench extracting)