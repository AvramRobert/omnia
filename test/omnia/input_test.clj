(ns omnia.input-test
  (:require [omnia.input :as i]
            [omnia.more :refer [time-return]]
            [clojure.test :refer [is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :refer [for-all]]
            [clojure.test.check.generators :as gen]
            [omnia.test-utils :refer :all]))

(def ^:const NR-OF-TESTS 100)

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
      (let [_             (println "Benchmarking `" desc "`")
            result-string (bench f n)]
        (println result-string)
        (println))) *benchmarks*))

(defn before-cursor? [this-seeker that-seeker]
  (let [[xt yt] (:cursor this-seeker)
        [xa ya] (:cursor that-seeker)]
    (if (= yt ya)
      (< xt xa)
      (< yt ya))))

(defn after-cursor? [this-seeker that-seeker]
  (not (before-cursor? this-seeker that-seeker)))

(defn same-cursor? [this-seeker that-seeker]
  (= (:cursor this-seeker)
     (:cursor that-seeker)))

;; I. Peering

(defn peer-at-start [seeker]
  (let [actual  (-> seeker (i/start) (i/peer (fn [_ b] b)))
        expected seeker]
    (is (i/equivalent? expected actual))))

(defn peer-at-end [seeker]
  (let [actual   (-> seeker (i/end) (i/peer (fn [a _] a)))
        expected (i/rebase seeker drop-last)]
    (is (i/equivalent? expected actual))))

(defn peer-inserting-line [seeker line]
  (let [actual (-> seeker
                   (i/peer (fn [a b] (concat a [line] b)))
                   (i/line))
        expected line]
    (is (= expected actual))))

(defn peer-inserting-and-removing-line [seeker line]
  (let [expected (-> seeker
                     (i/peer (fn [a b] (concat a [line] b)))
                     (i/peer (fn [a [_ & c]] (concat a c))))
        actual   seeker]
    (is (i/equivalent? expected actual))))

(defn peer-removing-previous-lines [seeker]
  (let [[_ y]    (:cursor seeker)
        actual   (i/peer seeker (fn [_ b] b))
        expected (i/rebase seeker #(drop y %))]
    (is (i/equivalent? expected actual))))

(defn peer-removing-next-lines [seeker]
  (let [[_ y]    (:cursor seeker)
        actual   (i/peer seeker (fn [a _] a))
        expected (i/rebase seeker #(take y %))]
    (is (i/equivalent? expected actual))))

(defn peer-creating-line-when-empty [seeker]
  (let [actual   (-> i/empty-seeker
                     (i/peer (fn [a b] (concat a (:lines seeker) b))))
        expected seeker]
    (is (i/equivalent? expected actual))))

(defn peering [seeker]
  (peer-at-start seeker)
  (peer-at-end seeker)
  (peer-inserting-line seeker (one gen-nonempty-line))
  (peer-inserting-and-removing-line seeker (one gen-nonempty-line))
  (peer-removing-previous-lines seeker)
  (peer-removing-next-lines seeker)
  (peer-creating-line-when-empty seeker))

(defspec peering-test
         NR-OF-TESTS
  (for-all [seeker gen-seeker]
           (peering seeker)))

(defbench peering-bench peering)

;; II. Splitting

(defn split-removing-line-start [seeker]
  (let [[x y]    (:cursor seeker)
        actual   (-> seeker
                     (i/split (fn [_ b] [b]))
                     (i/line))
        expected (->> seeker (i/line) (drop x))]
    (is (= expected actual))))

(defn split-removing-line-end [seeker]
  (let [[x y]    (:cursor seeker)
        actual   (-> seeker
                     (i/split (fn [a _] [a]))
                     (i/line))
        expected (->> seeker (i/line) (take x))]
    (is (= expected actual))))

(defn split-creating-new-line [seeker line]
  (let [actual (-> seeker
                   (i/split (fn [a b] [line (concat a b)]))
                   (i/line))
        expected line]
    (is (= expected actual))))

(defn split-enhancing-line [seeker line]
  (let [actual (-> seeker
                   (i/split (fn [a b] [(concat a line b)]))
                   (i/split (fn [a b] [(concat a (drop (count line) b))])))
        expected seeker]
    (is (i/equivalent? expected actual))))

(defn split-replacing-line [seeker text]
  (let [actual   (-> seeker
                     (i/split (constantly text))
                     (i/line))
        expected (first text)]
    (is (= expected actual))))

(defn split-creating-line-when-empty [seeker]
  (let [actual   (-> i/empty-seeker
                     (i/split (fn [a b] [(concat a (i/line seeker) b)]))
                     (i/line))
        expected (i/line seeker)]
    (is (= expected actual))))

(defn splitting [seeker]
  (split-removing-line-start seeker)
  (split-removing-line-end seeker)
  (split-creating-new-line seeker (one gen-nonempty-line))
  (split-enhancing-line seeker (one gen-nonempty-line))
  (split-replacing-line seeker (one gen-nonempty-text))
  (split-creating-line-when-empty seeker))

(defspec splitting-test
         NR-OF-TESTS
  (for-all [seeker gen-seeker]
           (splitting seeker)))

(defbench splitting-bench splitting)

;; III. Slicing

(defn slice-removing-left-of-line [seeker]
  (let [[x _]    (:cursor seeker)
        actual   (-> seeker
                     (i/slice (fn [a _] a))
                     (i/line))
        expected (->> seeker (i/line) (take x))]
    (is (= expected actual))))

(defn slice-removing-right-of-line [seeker]
  (let [[x _]    (:cursor seeker)
        actual   (-> seeker
                     (i/slice (fn [_ b] b))
                     (i/line))
        expected (->> seeker (i/line) (drop x))]
    (is (= expected actual))))

(defn slice-enhancing-line [seeker line]
  (let [actual   (-> seeker
                     (i/slice (fn [a b] (concat line a b)))
                     (i/line))
        expected (->> seeker (i/line) (concat line))]
    (is (= expected actual))))

(defn slice-replacing-line [seeker line]
  (let [actual   (-> seeker
                     (i/slice (constantly line))
                     (i/line))
        expected line]
    (is (= expected actual))))

(defn slice-creating-line-when-empty [seeker]
  (let [line (i/line seeker)
        actual (-> i/empty-seeker
                   (i/slice (fn [a b] (concat a line b)))
                   (i/line))]
    (is (= line actual))))

(defn slicing [seeker]
  (slice-removing-left-of-line seeker)
  (slice-removing-right-of-line seeker)
  (slice-enhancing-line seeker (one gen-nonempty-line))
  (slice-replacing-line seeker (one gen-nonempty-line))
  (slice-creating-line-when-empty seeker))

(defspec slicing-test
         NR-OF-TESTS
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
      (should-be #(= (i/line %) line)
                 #(= (i/right %) (first line)))))

(defn moving [seeker]
  (bound-movement seeker)
  (unbound-movement seeker (one gen-nonempty-line)))

(defspec moving-test
         NR-OF-TESTS
  (for-all [seeker gen-seeker]
           (moving seeker)))

(defbench moving-bench moving)

;; V. Retrieving

(defn get-previous-char [seeker text]
  (let [[line1 _] text]
    (-> seeker
        (i/peer (fn [a b] (concat a text b)))
        (i/start-x)
        (i/move-y inc)
        (should-be #(-> (i/go-back %) (i/prev-char) (= (last line1)))
                   #(-> (i/move-y % dec) (i/move-x inc) (i/prev-char) (= (first line1)))))))

(defn get-current-char [seeker]
  (let [actual   (-> seeker (i/start) (i/current-char))
        expected (-> seeker (:lines) (first) (first))]
    (is (= expected actual))))

(defn retrieving [seeker]
  (get-previous-char seeker (many gen-nonempty-line 2))
  (get-current-char seeker))

(defspec retrieving-test
         NR-OF-TESTS
  (for-all [seeker gen-seeker]
           (retrieving seeker)))

(defbench retrieving-bench retrieving)

;; VI. Progressing

(defn simple-progress [seeker line]
  (-> (i/start seeker)
      (i/split (fn [a b] [line a b]))
      (should-be #(-> % (i/go-forward) (after-cursor? %)))))

(defn stop-progress-when-text-ends [seeker]
  (-> (i/end seeker)
      (should-be #(-> % (i/go-forward) (same-cursor? %)))))

(defn wrap-when-line-ends [seeker text]
  (-> (i/start seeker)
      (i/peer (fn [a b] (concat text a b)))
      (i/end-x)
      (should-be #(-> % (i/go-forward) (after-cursor? %)))))

(defn progressing [seeker]
  (simple-progress seeker (one gen-nonempty-line))
  (stop-progress-when-text-ends seeker)
  (wrap-when-line-ends seeker (one gen-nonempty-text)))

(defspec progressing-test
         NR-OF-TESTS
  (for-all [seeker gen-seeker]
           (progressing seeker)))

(defbench progressing-bench progressing)

;; VII. Regressing

(defn simple-regress [seeker line]
  (-> seeker
      (i/split (fn [a b] [a line b]))
      (should-be #(-> % (i/go-forward) (i/go-back) (same-cursor? %)))))

(defn stop-regress-when-start [seeker]
  (-> (i/start-x seeker)
      (i/go-back)
      (same-cursor? seeker)))

(defn wrap-when-line-starts [seeker text]
  (-> seeker
      (i/peer (fn [a b] (concat a b text)))
      (i/end)
      (i/start-x)
      (should-be #(-> % (i/go-back) (before-cursor? %)))))

(defn regressing [seeker]
  (simple-regress seeker (one gen-nonempty-line))
  (stop-regress-when-start seeker)
  (wrap-when-line-starts seeker (one gen-nonempty-text)))

(defspec regressing-test
         NR-OF-TESTS
  (for-all [seeker gen-seeker]
           (regressing seeker)))

(defbench regressing-bench regressing)

;; VIII. Climbing

(defn simple-ascent [seeker text]
  (-> seeker
      (i/peer (fn [a b] (concat a text b)))
      (i/move-y inc)
      (should-be #(-> (i/go-up %) (before-cursor? %)))))

(defn ascent-to-end-of-line [seeker]
  "explicit data, because second line has to be larger than the first"
  (let [some-text [[\a \b] [\a \b \c]]]
    (-> seeker
        (i/peer (fn [a b] (concat a some-text b)))
        (should-be #(-> (i/move-y % inc) (i/end-x) (i/go-up) (same-cursor? (i/end-x %)))))))

(defn climbing [seeker]
  (simple-ascent seeker (one gen-nonempty-text))
  (ascent-to-end-of-line seeker))

(defspec climbing-test
         NR-OF-TESTS
  (for-all [seeker gen-seeker]
           (climbing seeker)))

(defbench climbing-bench climbing)

;; IX. Falling

(defn simple-descent [seeker text]
  (-> seeker
      (i/peer (fn [a b] (concat a text b)))
      (should-be #(-> % (i/go-down) (after-cursor? %)))))

(defn descent-to-end-of-line [seeker]
  "explicit data, because first line has to be larger than the second"
  (let [some-text [[\a \b \c] [\d \e]]]
    (-> seeker
        (i/peer (fn [a b] (concat a some-text b)))
        (i/move-y inc)
        (should-be #(-> (i/move-y % dec) (i/end-x) (i/go-down) (same-cursor? (i/end-x %)))))))

(defn falling [seeker]
  (simple-descent seeker (one gen-nonempty-text))
  (descent-to-end-of-line seeker))

(defspec falling-test
         NR-OF-TESTS
  (for-all [seeker gen-seeker]
           (falling seeker)))

(defbench falling-bench falling)

;; X. Deleting

(defn delete-previous [seeker]
  (let [some-text [[\a \b]]]
    (-> seeker
        (i/peer (fn [a b] (concat a b some-text)))
        (i/end)
        (should-be #(-> % (i/backspace) (i/prev-char) (= \a))
                   #(-> % (i/backspace) (i/backspace) (i/prev-char) (nil?))
                   #(-> % (i/backspace) (i/backspace) (i/backspace) (i/equivalent? seeker))))))

(defn delete-next [seeker]
  (let [some-text [[\a \b]]]
    (-> seeker
        (i/peer (fn [a b] (concat some-text a b)))
        (i/start)
        (should-be #(-> % (i/delete) (i/right) (= \b))
                   #(-> % (i/delete) (i/delete) (i/right) (nil?))
                   #(-> % (i/delete) (i/delete) (i/delete) (i/equivalent? seeker))))))

(defn delete-pairs [seeker]
  (let [remove-line #(-> % (i/backspace) (i/backspace) (i/backspace))
        round       [[\( \)]]
        brackets    [[\[ \]]]
        squiggly    [[\{ \}]]
        quote       [[\" \"]]]
    (-> seeker
        (i/peer (fn [a b] (concat a b round brackets squiggly quote))) ;; use split instead of peer to remove (vector) from tests
        (i/end)
        (should-be #(-> % (remove-line) (i/line) (vector) (= squiggly))
                   #(-> % (remove-line) (remove-line) (i/line) (vector) (= brackets))
                   #(-> % (remove-line) (remove-line) (remove-line) (i/line) (vector) (= round))
                   #(-> % (remove-line) (remove-line) (remove-line) (remove-line) (i/equivalent? seeker))))))

(defn delete-omitting-single-parens [seeker]
  (letfn [(f [s c] (i/slicel s #(conj % c)))]
    (-> (i/end seeker)
        (should-be #(-> (f % \)) (i/go-forward) (i/backspace) (i/equivalent? (f % \))))
                   #(-> (f % \]) (i/go-forward) (i/backspace) (i/equivalent? (f % \])))
                   #(-> (f % \}) (i/go-forward) (i/backspace) (i/equivalent? (f % \})))
                   #(-> (f % \") (i/go-forward) (i/backspace) (i/equivalent? (f % \")))))))

(defn delete-selections [seeker]
  (-> seeker
      (i/select-all)
      (i/backspace)
      (should-be #(i/equivalent? % i/empty-line))))

(defn deleting [seeker]
  (delete-previous seeker)
  (delete-next seeker)
  (delete-pairs seeker)
  (delete-omitting-single-parens seeker)
  (delete-selections seeker))

(defspec deleting-test
         NR-OF-TESTS
  (for-all [seeker gen-seeker]
           (deleting seeker)))

(defbench deleting-bench deleting)

;; XI. Inserting

(defn insert-literal [seeker c]
  (let [actual (-> seeker (i/insert c) (i/prev-char))]
    (is (= c actual))))

(defn insert-pairs [seeker]
  (letfn [(f [s l r] (i/slicel s #(conj % l r)))]
    (-> (i/end seeker)
        (should-be
          #(-> % (i/insert \() (i/equivalent? (f % \( \))))
          #(-> % (i/insert \[) (i/equivalent? (f % \[ \])))
          #(-> % (i/insert \{) (i/equivalent? (f % \{ \})))
          #(-> % (i/insert \") (i/equivalent? (f % \" \")))))))

(defn insert-ignoring-simple-parens [seeker]
  (letfn [(f [s l r] (i/slicel s #(conj % l r)))]
    (-> (i/end seeker)
        (should-be
          #(-> % (i/insert \() (i/insert \)) (i/equivalent? (f % \( \))))
          #(-> % (i/insert \[) (i/insert \]) (i/equivalent? (f % \[ \])))
          #(-> % (i/insert \{) (i/insert \}) (i/equivalent? (f % \{ \})))
          #(-> % (i/insert \") (i/insert \") (i/equivalent? (f % \" \")))))))

(defn insert-replacing-selection [seeker some-text c]
  (let [expected (i/peer seeker (fn [a b] (concat a [[c]] b)))
        actual   (-> seeker
                     (i/peer (fn [a b] (concat a some-text b)))
                     (i/start-x)
                     (i/select)
                     (i/move-y #(-> (count some-text) (dec) (+ %)))
                     (i/end-x)
                     (i/insert c))]
    (is (i/equivalent? expected actual))))

(defn inserting [seeker]
  (insert-literal seeker (one gen/char-alphanumeric))
  (insert-pairs seeker)
  (insert-ignoring-simple-parens seeker)
  (insert-replacing-selection seeker
                              (one gen-nonempty-text)
                              (one gen/char-alphanumeric)))

(defspec inserting-test
         NR-OF-TESTS
  (for-all [seeker gen-seeker]
           (inserting seeker)))

(defbench inserting-bench inserting)

;; XII. Jumping

(defn jump-over-words [seeker text]
  (-> seeker
      (i/peer (fn [a b] (concat a text b)))
      (i/start-x)
      (should-be #(-> % (i/jump-right) (i/jump-left) (same-cursor? %)))))

(defn jump-until-spaces [seeker line1 line2]
  (-> seeker
      (i/peer (fn [a b] (concat a [(concat line1 [\space] line2)] b)))
      (should-be #(-> % (i/start-x) (i/jump-right) (i/current-char) (= \space))
                 #(-> % (i/end-x) (i/jump-left) (i/prev-char) (= \space)))))

(defn jump-until-expr-ends [seeker line]
  (letfn [(f [s c]
            (-> (i/peer s #(concat %1 [(conj line c)] %2))
                (i/start-x)))]
    (should-be seeker
               #(-> (f % \() (i/jump-right) (i/current-char) (= \())
               #(-> (f % \[) (i/jump-right) (i/current-char) (= \[))
               #(-> (f % \{) (i/jump-right) (i/current-char) (= \{))
               #(-> (f % \") (i/jump-right) (i/current-char) (= \"))
               #(-> (f % \)) (i/jump-right) (i/current-char) (= \)))
               #(-> (f % \]) (i/jump-right) (i/current-char) (= \]))
               #(-> (f % \}) (i/jump-right) (i/current-char) (= \})))))

(defn jump-between-lines [seeker]
  (let [some-text [[\a \b]]]
    (-> seeker
        (i/peer (fn [a b] (concat a some-text b)))
        (i/end-x)
        (should-be #(-> % (i/jump-right) (after-cursor? %))))))

(defn jump-over-spaces [seeker line1 line2]
  (let [spaces [\space \space \space]]
    (-> seeker
        (i/peer (fn [a b] (concat a [(concat line1 spaces line2)] b)))
        (i/start-x)
        (should-be #(-> (i/current-char %) (= (first line1)))
                   #(-> (i/jump-right %) (i/jump-right) (i/current-char) (= (first line2)))
                   #(-> (i/end-x %) (i/jump-left) (i/current-char) (= (first line2)))
                   #(-> (i/end-x %) (i/jump-left) (i/jump-left) (i/prev-char) (= (last line1)))
                   #(-> (i/end-x %) (i/jump-left) (i/jump-left) (i/jump-left) (i/current-char) (= (first line1)))))))

(defn jumping [seeker]
  (jump-over-words seeker (one gen-nonempty-text))
  (jump-between-lines seeker)
  (jump-until-expr-ends seeker (one gen-nonempty-line))
  (jump-until-spaces seeker
                     (one gen-nonempty-line)
                     (one gen-nonempty-line))
  (jump-over-spaces seeker
                    (one gen-nonempty-line)
                    (one gen-nonempty-line)))

(defspec jumping-test
         NR-OF-TESTS
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
      (should-be #(-> (:start %) (first) (zero?))
                 #(-> (:end %) (first) (= 1)))))

(defn select-when-jumping [seeker]
  (-> seeker
      (i/start-x)
      (i/select)
      (i/end-x)
      (should-be #(-> (i/selection %) (:start) (= (:cursor (i/start-x %))))
                 #(-> (i/selection %) (:end) (= (:cursor (i/end-x %)))))))

(defn select-lines [seeker text]
  (-> seeker
      (i/peer (fn [a b] (concat a text b)))
      (i/start-x)
      (i/select)
      (i/go-down)
      (should-be #(-> (i/selection %) (:start) (= (:cursor (i/go-up %))))
                 #(-> (i/selection %) (:end) (= (:cursor %))))))

(defn select-blocks [seeker]
  (-> seeker
      (i/select-all)
      (should-be #(-> (i/selection %) (:start) (= (:cursor (i/start %))))
                 #(-> (i/selection %) (:end) (= (:cursor (i/end %)))))))

(defn selecting [seeker]
  (select-single-chars seeker (one gen-nonempty-text))
  (select-when-jumping seeker)
  (select-lines seeker (one gen-nonempty-text))
  (select-blocks seeker))

(defspec selecting-test
         NR-OF-TESTS
  (for-all [seeker gen-seeker]
           (selecting seeker)))


(defbench selecting-bench selecting)

;; XVI. Joining

(defn join-texts [seeker1 seeker2]
  (-> (i/conjoin seeker1 seeker2)
      (should-be #(i/equivalent? % (i/seeker (concat (:lines seeker1) (:lines seeker2))))
                 #(-> (i/line %) (= (i/line seeker2))))))

(defn join-texts-with-selections [seeker1 seeker2]
  (let [s1 (-> seeker1 (i/select) (i/end))
        s2 (-> seeker2 (i/select) (i/end))]
    (-> (i/conjoin s1 s2)
        (should-be #(-> (i/selection %) (:end) (= (:cursor (i/end %))))))))

(defn joining [seeker1]
  (let [seeker2 (first (gen/sample gen-seeker))]
    (join-texts seeker1 seeker2)
    (join-texts-with-selections seeker1 seeker2)))

(defspec joining-test
         NR-OF-TESTS
  (for-all [seeker gen-seeker]
           (joining seeker)))

(defbench joining-bench joining)

;; XV. Expanding

(defn expand-to-words [seeker line1 line2]
  (let [some-text [(concat line1 [\space] line2)]]
    (-> seeker
        (i/peer (fn [a b] (concat a some-text b)))
        (i/start-x)
        (should-be #(-> (i/expand %) (i/extract) (i/line) (= line1))
                   #(-> (i/jump-right %) (i/expand) (i/extract) (i/line) (= line1))
                   #(-> (i/end-x %) (i/expand) (i/extract) (i/line) (= line2))
                   #(-> (i/jump-right %) (i/jump-right) (i/expand) (i/extract) (i/line) (= line2))))))

(defn expand-from-words-to-expr [seeker]
  (reduce
    (fn [_ [l r]]
      (let [some-line [l l \a \b \space \space \c \d r r]]
        (-> seeker
            (i/peer (fn [a b] (concat a [some-line] b)))
            (should-be #(-> % (i/start-x) (i/expand) (i/extract) (i/line) (= some-line))
                       #(-> % (i/end-x) (i/expand) (i/extract) (i/line) (= some-line))
                       #(-> (i/start-x %)
                         (i/jump-right)
                         (i/jump-right)
                         (i/jump-right)
                         (i/go-forward)
                         (i/expand)
                         (i/expand)
                         (i/extract)
                         (i/line)
                         (= some-line))))))
    nil
    [[\( \)] [\[ \]] [\{ \}]]))

(defn expand-over-one-expr [seeker line]
  (reduce
    (fn [_ [l r]]
      (let [some-line (concat [l] line [r])]
        (-> seeker
            (i/peer (fn [a b] (concat a [some-line] b)))
            (i/start-x)
            (i/move-x inc)
            (should-be #(-> % (i/expand) (i/extract) (i/line) (= line))
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
                  some-line  (concat [ol] line2 inner-expr line3 [or])]
              (-> seeker
                  (i/peer (fn [a b] (concat a [some-line] b)))
                  (i/start-x)
                  (i/jump-right)
                  (i/jump-right)
                  (i/jump-right)
                  (should-be #(-> (i/expand %) (i/extract) (i/line) (= line1))
                             #(-> (i/expand %) (i/expand) (i/extract) (i/line) (= inner-expr))
                             #(-> (i/expand %) (i/expand) (i/expand) (i/extract) (i/line) (= some-line))))))
          nil
          parens))
      nil
      parens)))

(defn expand-over-all-expr [seeker line]
  (reduce
    (fn [_ [l r]]
      (let [expr     (concat [l] line [r])
            expected (-> seeker
                         (i/slice (fn [a b] (concat [l] a b [r])))
                         (i/end)
                         (i/peer (fn [a b] (concat a b [expr]))))
            actual   (-> expected
                         (i/expand)
                         (i/expand)
                         (i/expand)
                         (i/extract))]
        (is (i/equivalent? expected actual))))
    [[\( \)] [\[ \]] [\{ \}]]))

(defn expanding [seeker]
  (expand-to-words seeker (one gen-nonempty-line) (one gen-nonempty-line))
  (expand-from-words-to-expr seeker)
  (expand-over-one-expr seeker (one gen-nonempty-line))
  (expand-from-inner-to-outer-expr seeker
                                   (one gen-nonempty-line)
                                   (one gen-nonempty-line)
                                   (one gen-nonempty-line))
  (expand-over-all-expr seeker (one gen-nonempty-line)))

(defspec expanding-test
         NR-OF-TESTS
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
        (should-be #(-> (i/copy %) (:clipboard) (i/equivalent? (i/seeker [line1])))))))

(defn copy-lines [seeker some-text]
  (-> seeker
      (i/peer (fn [a b] (concat a some-text b)))
      (i/start-x)
      (i/select)
      (i/move-y #(-> (count some-text) (dec) (+ %)))
      (i/end-x)
      (should-be #(-> (i/copy %) (:clipboard) (i/equivalent? (i/seeker some-text))))))

(defn copying [seeker]
  (copy-within-line seeker (one gen-nonempty-line) (one gen-nonempty-line))
  (copy-lines seeker (one gen-nonempty-text)))

(defspec copying-test
         NR-OF-TESTS
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
        (should-be #(-> (i/cut %) (:clipboard) (i/equivalent? (i/seeker [line1])))
                   #(i/equivalent? (i/cut %) (i/backspace %))))))

(defn cut-lines [seeker some-text]
  (-> seeker
      (i/peer (fn [a b] (concat a some-text b)))
      (i/start-x)
      (i/select)
      (i/move-y #(-> (count some-text) (dec) (+ %)))
      (i/end-x)
      (should-be #(-> (i/cut %) (:clipboard) (i/equivalent? (i/seeker some-text)))
                 #(i/equivalent? (i/cut %) (i/backspace %)))))

(defn cutting [seeker]
  (cut-within-line seeker (one gen-nonempty-line) (one gen-nonempty-line))
  (cut-lines seeker (one gen-nonempty-text)))

(defspec cutting-test
         NR-OF-TESTS
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
      (should-be #(-> % (i/end) (i/break) (i/paste) (i/line) (= line)))))

(defn paste-within-line [seeker line]
  (-> seeker
      (i/peer (fn [a b] (concat a [line] b)))
      (i/start-x)
      (i/select)
      (i/jump-right)
      (i/copy)
      (i/deselect)
      (should-be #(-> (i/paste %) (i/line) (= (concat line line)))
                 #(-> (i/paste %)
                   (i/jump-left)
                   (i/paste)
                   (i/line) (= (concat line line line))))))

(defn paste-concatenating-lines [seeker line1 line2 line3]
  (-> seeker
      (i/peer (fn [a b] (concat a [line1 line2 line3] b)))
      (i/start-x)
      (i/select)
      (i/go-down)
      (i/end-x)
      (i/copy)
      (i/deselect)
      (should-be #(-> % (i/paste) (i/line) (= line2))
                 #(-> % (i/paste) (i/go-up) (i/line) (= (concat line2 line1))))))

(defn paste-overriding-selection [seeker some-text line2]
  (let [expected (i/peer seeker (fn [a b] (concat a [line2] b)))
        actual   (-> seeker
                     (assoc :clipboard (i/seeker [line2]))
                     (i/peer (fn [a b] (concat a some-text b)))
                     (i/start-x)
                     (i/select)
                     (i/move-y #(-> (count some-text) (dec) (+ %)))
                     (i/end-x)
                     (i/paste))]
    (is (i/equivalent? expected actual))))

(defn pasting [seeker]
  (paste-new-line seeker (one gen-nonempty-line))
  (paste-within-line seeker (one gen-nonempty-line))
  (paste-overriding-selection seeker
                              (one gen-nonempty-text)
                              (one gen-nonempty-line))
  (paste-concatenating-lines seeker
                             (one gen-nonempty-line)
                             (one gen-nonempty-line)
                             (one gen-nonempty-line)))

(defspec pasting-test
         NR-OF-TESTS
  (for-all [seeker gen-seeker]
           (pasting seeker)))

(defbench pasting-bench pasting)

;; XIX. Pairing

(defn- chars-at [seeker {:keys [start end]}]
  (letfn [(f [there] (-> (i/move seeker (constantly there))
                         (i/current-char)))]
    [(f start)
     (f end)]))

(defn pair-outer-parens [seeker]
  (let [parens [[\( \)] [\[ \]] [\{ \}]]]
    (reduce
      (fn [_ [l r]]
        (-> seeker
            (i/slice (fn [a b] (concat [l] a b [r])))
            (should-be #(->> (i/start-x %) (i/find-pair) (chars-at %) (= [l r]))
                       #(->> (i/end-x %) (i/go-back) (i/find-pair) (chars-at %) (= [l r]))))) nil parens)))

(defn pair-inner-parens [seeker line]
  (let [parens [[\( \)] [\[ \]] [\{ \}]]]
    (reduce
      (fn [_ [ol or]]
        (reduce
          (fn [_ [il ir]]
            (-> seeker
                (i/peer (fn [a b] (concat a [(concat [ol] [il] line [ir] [or])] b)))
                (i/start-x)
                (should-be #(->> (i/find-pair %) (chars-at %) (= [ol or]))
                           #(->> (i/go-forward %) (i/find-pair) (chars-at %) (= [il ir])))))
          nil parens))
      nil parens)))

(defn dont-pair-unbalanced-parens [seeker]
  (let [parens [[\( \)] [\[ \]] [\{ \}]]]
    (reduce
      (fn [_ [l r]]
        (-> seeker
            (i/slice (fn [a b] (concat [l l l] a b [r r])))
            (i/start-x)
            (should-be #(->> (i/find-pair %) (nil?))
                       #(->> (i/go-forward %) (i/find-pair) (chars-at %) (= [l r]))
                       #(->> (i/go-forward %) (i/go-forward) (i/find-pair) (chars-at %) (= [l r]))))) nil parens)))

(defn pairing [seeker]
  (pair-outer-parens seeker)
  (pair-inner-parens seeker (one gen-nonempty-line))
  (dont-pair-unbalanced-parens seeker))

(defspec pairing-test
         NR-OF-TESTS
  (for-all [seeker gen-seeker]
           (pairing seeker)))

(defbench pairing-bench pairing)

;; XX. Parens matching

(clojure.test/deftest matching-test
  (let [s1 (i/seeker [[\[ \[ \[ \4 \5 \] \]]])
        s2 (i/seeker [[\[ \( \a \{ \} \b \) \]]])
        s3 (i/seeker [[\[ \( \} \{ \) \]]])
        s4 (i/seeker [[\a \b \4 \( \)]])]
    ;; s1
    (is (= :unmatched (-> (i/open-expand s1) (first))))
    (is (= :matched (-> (i/end-x s1) (i/closed-expand) (first))))
    (is (= :matched (-> (i/go-forward s1) (i/open-expand) (first))))
    (is (= :matched (-> (i/end-x s1) (i/go-back) (i/closed-expand) (first))))
    (is (= :matched (-> (i/go-forward s1) (i/go-forward) (i/open-expand) (first))))
    (is (= :unmatched (-> (i/go-forward s1) (i/go-forward) (i/go-forward) (i/open-expand) (first))))
    (is (= :unmatched (-> (i/end-x s1) (i/open-expand) (first))))
    (is (= :unmatched (-> (i/go-forward s1) (i/closed-expand) (first))))
    (is (= :unmatched (-> (i/start s1) (i/near-expand) (first))))
    (is (= :unmatched (-> (i/go-forward s1) (i/near-expand) (first))))
    (is (= :unmatched (-> (i/end-x s1) (i/near-expand) (first))))
    (is (= :matched (-> (i/go-forward s1) (i/go-forward) (i/near-expand) (first))))
    (is (= :matched (-> (i/end-x s1) (i/go-back) (i/near-expand) (first))))
    ;;;; s2
    (is (= :matched (-> (i/open-expand s2) (first))))
    (is (= :matched (-> (i/end-x s2) (i/closed-expand) (first))))
    (is (= :matched (-> (i/go-forward s2) (i/open-expand) (first))))
    (is (= :matched (-> (i/go-forward s2) (i/go-forward) (i/near-expand) (first))))
    (is (= :matched (-> (i/go-forward s2) (i/go-forward) (i/go-forward) (i/near-expand) (first))))
    (is (= :matched (-> (i/end-x s2) (i/go-back) (i/near-expand) (first))))
    (is (= :matched (-> (i/end-x s2) (i/go-back) (i/go-back) (i/near-expand) (first))))
    ;;;;; s3
    (is (= :matched (-> (i/open-expand s3) (first))))
    (is (= :matched (-> (i/go-forward s3) (i/open-expand) (first))))
    (is (= :matched (-> (i/go-forward s3) (i/go-forward) (i/open-expand) (first))))
    (is (= :matched (-> (i/go-forward s3) (i/go-forward) (i/near-expand) (first))))
    (is (= :unmatched (-> (i/go-forward s3) (i/go-forward) (i/go-forward) (i/near-expand) (first))))
    (is (= :unmatched (-> (i/go-forward s3) (i/go-forward) (i/go-forward) (i/closed-expand) (first))))
    (is (= :unmatched (-> (i/end-x s3) (i/go-back) (i/near-expand) (first))))
    (is (= :unmatched (-> (i/end-x s3) (i/go-back) (i/go-back) (i/near-expand) (first))))
    (is (= :unmatched (-> (i/end-x s3) (i/go-back) (i/go-back) (i/go-back) (i/open-expand) (first))))
    ;;;; s4
    (is (= :unmatched (-> (i/open-expand s4) (first))))
    (is (= :unmatched (-> (i/closed-expand s4) (first))))
    (is (= :unmatched (-> (i/go-forward s4) (i/open-expand) (first))))
    (is (= :unmatched (-> (i/go-forward s4) (i/go-forward) (i/open-expand) (first))))
    (is (= :matched (-> (i/go-forward s4) (i/go-forward) (i/go-forward) (i/open-expand) (first))))
    (is (= :matched (-> (i/end-x s4) (i/go-back) (i/near-expand) (first))))
    (is (= :unmatched (-> (i/end-x s4) (i/go-back) (i/go-back) (i/near-expand) (first))))
    (is (= :unmatched (-> (i/end-x s4) (i/go-back) (i/go-back) (i/go-back) (i/near-expand) (first))))))

;; XX1. Extracting

(defn extract-empty []
  (let [actual (-> i/empty-seeker (i/expand) (i/extract) (i/line))
        expected []]
    (is (= expected actual))))

(defn extract-bounded [seeker]
  (let [actual   (-> seeker (i/start) (i/select) (i/end) (i/extract))
        expected seeker]
    (is (i/equivalent? expected actual))))

(defn extract-selected [seeker]
  (let [actual   (-> seeker (i/select) (i/end) (i/extract))
        expected (-> seeker
                     (i/split (fn [_ b] [b]))
                     (i/peer (fn [_ b] b)))]
    (is (i/equivalent? expected actual))))

(defn extracting [seeker]
  (extract-empty)
  (extract-bounded seeker)
  (extract-selected seeker))

(defspec extracting-test
         NR-OF-TESTS
  (for-all [seeker gen-seeker]
           (extracting seeker)))

(defbench extracting-bench extracting)

;; XXI. Undoing / Redoing

(defn balance-undo-redo [seeker]
  (let [historical (-> seeker
                       (i/remember)
                       (i/insert \a)
                       (i/remember)
                       (i/insert \b))]
    (should-be historical
               #(-> % (:rhistory) (count) (= 0))
               #(-> % (:history) (count) (= 2))
               #(-> % (i/undo) (i/undo) (:rhistory) (count) (= 2))
               #(-> % (i/undo) (i/undo) (:history) (empty?))
               #(-> % (i/undo) (:history) (count) (= 1))
               #(-> % (i/undo) (:rhistory) (count) (= 1)))))

(defn limit-undoing [seeker]
  (let [history (one (gen-history {:size 50 :element-size 3}))]
    (-> seeker
        (assoc :history history)
        (i/remember)
        (i/insert \a)
        (should-be #(-> (i/remember %) (i/insert \b) (i/undo) (i/equivalent? %))
                   #(-> (i/remember %) (i/insert \b) (i/undo) (:history) (count) (= 49))))))

(defn redo-undone [seeker]
  (-> seeker
      (i/remember)
      (i/insert \a)
      (should-be #(-> % (i/undo) (i/redo) (i/equivalent? %)))))

(defn undo-remembered [seeker]
  (-> seeker
      (i/remember)
      (i/insert \a)
      (i/undo)
      (should-be #(i/equivalent? % seeker))))


(defn keep-clipboard-between-undos-redos [seeker]
  (-> seeker
      (i/select-all)
      (i/cut)
      (i/remember)
      (i/insert \a)
      (i/remember)
      (i/insert \b)
      (i/undo)
      (i/undo)
      (i/paste)
      (should-be #(i/equivalent? % seeker))))

(defn undo-redo [seeker]
  (undo-remembered seeker)
  (redo-undone seeker)
  (limit-undoing seeker)
  (balance-undo-redo seeker)
  (keep-clipboard-between-undos-redos seeker))

(defspec undoing-redoing-test
         NR-OF-TESTS
  (for-all [seeker gen-seeker]
           (undo-redo seeker)))

(defbench undoing-redoing-bench undo-redo)

(defn preserve-history [seeker]
  (let [changed (process' seeker [(character \a) (character \b)])]
    (is (not (i/equivalent? changed seeker)))
    (is (not (i/equivalent? (process' changed [undo]) seeker)))
    (is (i/equivalent? (process' changed [undo undo]) seeker))))

(defn traverse-history [seeker]
  (let [changed (process' seeker [(character \a) (character \b)])]
    (is (not (i/equivalent? (process' changed [undo undo redo]) changed)))
    (is (i/equivalent? (process' changed [undo undo redo redo]) changed))
    (is (i/equivalent? (process' changed [undo (character \k) undo redo redo]) changed))))

(defn history [seeker]
  (preserve-history seeker)
  (traverse-history seeker))

(defspec history-test
         NR-OF-TESTS
  (for-all [seeker gen-seeker]
           (history seeker)))

(defbench history-test history)