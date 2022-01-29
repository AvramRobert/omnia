(ns omnia.text-test
  (:require [clojure.test.check.generators :as gen]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :refer [for-all]]
            [clojure.test :refer [deftest is]]
            [omnia.text.core :as i]
            [omnia.util.debug :refer [time-return]]
            [omnia.util.generator :refer [one many]]
            [omnia.test-utils :refer :all]))

(def ^:const NR-OF-TESTS 1)

(def ^:dynamic *benchmarks* [])

(defn bench [f n]
  (letfn [(avg [a] (/ a n))]
    (->> (range 0 n)
         (mapv (fn [_]
                 (let [seeker (first (gen/sample gen-text-area))]
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
  (run! `(fn [[f desc]]
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
  (let [actual   (-> seeker (i/start) (i/peer (fn [_ b] b)))
        expected seeker]
    (is (i/equivalent? expected actual))))

(defn peer-at-end [seeker]
  (let [actual   (-> seeker (i/end) (i/peer (fn [a _] a)))
        expected (i/rebase seeker drop-last)]
    (is (i/equivalent? expected actual))))

(defn peer-inserting-line [seeker line]
  (let [actual   (-> seeker
                     (i/peer (fn [a b] (concat a [line] b)))
                     (i/current-line))
        expected line]
    (is (= expected actual))))

(defn peer-inserting-and-removing-line [seeker line]
  (let [expected (-> seeker
                     (i/peer (fn [a b] (concat a [line] b)))
                     (i/peer (fn [a [_ & c]] (concat a c))))
        actual   seeker]
    (is (i/equivalent? expected actual))))

(defn peer-removing-previous-lines [seeker]
  (let [[_ y] (:cursor seeker)
        actual   (i/peer seeker (fn [_ b] b))
        expected (i/rebase seeker #(drop y %))]
    (is (i/equivalent? expected actual))))

(defn peer-removing-next-lines [seeker]
  (let [[_ y] (:cursor seeker)
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
  (for-all [seeker gen-text-area]
    (peering seeker)))

(defbench peering-bench peering)

;; II. Splitting

(defn split-removing-line-start [seeker]
  (let [[x y] (:cursor seeker)
        actual   (-> seeker
                     (i/split (fn [_ b] [b]))
                     (i/current-line))
        expected (->> seeker (i/current-line) (drop x))]
    (is (= expected actual))))

(defn split-removing-line-end [seeker]
  (let [[x y] (:cursor seeker)
        actual   (-> seeker
                     (i/split (fn [a _] [a]))
                     (i/current-line))
        expected (->> seeker (i/current-line) (take x))]
    (is (= expected actual))))

(defn split-creating-new-line [seeker line]
  (let [actual   (-> seeker
                     (i/split (fn [a b] [line (concat a b)]))
                     (i/current-line))
        expected line]
    (is (= expected actual))))

(defn split-enhancing-line [seeker line]
  (let [actual   (-> seeker
                     (i/split (fn [a b] [(concat a line b)]))
                     (i/split (fn [a b] [(concat a (drop (count line) b))])))
        expected seeker]
    (is (i/equivalent? expected actual))))

(defn split-replacing-line [seeker text]
  (let [actual   (-> seeker
                     (i/split (constantly text))
                     (i/current-line))
        expected (first text)]
    (is (= expected actual))))

(defn split-creating-line-when-empty [seeker]
  (let [actual   (-> i/empty-seeker
                     (i/split (fn [a b] [(concat a (i/current-line seeker) b)]))
                     (i/current-line))
        expected (i/current-line seeker)]
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
  (for-all [seeker gen-text-area]
    (splitting seeker)))

(defbench splitting-bench splitting)

;; III. Slicing

(defn slice-removing-left-of-line [seeker]
  (let [[x _] (:cursor seeker)
        actual   (-> seeker
                     (i/slice (fn [a _] a))
                     (i/current-line))
        expected (->> seeker (i/current-line) (take x))]
    (is (= expected actual))))

(defn slice-removing-right-of-line [seeker]
  (let [[x _] (:cursor seeker)
        actual   (-> seeker
                     (i/slice (fn [_ b] b))
                     (i/current-line))
        expected (->> seeker (i/current-line) (drop x))]
    (is (= expected actual))))

(defn slice-enhancing-line [seeker line]
  (let [actual   (-> seeker
                     (i/slice (fn [a b] (concat line a b)))
                     (i/current-line))
        expected (->> seeker (i/current-line) (concat line))]
    (is (= expected actual))))

(defn slice-replacing-line [seeker line]
  (let [actual   (-> seeker
                     (i/slice (constantly line))
                     (i/current-line))
        expected line]
    (is (= expected actual))))

(defn slice-creating-line-when-empty [seeker]
  (let [line   (i/current-line seeker)
        actual (-> i/empty-seeker
                   (i/slice (fn [a b] (concat a line b)))
                   (i/current-line))]
    (is (= line actual))))

(defn slicing [seeker]
  (slice-removing-left-of-line seeker)
  (slice-removing-right-of-line seeker)
  (slice-enhancing-line seeker (one gen-nonempty-line))
  (slice-replacing-line seeker (one gen-nonempty-line))
  (slice-creating-line-when-empty seeker))

(defspec slicing-test
         NR-OF-TESTS
  (for-all [seeker gen-text-area]
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
      (should-be #(= (i/current-line %) line)
                 #(= (i/current-char %) (first line)))))

(defn moving [seeker]
  (bound-movement seeker)
  (unbound-movement seeker (one gen-nonempty-line)))

(defspec moving-test
         NR-OF-TESTS
  (for-all [seeker gen-text-area]
    (moving seeker)))

(defbench moving-bench moving)

;; V. Retrieving

(defn get-previous-char [seeker text]
  (let [[line1 _] text]
    (-> seeker
        (i/peer (fn [a b] (concat a text b)))
        (i/start-x)
        (i/move-y inc)
        (should-be #(-> (i/move-left %) (i/previous-char) (= (last line1)))
                   #(-> (i/move-y % dec) (i/move-x inc) (i/previous-char) (= (first line1)))))))

(defn get-current-char [seeker]
  (let [actual   (-> seeker (i/start) (i/current-char))
        expected (-> seeker (:lines) (first) (first))]
    (is (= expected actual))))

(defn retrieving [seeker]
  (get-previous-char seeker (many gen-nonempty-line 2))
  (get-current-char seeker))

(defspec retrieving-test
         NR-OF-TESTS
  (for-all [seeker gen-text-area]
    (retrieving seeker)))

(defbench retrieving-bench retrieving)

;; VI. Progressing

(defn simple-progress [seeker line]
  (-> (i/start seeker)
      (i/split (fn [a b] [line a b]))
      (should-be #(-> % (i/move-right) (after-cursor? %)))))

(defn stop-progress-when-text-ends [seeker]
  (-> (i/end seeker)
      (should-be #(-> % (i/move-right) (same-cursor? %)))))

(defn wrap-when-line-ends [seeker text]
  (-> (i/start seeker)
      (i/peer (fn [a b] (concat text a b)))
      (i/end-x)
      (should-be #(-> % (i/move-right) (after-cursor? %)))))

(defn progressing [seeker]
  (simple-progress seeker (one gen-nonempty-line))
  (stop-progress-when-text-ends seeker)
  (wrap-when-line-ends seeker (one gen-nonempty-text)))

(defspec progressing-test
         NR-OF-TESTS
  (for-all [seeker gen-text-area]
    (progressing seeker)))

(defbench progressing-bench progressing)

;; VII. Regressing

(defn simple-regress [seeker line]
  (-> seeker
      (i/split (fn [a b] [a line b]))
      (should-be #(-> % (i/move-right) (i/move-left) (same-cursor? %)))))

(defn stop-regress-when-start [seeker]
  (-> (i/start-x seeker)
      (i/move-left)
      (same-cursor? seeker)))

(defn wrap-when-line-starts [seeker text]
  (-> seeker
      (i/peer (fn [a b] (concat a b text)))
      (i/end)
      (i/start-x)
      (should-be #(-> % (i/move-left) (before-cursor? %)))))

(defn regressing [seeker]
  (simple-regress seeker (one gen-nonempty-line))
  (stop-regress-when-start seeker)
  (wrap-when-line-starts seeker (one gen-nonempty-text)))

(defspec regressing-test
         NR-OF-TESTS
  (for-all [seeker gen-text-area]
    (regressing seeker)))

(defbench regressing-bench regressing)

;; VIII. Climbing

(defn simple-ascent [seeker text]
  (-> seeker
      (i/peer (fn [a b] (concat a text b)))
      (i/move-y inc)
      (should-be #(-> (i/move-up %) (before-cursor? %)))))

(defn ascent-to-end-of-line [seeker]
  "explicit data, because second line has to be larger than the first"
  (let [some-text [[\a \b] [\a \b \c]]]
    (-> seeker
        (i/peer (fn [a b] (concat a some-text b)))
        (should-be #(-> (i/move-y % inc) (i/end-x) (i/move-up) (same-cursor? (i/end-x %)))))))

(defn climbing [seeker]
  (simple-ascent seeker (one gen-nonempty-text))
  (ascent-to-end-of-line seeker))

(defspec climbing-test
         NR-OF-TESTS
  (for-all [seeker gen-text-area]
    (climbing seeker)))

(defbench climbing-bench climbing)

;; IX. Falling

(defn simple-descent [seeker text]
  (-> seeker
      (i/peer (fn [a b] (concat a text b)))
      (should-be #(-> % (i/move-down) (after-cursor? %)))))

(defn descent-to-end-of-line [seeker]
  "explicit data, because first line has to be larger than the second"
  (let [some-text [[\a \b \c] [\d \e]]]
    (-> seeker
        (i/peer (fn [a b] (concat a some-text b)))
        (i/move-y inc)
        (should-be #(-> (i/move-y % dec) (i/end-x) (i/move-down) (same-cursor? (i/end-x %)))))))

(defn falling [seeker]
  (simple-descent seeker (one gen-nonempty-text))
  (descent-to-end-of-line seeker))

(defspec falling-test
         NR-OF-TESTS
  (for-all [seeker gen-text-area]
    (falling seeker)))

(defbench falling-bench falling)

;; X. Deleting

(defn delete-previous [seeker]
  (let [some-text [[\a \b]]]
    (-> seeker
        (i/peer (fn [a b] (concat a b some-text)))
        (i/end)
        (should-be #(-> % (i/backspace) (i/previous-char) (= \a))
                   #(-> % (i/backspace) (i/backspace) (i/previous-char) (nil?))
                   #(-> % (i/backspace) (i/backspace) (i/backspace) (i/equivalent? seeker))))))

(defn delete-next [seeker]
  (let [some-text [[\a \b]]]
    (-> seeker
        (i/peer (fn [a b] (concat some-text a b)))
        (i/start)
        (should-be #(-> % (i/delete) (i/current-char) (= \b))
                   #(-> % (i/delete) (i/delete) (i/current-char) (nil?))
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
        (should-be #(-> % (remove-line) (i/current-line) (vector) (= squiggly))
                   #(-> % (remove-line) (remove-line) (i/current-line) (vector) (= brackets))
                   #(-> % (remove-line) (remove-line) (remove-line) (i/current-line) (vector) (= round))
                   #(-> % (remove-line) (remove-line) (remove-line) (remove-line) (i/equivalent? seeker))))))

(defn delete-omitting-single-parens [seeker]
  (letfn [(f [s c] (i/slicel s #(conj % c)))]
    (-> (i/end seeker)
        (should-be #(-> (f % \)) (i/move-right) (i/backspace) (i/equivalent? (f % \))))
                   #(-> (f % \]) (i/move-right) (i/backspace) (i/equivalent? (f % \])))
                   #(-> (f % \}) (i/move-right) (i/backspace) (i/equivalent? (f % \})))
                   #(-> (f % \") (i/move-right) (i/backspace) (i/equivalent? (f % \")))))))

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
  (for-all [seeker gen-text-area]
    (deleting seeker)))

(defbench deleting-bench deleting)

;; XI. Inserting

(defn insert-literal [seeker c]
  (let [actual (-> seeker (i/insert c) (i/previous-char))]
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
                     (i/start-selection)
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
  (for-all [seeker gen-text-area]
    (inserting seeker)))

(defbench inserting-bench inserting)

;; XII. Jumping

(deftest jumps-over-words
  (let [text   (i/from-string "these are words")
        t      (-> text (i/current-char))
        space1 (-> text (i/jump-right) (i/current-char))
        a      (-> text (i/jump-right) (i/jump-right) (i/current-char))
        space2 (-> text (i/jump-right) (i/jump-right) (i/jump-right) (i/current-char))
        w      (-> text (i/jump-right) (i/jump-right) (i/jump-right) (i/jump-right) (i/current-char))]
    (is (= t \t))
    (is (= space1 \space))
    (is (= a \a))
    (is (= space2 \space))
    (is (= w \w))))

(deftest jumps-right-over-open-paired-tokens
  (let [text         (i/from-string "({[\"word")
        open-paren   (-> text (i/current-char))
        open-curly   (-> text (i/jump-right) (i/current-char))
        open-bracket (-> text (i/jump-right) (i/jump-right) (i/current-char))
        open-quote   (-> text (i/jump-right) (i/jump-right) (i/jump-right) (i/current-char))
        word         (-> text (i/jump-right) (i/jump-right) (i/jump-right) (i/jump-right) (i/current-char))]
    (is (= open-paren \())
    (is (= open-curly \{))
    (is (= open-bracket \[))
    (is (= open-quote \"))
    (is (= word \w))))

(deftest jumps-left-over-open-paired-tokens
  (let [text         (i/from-string "word({[\"")
        open-quote   (-> text (i/end) (i/jump-left) (i/current-char))
        open-bracket (-> text (i/end) (i/jump-left) (i/jump-left) (i/current-char))
        open-curly   (-> text (i/end) (i/jump-left) (i/jump-left) (i/jump-left) (i/current-char))
        open-paren   (-> text (i/end) (i/jump-left) (i/jump-left) (i/jump-left) (i/jump-left) (i/current-char))
        word         (-> text (i/end) (i/jump-left) (i/jump-left) (i/jump-left) (i/jump-left) (i/jump-left) (i/current-char))]
    (is (= open-quote \"))
    (is (= open-bracket \[))
    (is (= open-curly \{))
    (is (= open-paren \())
    (is (= word \w))))

(deftest jumps-right-over-closed-paired-tokens
  (let [text           (i/from-string ")}]\"word")
        closed-paren   (-> text (i/current-char))
        closed-curly   (-> text (i/jump-right) (i/current-char))
        closed-bracket (-> text (i/jump-right) (i/jump-right) (i/current-char))
        closed-quote   (-> text (i/jump-right) (i/jump-right) (i/jump-right) (i/current-char))
        word           (-> text (i/jump-right) (i/jump-right) (i/jump-right) (i/jump-right) (i/current-char))]
    (is (= closed-paren \)))
    (is (= closed-curly \}))
    (is (= closed-bracket \]))
    (is (= closed-quote \"))
    (is (= word \w))))

(deftest jumps-left-over-closed-paired-tokens
  (let [text           (i/from-string "word)}]\"")
        closed-quote   (-> text (i/end) (i/jump-left) (i/current-char))
        closed-bracket (-> text (i/end) (i/jump-left) (i/jump-left) (i/current-char))
        closed-curly   (-> text (i/end) (i/jump-left) (i/jump-left) (i/jump-left) (i/current-char))
        closed-paren   (-> text (i/end) (i/jump-left) (i/jump-left) (i/jump-left) (i/jump-left) (i/current-char))
        word           (-> text (i/end) (i/jump-left) (i/jump-left) (i/jump-left) (i/jump-left) (i/jump-left) (i/current-char))]
    (is (= closed-quote \"))
    (is (= closed-bracket \]))
    (is (= closed-curly \}))
    (is (= closed-paren \)))
    (is (= word \w))))

(deftest jumps-until-spaces
  (let [text  (i/from-string "long word")
        space (-> text (i/jump-right) (i/current-char))]
    (is (= space \space))))

(deftest jumps-over-spaces
  (let [text (i/from-string "spaced      out")
        o    (-> text (i/jump-right) (i/jump-right) (i/current-char))]
    (is (= o \o))))

(deftest jumps-between-lines
  (let [text (i/from-string "first\nsecond")
        s    (-> text
                 (i/jump-right)
                 (i/jump-right)
                 (i/current-char))
        f    (-> text
                 (i/end)
                 (i/jump-left)
                 (i/jump-left)
                 (i/jump-left)
                 (i/current-char))]
    (is (= s \s))
    (is (= f \f))))

;; XIII. Selecting

;; FIXME
(defn select-invariants-with-single-chars [seeker]
  "The selection is not kept in sync with every selection action.
  It's currently simply calculated at the end relative to the cursor.
  Which is not that good've an idea"
  (let [region (-> "1"
                   (i/from-string)
                   (process' [right select-left select-left select-right])
                   (:selection))]
    (is (= (:start region) [0 0]))
    (is (= (:end region) [1 0]))))

(defn select-single-chars [seeker text]
  (let [result (-> seeker
                   (i/peer (fn [a b] (concat a text b)))
                   (i/start-x)
                   (i/start-selection)
                   (i/move-x inc)
                   (i/adjust-selection)
                   (:selection))
        [xs _] (:start result)
        [xe _] (:end result)]
    (is (= xs 0))
    (is (= xe 1))))

(defn select-when-jumping [seeker]
  (-> seeker
      (i/start-x)
      (i/start-selection)
      (i/end-x)
      (i/adjust-selection)
      (should-be #(-> (i/selection %) (:start) (= (:cursor (i/start-x %))))
                 #(-> (i/selection %) (:end) (= (:cursor (i/end-x %)))))))

(defn select-lines [seeker text]
  (-> seeker
      (i/peer (fn [a b] (concat a text b)))
      (i/start-x)
      (i/start-selection)
      (i/move-down)
      (i/adjust-selection)
      (should-be #(-> (i/selection %) (:start) (= (:cursor (i/move-up %))))
                 #(-> (i/selection %) (:end) (= (:cursor %))))))

(defn select-blocks [seeker]
  (-> seeker
      (i/select-all)
      (should-be #(-> (i/selection %) (:start) (= (:cursor (i/start %))))
                 #(-> (i/selection %) (:end) (= (:cursor (i/end %)))))))

(defn selecting [seeker]
  (select-invariants-with-single-chars seeker)
  (select-single-chars seeker (one gen-nonempty-text))
  (select-when-jumping seeker)
  (select-lines seeker (one gen-nonempty-text))
  (select-blocks seeker))

(defspec selecting-test
         NR-OF-TESTS
  (for-all [seeker gen-text-area]
    (selecting seeker)))


(defbench selecting-bench selecting)

;; XVI. Joining

(defn join-texts [seeker1 seeker2]
  (-> (i/conjoin seeker1 seeker2)
      (should-be #(i/equivalent? % (i/seeker (concat (:lines seeker1) (:lines seeker2))))
                 #(-> (i/current-line %) (= (i/current-line seeker2))))))

(defn join-texts-with-selections [seeker1 seeker2]
  (let [s1 (-> seeker1 (i/start-selection) (i/end))
        s2 (-> seeker2 (i/start-selection) (i/end))]
    (-> (i/conjoin s1 s2)
        (should-be #(-> (i/selection %) (:end) (= (:cursor (i/end %))))))))

(defn joining [seeker1]
  (let [seeker2 (first (gen/sample gen-text-area))]
    (join-texts seeker1 seeker2)
    (join-texts-with-selections seeker1 seeker2)))

(defspec joining-test
         NR-OF-TESTS
  (for-all [seeker gen-text-area]
    (joining seeker)))

(defbench joining-bench joining)

;; XV. Expanding

(deftest expands-to-words
  (let [text            (i/from-string "some line\nanother line")
        word1-start     (-> text (i/start-x) (i/expand) (i/extract) (i/current-line))
        word1-middle    (-> text (i/move-right) (i/move-right) (i/expand) (i/extract) (i/current-line))
        sentence-middle (-> text (i/jump-right) (i/expand) (i/extract) (:lines))
        word2-start     (-> text (i/jump-right) (i/jump-right) (i/expand) (i/extract) (i/current-line))
        word2-middle    (-> text (i/jump-right) (i/jump-right) (i/move-right) (i/move-right) (i/expand) (i/extract) (i/current-line))
        word2-end       (-> text (i/jump-right) (i/jump-right) (i/jump-right) (i/expand) (i/extract) (i/current-line))]
    (is (= word1-start word1-middle [\s \o \m \e]))
    (is (= word2-start word2-middle word2-end [\l \i \n \e]))
    (is (= sentence-middle [[\s \o \m \e \space \l \i \n \e] [\a \n \o \t \h \e \r \space \l \i \n \e]]))))

(deftest expands-over-exprs
  (->> [[\( \)] [\[ \]] [\{ \}]]
       (run! (fn [[l r]]
               (let [text        (i/from-string (str l l "some  word" r r))
                     from-start  (-> text (i/start-x) (i/expand) (i/extract) (i/current-line))
                     from-end    (-> text (i/end-x) (i/expand) (i/extract) (i/current-line))
                     from-middle (-> text
                                     (i/start-x)
                                     (i/jump-right)
                                     (i/jump-right)
                                     (i/jump-right)
                                     (i/expand)
                                     (i/extract)
                                     (i/current-line))]
                 (is (= from-start from-end [l l \s \o \m \e \space \space \w \o \r \d r r]))
                 (is (= from-middle [l \s \o \m \e \space \space \w \o \r \d r])))))))

(deftest expands-from-words-to-exprs
  (let [parens [[\( \)] [\[ \]] [\{ \}]]]
    (run!
      (fn [[ol or]]
        (run!
          (fn [[il ir]]
            (let [text   (-> (str ol il "some  word" ir or)
                             (i/from-string)
                             (i/jump-right)
                             (i/jump-right))
                  word   (-> text
                             (i/expand)
                             (i/extract)
                             (i/current-line))
                  expr   (-> text
                             (i/expand)
                             (i/expand)
                             (i/extract)
                             (i/current-line))
                  o-expr (-> text
                             (i/expand)
                             (i/expand)
                             (i/expand)
                             (i/extract)
                             (i/current-line))]
              (is (= word [\s \o \m \e]))
              (is (= expr [il \s \o \m \e \space \space \w \o \r \d ir]))
              (is (= o-expr [ol il \s \o \m \e \space \space \w \o \r \d ir or]))))
          parens))
      parens)))

;; XVI. Copying

(defn copy-within-line [seeker line1 line2]
  (let [some-text [(concat line1 [\space] line2)]]
    (-> seeker
        (i/peer (fn [a b] (concat a some-text b)))
        (i/start-x)
        (i/start-selection)
        (i/jump-right)
        (should-be #(-> (i/copy %) (:clipboard) (i/equivalent? (i/seeker [line1])))))))

(defn copy-lines [seeker some-text]
  (-> seeker
      (i/peer (fn [a b] (concat a some-text b)))
      (i/start-x)
      (i/start-selection)
      (i/move-y #(-> (count some-text) (dec) (+ %)))
      (i/end-x)
      (should-be #(-> (i/copy %) (:clipboard) (i/equivalent? (i/seeker some-text))))))

(defn copying [seeker]
  (copy-within-line seeker (one gen-nonempty-line) (one gen-nonempty-line))
  (copy-lines seeker (one gen-nonempty-text)))

(defspec copying-test
         NR-OF-TESTS
  (for-all [seeker gen-text-area]
    (copying seeker)))

(defbench copying-bench copying)

;; XVII. Cutting

(defn cut-within-line [seeker line1 line2]
  (let [some-text [(concat line1 [\space] line2)]]
    (-> seeker
        (i/peer (fn [a b] (concat a some-text b)))
        (i/start-x)
        (i/start-selection)
        (i/jump-right)
        (should-be #(-> (i/cut %) (:clipboard) (i/equivalent? (i/seeker [line1])))
                   #(i/equivalent? (i/cut %) (i/backspace %))))))

(defn cut-lines [seeker some-text]
  (-> seeker
      (i/peer (fn [a b] (concat a some-text b)))
      (i/start-x)
      (i/start-selection)
      (i/move-y #(-> (count some-text) (dec) (+ %)))
      (i/end-x)
      (should-be #(-> (i/cut %) (:clipboard) (i/equivalent? (i/seeker some-text)))
                 #(i/equivalent? (i/cut %) (i/backspace %)))))

(defn cutting [seeker]
  (cut-within-line seeker (one gen-nonempty-line) (one gen-nonempty-line))
  (cut-lines seeker (one gen-nonempty-text)))

(defspec cutting-test
         NR-OF-TESTS
  (for-all [seeker gen-text-area]
    (cutting seeker)))

(defbench cutting-bench cutting)

;; XVIII. Pasting

(defn paste-new-line [seeker line]
  (-> seeker
      (i/peer (fn [a b] (concat a [line] b)))
      (i/start-x)
      (i/start-selection)
      (i/end-x)
      (i/copy)
      (should-be #(-> % (i/end) (i/break) (i/paste) (i/current-line) (= line)))))

(defn paste-within-line [seeker line]
  (-> seeker
      (i/peer (fn [a b] (concat a [line] b)))
      (i/start-x)
      (i/start-selection)
      (i/jump-right)
      (i/copy)
      (i/deselect)
      (should-be #(-> (i/paste %) (i/current-line) (= (concat line line)))
                 #(-> (i/paste %)
                      (i/jump-left)
                      (i/paste)
                      (i/current-line) (= (concat line line line))))))

(defn paste-concatenating-lines [seeker line1 line2 line3]
  (-> seeker
      (i/peer (fn [a b] (concat a [line1 line2 line3] b)))
      (i/start-x)
      (i/start-selection)
      (i/move-down)
      (i/end-x)
      (i/copy)
      (i/deselect)
      (should-be #(-> % (i/paste) (i/current-line) (= line2))
                 #(-> % (i/paste) (i/move-up) (i/current-line) (= (concat line2 line1))))))

(defn paste-overriding-selection [seeker some-text line2]
  (let [expected (i/peer seeker (fn [a b] (concat a [line2] b)))
        actual   (-> seeker
                     (assoc :clipboard (i/seeker [line2]))
                     (i/peer (fn [a b] (concat a some-text b)))
                     (i/start-x)
                     (i/start-selection)
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
  (for-all [seeker gen-text-area]
    (pasting seeker)))

(defbench pasting-bench pasting)

;; XIX. Parens matching

(deftest parens-matching-test
  (let [s1 (i/seeker [[\[ \[ \[ \4 \5 \[ \] \]]])
        s2 (i/seeker [[\[ \( \a \{ \} \b \) \]]])
        s3 (i/seeker [[\[ \) \( \} \{ \) \] \[]])
        s4 (i/seeker [[\a \b \4 \( \)]])]
    ;; s1 mismatches
    (is (= nil (-> s1 (i/start) (i/find-pair))))
    (is (= nil (-> s1 (i/move-right) (i/find-pair))))

    ;; s1 matches
    (is (not= nil (-> s1 (i/end-x) (i/find-pair))))
    (is (not= nil (-> s1 (i/end-x) (i/move-left) (i/find-pair))))
    (is (not= nil (-> s1 (i/move-right) (i/move-right) (i/move-right) (i/find-pair))))
    (is (not= nil (-> s1 (i/move-right) (i/move-right) (i/find-pair))))
    (is (not= nil (-> s1 (i/end-x) (i/move-left) (i/find-pair))))
    (is (not= nil (-> s1 (i/move-right) (i/move-right) (i/find-pair))))

    ;;;; s2 matches
    (is (not= nil (-> s2 (i/find-pair))))
    (is (not= nil (-> s2 (i/end-x) (i/find-pair))))
    (is (not= nil (-> s2 (i/move-right) (i/find-pair))))
    (is (not= nil (-> s2 (i/move-right) (i/move-right) (i/find-pair))))
    (is (not= nil (-> s2 (i/move-right) (i/move-right) (i/move-right) (i/find-pair))))
    (is (not= nil (-> s2 (i/end-x) (i/move-left) (i/find-pair))))
    (is (not= nil (-> s2 (i/end-x) (i/move-left) (i/move-left) (i/find-pair))))

    ;; s3 matches
    (is (not= nil (-> s3 (i/move-right) (i/move-right) (i/move-right) (i/find-pair))))
    (is (not= nil (-> s3 (i/move-right) (i/move-right) (i/move-right) (i/find-pair))))
    (is (not= nil (-> s3 (i/end-x) (i/move-left) (i/move-left) (i/find-pair))))

    ;; s3 mismatches
    (is (= nil (-> s3 (i/end-x) (i/move-left) (i/find-pair))))
    (is (= nil (-> s3 (i/end-x) (i/move-left) (i/move-left) (i/move-left) (i/find-pair))))

    ;; s3 matches
    (is (not= nil (-> s3 (i/move-right) (i/find-pair))))
    (is (not= nil (-> s3 (i/find-pair))))
    (is (not= nil (-> s3 (i/move-right) (i/move-right) (i/find-pair))))
    (is (not= nil (-> s3 (i/move-right) (i/move-right) (i/find-pair))))

    ;; s4 mismatches
    (is (= nil (-> s4 (i/find-pair))))
    (is (= nil (-> s4 (i/find-pair))))
    (is (= nil (-> s4 (i/move-right) (i/find-pair))))
    (is (= nil (-> s4 (i/move-right) (i/move-right) (i/find-pair))))
    (is (= nil (-> s4 (i/end-x) (i/move-left) (i/move-left) (i/move-left) (i/find-pair))))

    ;; s4 matches
    (is (not= nil (-> s4 (i/move-right) (i/move-right) (i/move-right) (i/find-pair))))
    (is (not= nil (-> s4 (i/end-x) (i/move-left) (i/find-pair))))))

;; XX. Extracting

(deftest extracts-empty-text
  (let [text (-> i/empty-seeker (i/expand) (i/extract) (i/current-line))]
    (is (= text []))))

(deftest extracts-selected-larger-text-from-same-line
  (let [extracted (-> "this is some line"
                      (i/from-string)
                      (i/jump-right)
                      (i/start-selection)
                      (i/jump-right)
                      (i/jump-right)
                      (i/jump-right)
                      (i/move-right)
                      (i/adjust-selection)
                      (i/extract)
                      (i/current-line))]
    (is (= extracted [\space \i \s \space \s \o]))))

(deftest extracts-selected-smaller-text-from-same-line
  (let [extracted (-> "a"
                      (i/from-string)
                      (i/start-selection)
                      (i/jump-right)
                      (i/jump-left)
                      (i/jump-right)
                      (i/adjust-selection)
                      (i/extract)
                      (i/current-line))]
    (is (= extracted [\a]))))

(deftest extracts-selected-text-from-multiple-lines
  (let [extracted (-> "first one\nseconds it"
                      (i/from-string)
                      (i/jump-right)
                      (i/start-selection)
                      (i/jump-right)
                      (i/jump-right)
                      (i/jump-right)
                      (i/jump-right)
                      (i/adjust-selection)
                      (i/extract)
                      (:lines))]
    (is (= extracted [[\space \o \n \e] [\s \e \c \o \n \d \s \space]]))))

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
  (let [history (one (gen-history {:prefilled-size 50 :element-size 3}))]
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
  (for-all [seeker gen-text-area]
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
  (for-all [seeker gen-text-area]
    (history seeker)))

(defbench history-test history)