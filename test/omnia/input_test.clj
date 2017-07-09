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

(def gen-seeker (->> gen/char-ascii
                     (gen/vector)
                     (gen/vector)
                     (gen/fmap i/seeker)
                     (gen/fmap #(let [y (rand-line %)
                                      x (rand-pos % y)]
                                  (i/move % (fn [_] [x y]))))))

(defmacro <=> [this-seeker that-seeker]
  `(is (= (:lines ~this-seeker) (:lines ~that-seeker))))

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

(defspec moving
         100
         (for-all [seeker gen-seeker]
                  (bounded seeker)))