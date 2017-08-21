(ns omnia.highlight-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :refer [for-all]]
            [clojure.test.check.generators :as gen]
            [clojure.string :as s]
            [omnia.highlight-beta :as h]))
(defn gen-tokens [& tokens] (gen/elements tokens))

(defn in? [tokens]
  (let [s (set tokens)]
    #(contains? s %)))

(def gen-list-token (gen-tokens \( \)))
(def gen-vector-token (gen-tokens \[ \]))
(def gen-map-token (gen-tokens \{ \}))
(def gen-break-token (gen-tokens \newline))
(def gen-space-token (gen-tokens \space))
(def gen-comment-token (gen-tokens \;))
(def gen-char-token (gen-tokens \\))
(def gen-keyword-token (gen-tokens \:))
(def gen-string-token (gen-tokens \"))
(def gen-number-token (apply gen-tokens h/numbers))
(def gen-word-token (apply gen-tokens h/words))

(defn- gen-with-token [gen-chars gen-token]
  (->> [gen-chars gen-token]
       (gen/one-of)
       (gen/vector)))

(defn gen-alpha-num-with-token [gen-token]
  (gen-with-token gen/char-alphanumeric gen-token))

(defn gen-alpha-with-token [gen-token]
  (gen-with-token gen/char-alpha gen-token))

(defn state-detections [emissions & states]
  (let [s (set states)]
    (->> emissions
         (filter (fn [[_ type]] (contains? s type)))
         (map first))))

(defn grouped-gens [generated tokens]
  (let [grpd? (in? tokens)]
    (->> generated
         (partition-by grpd?)
         (filter (fn [elms] (some grpd? elms))))))

(defn pred-gens [generated {:keys [detect? reset? with-detect? with-reset?]
                            :or {reset? (fn [_] false)
                                 with-detect? false
                                 with-reset? false}}]
  (let [prepend (fn [item coll] (concat [item] coll))
        append (fn [item coll] (concat coll [item]))
        unviable? (comp not detect?)
        unreset? (comp not reset?)]
    (loop [items []
           rem generated]
      (if (empty? rem)
        (filter (comp not empty?) items)
        (let [pre (drop-while unviable? rem)
              pro (->> pre (rest) (take-while unreset?))
              post (->> pre (rest) (drop-while unreset?))
              viable (first pre)
              reset (first post)]
          (recur (cond->> pro
                          (and with-detect? viable) (prepend viable)
                          (and with-reset? reset) (append reset)
                          :always (conj items))
                 (if with-reset? (rest post) post)))))))

(defspec detect-lists
         100
         (for-all [tokens (gen-alpha-num-with-token gen-list-token)]
                  (let [gens (grouped-gens tokens [\( \)])
                        detections (-> (h/process tokens vector)
                                       (state-detections h/-list))]
                    (is (= gens detections)))))

(defspec detect-vectors
         100
         (for-all [tokens (gen-alpha-num-with-token gen-vector-token)]
                  (let [gens (grouped-gens tokens [\[ \]])
                        detections (-> (h/process tokens vector)
                                       (state-detections h/-vector))]
                    (is (= gens detections)))))

(defspec detect-maps
         100
         (for-all [tokens (gen-alpha-num-with-token gen-map-token)]
                  (let [gens (grouped-gens tokens [\{ \}])
                        detections (-> (h/process tokens vector)
                                       (state-detections h/-map))]
                    (is (= gens detections)))))

(defspec detect-breaks
         100
         (for-all [tokens (gen-alpha-num-with-token gen-break-token)]
                  (let [gens (grouped-gens tokens [\newline])
                        detections (-> (h/process tokens vector)
                                       (state-detections h/-break))]
                    (is (= gens detections)))))

(defspec detect-spaces
         100
         (for-all [tokens (gen-alpha-num-with-token gen-space-token)]
                  (let [gens (grouped-gens tokens [\space])
                        detections (-> (h/process tokens vector)
                                       (state-detections h/-space))]
                    (is (= gens detections)))))

(defspec detect-comments
         100
         (for-all [tokens (gen-alpha-num-with-token gen-comment-token)]
                  (let [gens (pred-gens tokens
                                        {:detect? (in? [\;])
                                         :with-detect? true})
                        detections (-> (h/process tokens vector)
                                       (state-detections h/-comment))]
                    (is (= gens detections)))))

(defspec detect-functions
         100
         (for-all [tokens (gen-alpha-with-token gen-list-token)]
                  (let [gens (pred-gens tokens
                                        {:detect? (in? [\(])
                                         :reset? (in? [\( \)])})
                        detections (-> (h/process tokens vector)
                                       (state-detections h/-function))]
                    (is (= gens detections)))))

(defspec detect-chars
         100
         (for-all [tokens (gen-alpha-with-token gen-char-token)]
                  (let [gens (pred-gens tokens
                                        {:detect? (in? [\\])
                                         :with-detect? true})
                        detections (-> (h/process tokens vector)
                                       (state-detections h/-char))]
                    (is (= gens detections)))))

(defspec detect-keywords
         100
         (for-all [tokens (gen-with-token gen-space-token gen-keyword-token)]
                  (let [gens (pred-gens tokens
                                        {:detect? (in? [\:])
                                         :reset? (in? [\space])
                                         :with-detect? true})
                        detections (-> (h/process tokens vector)
                                       (state-detections h/-keyword))]
                    (is (= gens detections)))))

(defspec detect-numbers
         100
         (for-all [tokens (gen-with-token gen-space-token gen-number-token)]
                  (let [gens (pred-gens tokens
                                        {:detect? (in? h/numbers)
                                         :reset? (in? [\space])
                                         :with-detect? true})
                        detections (-> (h/process tokens vector)
                                       (state-detections h/-number))]
                    (is (= gens detections)))))

(defspec detect-strings
         100
         (for-all [tokens (gen-alpha-num-with-token gen-string-token)]
                  (let [gens (pred-gens tokens
                                        {:detect? (in? [\"])
                                         :reset? (in? [\"])
                                         :with-detect? true
                                         :with-reset? true})
                        processed (-> (h/process tokens vector)
                                      (state-detections h/-string h/-string*)
                                      (->> (reduce concat)))]
                    (is (= (apply concat gens) processed)))))

(defspec detect-text
         100
         (for-all [tokens (gen-with-token gen-space-token gen/char-alpha)]
                  (let [gens (pred-gens tokens
                                        {:detect? #(Character/isAlphabetic (int %))
                                         :reset? (in? [\space])
                                         :with-detect? true})
                        detections (-> (h/process tokens vector)
                                       (state-detections h/-text)
                                       (->> (filter (comp not empty?))))] ;; the interpreter always emits an empty text with the first transition
                    (is (= gens detections)))))

(defspec detect-words
         100
         (letfn [(words [tokens]
                   (-> (apply str tokens)
                       (s/split #" ")
                       (->> (mapv #(vec (.toCharArray %))))
                       (->> (filter #(contains? h/words %)))))]
           (for-all [tokens (->> (gen-with-token gen-space-token gen-word-token)
                                 (gen/fmap (comp vec flatten)))]
                    (let [gens (words tokens)
                          detections (-> (h/process tokens vector)
                                         (state-detections h/-word))]
                      (is (= gens detections))))))

(deftest transition-from-lists (is true))
(deftest transition-from-vectors (is true))
(deftest transition-from-maps (is true))
(deftest transition-from-words (is true))
(deftest transition-from-chars (is true))
(deftest transition-from-numbers (is true))
(deftest transition-from-strings (is true))
(deftest transition-from-keywords (is true))
(deftest transition-from-comments (is true))
(deftest transition-from-breaks (is true))
(deftest transition-from-spaces (is true))
(deftest transition-from-text (is true))
(deftest transition-from-functions (is true))