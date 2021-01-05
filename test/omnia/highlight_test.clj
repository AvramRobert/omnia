(ns omnia.highlight-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :refer [for-all]]
            [clojure.test.check.generators :as gen]
            [clojure.string :as s]
            [omnia.highlight :as h]
            [omnia.highlighting :as h2]))

(def ^:const NR-OF-TESTS 100)

(def state-chars
  {h/-list     [\( \)]
   h/-vector   [\[ \]]
   h/-map      [\{ \}]
   h/-char     [\\]
   h/-number   h/numbers
   h/-string   [\"]
   h/-string*  [\"]
   h/-function [\l \a \m \b \d \a]
   h/-text     [\s \o \m \e]
   h/-word     [\f \t \n]
   h/-break    [\newline]
   h/-space    [\space]
   h/-comment  [\;]
   h/-keyword  [\:]})

(def ids (keys state-chars))

(defn chars-for [state]
  (get state-chars state []))

(defn gen-tokens [& tokens] (gen/elements tokens))

(defn in? [tokens]
  (let [s (set tokens)]
    #(contains? s %)))

(defn in-vector [stream]
  (h/foldl (fn [v [e s]] (conj v [e (:id s)])) [] stream))

(defn detect [node-set stream]
  (->> stream
       (h2/fold'
         (fn [vec state emission value]
           (if (node-set (:node state))
             (conj vec [emission value])
             vec)) [])))

(defn identify [char-set stream]
  (->> stream
       (partition-by #(contains? char-set %))
       (filter #(clojure.set/subset? (set %) char-set))))

(defn label-as [label stream]
  (->> stream (map #(vector label %))))

(def gen-sign-token (gen-tokens \+ \-))
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

(defn- gen-with-token [& gens]
  (->> (vec gens)
       (gen/one-of)
       (gen/vector)))

(defn gen-alpha-num-with-token [gen-token]
  (gen-with-token gen/char-alphanumeric gen-token))

(defn gen-alpha-with-token [gen-token]
  (gen-with-token gen/char-alpha gen-token))

(defn detections [emissions & states]
  "Based on given `states`, extracts the detected tokens from `emissions`
  produced by the highlighting engine.
  Returns a collection of vectors of the tokens detected."
  (let [s (set states)]
    (->> emissions
         (filter (fn [[_ type]] (contains? s type)))
         (map first))))

(defn grouped-gens [generated tokens]
  "Uses `tokens` to extract either series or individual tokens from
  the `generated` vector of chars.
  Returns a collection with vectors of the `tokens` it found in succession.
  Is equivalent to aggregated state detection"
  (let [grpd? (in? tokens)]
    (->> generated
         (partition-by grpd?)
         (filter (fn [elms] (some grpd? elms))))))

(defn pred-gens [generated {:keys [detect? reset? include-detected? include-reset?]
                            :or   {reset?            (fn [_] false)
                                   include-detected? false
                                   include-reset?    false}}]
  "Uses the predicates `detect?` and `reset?` to extract either series or
  individual tokens from the `generated` vector of characters.
  `detect?` is a predicate applied on the current char stating when an extraction
            should be started.
  `reset?` is a predicate applied on the current char stating when an ongoing
           extraction should be stopped.
  Both `include-detected?` and `include-reset?` denote if the characters
  `detect?` and `reset?` initially identified should be added to the extracted tokens.
  Returns a collection with vectors of the `tokens` it found.
  Is equivalent to aggregate state detection relative to some other characters."
  (let [prepend   (fn [item coll] (concat [item] coll))
        append    (fn [item coll] (concat coll [item]))
        unviable? (comp not detect?)
        unreset?  (comp not reset?)]
    (loop [items []
           rem   generated]
      (if (empty? rem)
        (filter (comp not empty?) items)
        (let [pre    (drop-while unviable? rem)
              pro    (->> pre (rest) (take-while unreset?))
              post   (->> pre (rest) (drop-while unreset?))
              viable (first pre)
              reset  (first post)]
          (recur (cond->> pro
                          (and include-detected? viable) (prepend viable)
                          (and include-reset? reset) (append reset)
                          :always (conj items))
                 (if include-reset? (rest post) post)))))))

(defn transition! [state disallowed]
  "Given a `state`, it forces it through all the available transitions
  and checks them. Disallowed transitions are specified
  in `disallowed` as key-value pairs. The key is the disallowed
  state-identifier, whist the value is the actual state-identifier
  the transition needs to have should it encounter
  a character that would lead to the disallowed state."
  (doseq [id ids
          c  (chars-for id)]
    (= (:id (h/transition state c)) (get disallowed id id))))

(defspec detect-lists
         NR-OF-TESTS
  (for-all [tokens (gen-alpha-num-with-token gen-list-token)]
           (let [gens       (grouped-gens tokens [\( \)])
                 detections (-> (in-vector tokens)
                                (detections h/-list))]
             (is (= gens detections)))))

(defspec detect-vectors
         NR-OF-TESTS
  (for-all [tokens (gen-alpha-num-with-token gen-vector-token)]
           (let [gens       (grouped-gens tokens [\[ \]])
                 detections (-> (in-vector tokens)
                                (detections h/-vector))]
             (is (= gens detections)))))

(defspec detect-maps
         NR-OF-TESTS
  (for-all [tokens (gen-alpha-num-with-token gen-map-token)]
           (let [gens       (grouped-gens tokens [\{ \}])
                 detections (-> (in-vector tokens)
                                (detections h/-map))]
             (is (= gens detections)))))

(defspec detect-breaks
         NR-OF-TESTS
  (for-all [tokens (gen-alpha-num-with-token gen-break-token)]
           (let [gens       (grouped-gens tokens [\newline])
                 detections (-> (in-vector tokens)
                                (detections h/-break))]
             (is (= gens detections)))))

(defspec detect-spaces
         NR-OF-TESTS
  (for-all [tokens (gen-alpha-num-with-token gen-space-token)]
           (let [gens       (grouped-gens tokens [\space])
                 detections (-> (in-vector tokens)
                                (detections h/-space))]
             (is (= gens detections)))))

(defspec detect-spaces'
         NR-OF-TESTS
  (for-all [tokens (gen-alpha-num-with-token gen-space-token)]
           (let [actual (detect #{h2/space-node} tokens)
                 expected (->> tokens
                               (identify #{\space})
                               (label-as :text))]
             (is (= actual expected)))))

(defspec detect-comments
         NR-OF-TESTS
  (for-all [tokens (gen-alpha-num-with-token gen-comment-token)]
           (let [gens       (pred-gens tokens
                                       {:detect?           (in? [\;])
                                        :include-detected? true})
                 detections (-> (in-vector tokens)
                                (detections h/-comment))]
             (is (= gens detections)))))

(defspec detect-functions
         NR-OF-TESTS
  (for-all [tokens (gen-alpha-with-token gen-list-token)]
           (let [gens       (pred-gens tokens
                                       {:detect? (in? [\(])
                                        :reset?  (in? [\( \)])})
                 detections (-> (in-vector tokens)
                                (detections h/-function))]
             (is (= gens detections)))))

(defspec detect-chars
         NR-OF-TESTS
  (for-all [tokens (gen-alpha-with-token gen-char-token)]
           (let [gens       (pred-gens tokens
                                       {:detect?           (in? [\\])
                                        :include-detected? true})
                 detections (-> (in-vector tokens)
                                (detections h/-char))]
             (is (= gens detections)))))

(defspec detect-keywords
         NR-OF-TESTS
  (for-all [tokens (gen-with-token gen-space-token gen-keyword-token)]
           (let [gens       (pred-gens tokens
                                       {:detect?           (in? [\:])
                                        :reset?            (in? [\space])
                                        :include-detected? true})
                 detections (-> (in-vector tokens)
                                (detections h/-keyword))]
             (is (= gens detections)))))

(defspec detect-keywords'
         NR-OF-TESTS
  (for-all [tokens (gen-with-token gen-space-token gen-keyword-token)]
           (let [actual (detect #{h2/keyword-node} tokens)
                 expected (->> tokens
                               (identify #{\:})
                               (label-as :keyword))]
             (is (= actual expected)))))

(defspec detect-numbers
         NR-OF-TESTS
  (for-all [tokens (gen-with-token gen-space-token gen-number-token)]
           (let [gens       (pred-gens tokens
                                       {:detect?           (in? h/numbers)
                                        :reset?            (in? [\space])
                                        :include-detected? true})
                 detections (-> (in-vector tokens)
                                (detections h/-number))]
             (is (= gens detections)))))

(defspec detect-numbers'
         NR-OF-TESTS
  (for-all [tokens (gen-with-token gen-space-token gen-number-token)]
           (let [numbers (->> (range 0 10) (map #(Character/forDigit % 10)) (set))
                 expected (->> tokens
                               (identify numbers)
                               (label-as :number))
                 actual  (detect #{h2/number-node} tokens)]
             (is (= expected actual)))))

(defspec detect-signed-numbers
         NR-OF-TESTS
  (for-all [tokens (gen-with-token gen-space-token gen-number-token gen-sign-token)]
           (let [valid-number? #(-> (comp not (in? [\+ \-]))
                                    (filter %)
                                    (s/join)
                                    (BigInteger.))
                 processed     (-> (in-vector tokens)
                                   (detections h/-number))]
             (if (seq processed)
               (is (every? valid-number? processed))
               (is true)))))

(defspec detect-strings
         NR-OF-TESTS
  (for-all [tokens (gen-alpha-num-with-token gen-string-token)]
           (let [gens      (pred-gens tokens
                                      {:detect?           (in? [\"])
                                       :reset?            (in? [\"])
                                       :include-detected? true
                                       :include-reset?    true})
                 processed (-> (in-vector tokens)
                               (detections h/-string h/-string*)
                               (->> (reduce concat)))]
             (is (= (apply concat gens) processed)))))

(defspec detect-text
         NR-OF-TESTS
  (for-all [tokens (gen-with-token gen-space-token gen/char-alpha)]
           (let [gens       (pred-gens tokens
                                       {:detect?           #(Character/isAlphabetic (int %))
                                        :reset?            (in? [\space])
                                        :include-detected? true})
                 detections (-> (in-vector tokens)
                                (detections h/-text)
                                (->> (filter (comp not empty?))))] ;; the interpreter always emits an empty text with the first transition
             (is (= gens detections)))))

(defspec detect-words
         NR-OF-TESTS
  (letfn [(words [tokens]
            (-> (apply str tokens)
                (s/split #" ")
                (->> (mapv #(vec (.toCharArray %))))
                (->> (filter #(contains? h/words %)))))]
    (for-all [tokens (->> (gen-with-token gen-space-token gen-word-token)
                          (gen/fmap (comp vec flatten)))]
             (let [gens       (words tokens)
                   detections (-> (in-vector tokens)
                                  (detections h/-word))]
               (is (= gens detections))))))

(deftest transition-from-lists
  (transition! h/->open-list {h/-text    h/-function
                              h/-word    h/-function
                              h/-string* h/-string})
  (transition! h/->close-list {h/-function h/-text
                               h/-string*  h/-string}))

(deftest transition-from-vectors
  (transition! h/->open-vector {h/-function h/-text
                                h/-string*  h/-string})
  (transition! h/->close-vector {h/-function h/-text
                                 h/-string*  h/-string}))

(deftest transition-from-maps
  (transition! h/->open-map {h/-function h/-text
                             h/-string*  h/-string})
  (transition! h/->close-map {h/-function h/-text
                              h/-string*  h/-string}))

(deftest transition-from-words
  (transition! h/->word {h/-function h/-word
                         h/-number   h/-word
                         h/-keyword  h/-word
                         h/-text     h/-word
                         h/-string*  h/-string}))

(deftest transition-from-chars
  (transition! h/->char {h/-list     h/-char
                         h/-vector   h/-char
                         h/-map      h/-char
                         h/-function h/-char
                         h/-text     h/-char
                         h/-word     h/-char
                         h/-number   h/-char
                         h/-string   h/-char
                         h/-string*  h/-char
                         h/-comment  h/-char
                         h/-keyword  h/-char}))

(deftest transition-from-numbers
  (transition! h/->number {h/-function h/-number
                           h/-text     h/-number
                           h/-word     h/-number
                           h/-keyword  h/-number
                           h/-char     h/-number
                           h/-string*  h/-string}))

(deftest transition-from-signed-numbers
  (transition! h/->signed-number {h/-function h/-number
                                  h/-text     h/-number
                                  h/-word     h/-number
                                  h/-keyword  h/-number
                                  h/-char     h/-number
                                  h/-string*  h/-string}))

(deftest transition-from-strings
  (transition! h/->open-string {h/-list     h/-string
                                h/-vector   h/-string
                                h/-map      h/-string
                                h/-function h/-string
                                h/-text     h/-string
                                h/-word     h/-string
                                h/-number   h/-string
                                h/-comment  h/-string
                                h/-keyword  h/-string
                                h/-space    h/-string
                                h/-break    h/-string
                                h/-char     h/-string
                                h/-string   h/-string*})

  (transition! h/->close-string {h/-string*  h/-string
                                 h/-function h/-text}))

(deftest transition-from-keywords
  (transition! h/->keyword {h/-string*  h/-string
                            h/-function h/-keyword
                            h/-number   h/-keyword
                            h/-text     h/-keyword
                            h/-word     h/-keyword}))

(deftest transition-from-comments
  (transition! h/->comment {h/-list     h/-comment
                            h/-vector   h/-comment
                            h/-map      h/-comment
                            h/-function h/-comment
                            h/-text     h/-comment
                            h/-word     h/-comment
                            h/-number   h/-comment
                            h/-keyword  h/-comment
                            h/-space    h/-comment
                            h/-char     h/-comment
                            h/-string   h/-comment
                            h/-string*  h/-comment}))

(deftest transition-from-breaks
  (transition! h/->break {h/-string*  h/-string
                          h/-function h/-text}))

(deftest transition-from-spaces
  (transition! h/->space {h/-string*  h/-string
                          h/-function h/-text}))

(deftest transition-from-text
  (transition! h/->text {h/-string*  h/-string
                         h/-number   h/-text
                         h/-keyword  h/-text
                         h/-word     h/-text
                         h/-function h/-text}))

(deftest transition-from-functions
  (transition! h/->function {h/-string* h/-string
                             h/-word    h/-function
                             h/-text    h/-function
                             h/-number  h/-function
                             h/-keyword h/-function})) 1