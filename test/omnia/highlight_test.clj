(ns omnia.highlight-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :refer [split]]
            [clojure.set :refer [difference]]
            [omnia.more :refer [map-vals]]
            [omnia.highlighting :as h]))

(def triggers
  (assoc h/triggers
    (:node h/function) #{\x \^ \*}
    (:node h/text) #{\x \^ \*}))

(defn node-set [states]
  (->> states (map :node) (set)))

(defn catalog-detections [stream]
  (h/fold'
    (fn [vec _ emission value]
      (conj vec [emission value])) [] stream))

(defn test-emissions [data]
  (doseq [[chars emissions] data
          :let [detections (catalog-detections chars)]]
    (is (= emissions (map first detections)))))

(defn catalog-transitions [f]
  (for [[t-node chars] triggers
        char chars
        :let [next-node (f char)]]
    (if (= t-node next-node)
      {:allowed [t-node]}
      {:disallowed [t-node]})))

(defn transitions [state]
  (->> state
       (:transition)
       (catalog-transitions)
       (apply merge-with concat)
       (map-vals set)))

(defn diffed-message [case expected-set actual-set]
  (if (> (count expected-set) (count actual-set))
    (format "In the %s case: `expected` set also contained: %s" case (difference expected-set actual-set))
    (format "In the %s case: `actual` set also contained: %s" case (difference actual-set expected-set))))

(defn test-transitions [{:keys [state allowed disallowed]}]
  (let [{actual-allowed    :allowed
         actual-disallowed :disallowed} (transitions state)
        expected-allowed    (node-set allowed)
        expected-disallowed (node-set disallowed)]
    (is (= expected-allowed actual-allowed)
        (diffed-message "allowed" expected-allowed actual-allowed))
    (is (= expected-disallowed actual-disallowed)
        (diffed-message "disallowed" expected-disallowed actual-disallowed))))

(deftest detect-lists
  (test-emissions [["()" [:text :list :list]]
                   ["(hello)" [:text :list :function :list]]
                   [")rest" [:text :list :text]]
                   [")(" [:text :list :list]]
                   ["( abc )" [:text :list :text :text :text :list]]
                   ["a (\n )" [:text :text :text :list :text :text :list]]]))

(deftest detect-vectors
  (test-emissions [["[]" [:text :vector :vector]]
                   ["[hello]" [:text :vector :text :vector]]
                   ["][" [:text :vector :vector]]
                   ["]rest" [:text :vector :text]]
                   ["a [\n ]" [:text :text :text :vector :text :text :vector]]]))

(deftest detect-maps
  (test-emissions [["{}" [:text :map :map]]
                   ["{hello}" [:text :map :text :map]]
                   ["}{" [:text :map :map]]
                   ["}rest" [:text :map :text]]
                   ["a {\n }" [:text :text :text :map :text :text :map]]]))

(deftest detect-breaks
  (test-emissions [["\n\n\n" [:text :text :text :text]]
                   ["a\nb\nc" [:text :text :text :text :text :text]]
                   [";ab \"" [:text :comment]]
                   [";ab \n\"" [:text :comment :text :string]]]))

(deftest detect-spaces
  (test-emissions [[" " [:text :text]]
                   ["a b" [:text :text :text :text]]]))

(deftest detect-comments
  (test-emissions [[";comment  123" [:text :comment]]
                   [";;comment(" [:text :comment]]
                   [";+13adasd" [:text :comment]]
                   ["; \n(" [:text :comment :text :list]]]))

(deftest detect-functions
  (test-emissions [["(hello)" [:text :list :function :list]]
                   ["()" [:text :list :list]]
                   ["(bla" [:text :list :function]]]))

(deftest detect-chars
  (test-emissions [["\\a" [:text :character]]
                   ["\\\\" [:text :character]]
                   ["\\abc" [:text :text]]
                   ["\\" [:text :text]]
                   ["\\\\\\" [:text :text]]]))

(deftest detect-keywords
  (test-emissions [[":hello" [:text :keyword]]
                   ["a:bello" [:text :text]]
                   ["::bla" [:text :keyword]]
                   [":1:a:b" [:text :keyword]]
                   [": " [:text :keyword :text]]]))

(deftest detect-numbers
  (test-emissions [["123" [:text :number]]
                   ["123a" [:text :number]]
                   ["+123" [:text :number]]
                   ["-123" [:text :number]]
                   ["+-12" [:text :text]]
                   ["+" [:text :text]]
                   ["-" [:text :text]]
                   ["a123" [:text :text]]
                   ["a+1" [:text :text]]
                   ["a-1" [:text :text]]]))

(deftest detect-strings
  (test-emissions [["\"\"" [:text :string :string]]
                   ["\"hello\"" [:text :string :string]]
                   ["\"hello bla bhe\"" [:text :string :string]]
                   ["\"line" [:text :string]]
                   ["\"line1\nline2\"" [:text :string :string]]]))


(deftest detect-text
  (test-emissions [["abc" [:text :text]]
                   ["a1+" [:text :text]]]))

(deftest detect-words
  (test-emissions [["true" [:text :word]]
                   ["false" [:text :word]]
                   ["nil" [:text :word]]
                   ["truex" [:text :text]]
                   ["falsex" [:text :text]]
                   ["nilx" [:text :text]]]))

(deftest open-list-transitions
  (test-transitions
    {:state      h/open-list
     :allowed    [h/open-list
                  h/close-list
                  h/open-vector
                  h/close-vector
                  h/open-map
                  h/close-map
                  h/open-string
                  h/key-word
                  h/com-ment
                  h/number
                  h/space
                  h/break
                  h/function
                  h/character]
     :disallowed [h/close-string
                  h/word
                  h/text]}))

(deftest closed-list-transitions
  (test-transitions
    {:state      h/close-string
     :allowed    [h/open-list
                  h/close-list
                  h/open-vector
                  h/close-vector
                  h/open-map
                  h/close-map
                  h/open-string
                  h/key-word
                  h/com-ment
                  h/number
                  h/space
                  h/break
                  h/word
                  h/text
                  h/character]
     :disallowed [h/close-string
                  h/function]}))

(deftest open-vector-transitions
  (test-transitions
    {:state      h/open-vector
     :allowed    [h/open-list
                  h/close-list
                  h/open-vector
                  h/close-vector
                  h/open-map
                  h/close-map
                  h/open-string
                  h/key-word
                  h/com-ment
                  h/number
                  h/space
                  h/break
                  h/word
                  h/text
                  h/character]
     :disallowed [h/close-string
                  h/function]}))

(deftest close-vector-transitions
  (test-transitions
    {:state      h/close-vector
     :allowed    [h/open-list
                  h/close-list
                  h/open-vector
                  h/close-vector
                  h/open-map
                  h/close-map
                  h/open-string
                  h/key-word
                  h/com-ment
                  h/number
                  h/space
                  h/break
                  h/word
                  h/text
                  h/character]
     :disallowed [h/close-string
                  h/function]}))

(deftest open-map-transitions
  (test-transitions
    {:state      h/open-map
     :allowed    [h/open-list
                  h/close-list
                  h/open-vector
                  h/close-vector
                  h/open-map
                  h/close-map
                  h/open-string
                  h/key-word
                  h/com-ment
                  h/number
                  h/space
                  h/break
                  h/word
                  h/text
                  h/character]
     :disallowed [h/close-string
                  h/function]}))

(deftest close-map-transitions
  (test-transitions
    {:state      h/close-map
     :allowed    [h/open-list
                  h/close-list
                  h/open-vector
                  h/close-vector
                  h/open-map
                  h/close-map
                  h/open-string
                  h/key-word
                  h/com-ment
                  h/number
                  h/space
                  h/break
                  h/word
                  h/text
                  h/character]
     :disallowed [h/close-string
                  h/function]}))

(deftest open-string-transitions
  (test-transitions
    {:state      h/open-string
     :allowed    [h/close-string]
     :disallowed [h/open-list
                  h/close-list
                  h/open-vector
                  h/close-vector
                  h/open-map
                  h/close-map
                  h/open-string
                  h/key-word
                  h/com-ment
                  h/number
                  h/space
                  h/break
                  h/word
                  h/text
                  h/character
                  h/function]}))

(deftest close-string-transitions
  (test-transitions
    {:state      h/close-string
     :allowed    [h/open-list
                  h/close-list
                  h/open-vector
                  h/close-vector
                  h/open-map
                  h/close-map
                  h/open-string
                  h/key-word
                  h/com-ment
                  h/number
                  h/space
                  h/break
                  h/word
                  h/text
                  h/character]
     :disallowed [h/close-string
                  h/function]}))

(deftest keyword-transitions
  (test-transitions
    {:state      h/key-word
     :allowed    [h/open-list
                  h/close-list
                  h/open-vector
                  h/close-vector
                  h/open-map
                  h/close-map
                  h/open-string
                  h/key-word
                  h/com-ment
                  h/space
                  h/break
                  h/character]
     :disallowed [h/close-string
                  h/function
                  h/word
                  h/text
                  h/number]}))

(deftest comment-transitions
  (test-transitions
    {:state      h/com-ment
     :allowed    [h/break
                  h/com-ment]
     :disallowed [h/open-list
                  h/close-list
                  h/open-vector
                  h/close-vector
                  h/open-map
                  h/close-map
                  h/open-string
                  h/key-word
                  h/space
                  h/character
                  h/open-string
                  h/close-string
                  h/function
                  h/word
                  h/text
                  h/number]}))

(deftest number-transitions
  (test-transitions
    {:state      h/number
     :allowed    [h/open-list
                  h/close-list
                  h/open-vector
                  h/close-vector
                  h/open-map
                  h/close-map
                  h/open-string
                  h/com-ment
                  h/number
                  h/space
                  h/break]
     :disallowed [h/close-string
                  h/function
                  h/word
                  h/text
                  h/key-word
                  h/character]}))

(deftest word-transitions
  (test-transitions
    {:state      h/word
     :allowed    [h/open-list
                  h/close-list
                  h/open-vector
                  h/close-vector
                  h/open-map
                  h/close-map
                  h/open-string
                  h/com-ment
                  h/number
                  h/space
                  h/break
                  h/word
                  h/key-word
                  h/character]
     :disallowed [h/close-string
                  h/function
                  h/text]}))