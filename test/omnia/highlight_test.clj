(ns omnia.highlight-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :refer [split]]
            [clojure.set :refer [difference]]
            [omnia.more :refer [map-vals]]
            [omnia.highlighting :as h]))

(def triggers
  (assoc h/triggers
    (:node h/function) #{\x \^ \*}
    (:node h/text)     #{\x \^ \*}))

(defn node-set [states]
  (->> states (map :node) (set)))

(defn catalog-detections [stream]
  (h/fold'
    (fn [vec state emission value]
      (conj vec {:state state
                 :emission emission
                 :value value})) [] stream))

(defn test-detections [data]
  (doseq [[chars states emissions] data
          :let [detections         (-> chars (catalog-detections) (rest)) ;; remove the initial `break` state
                detected-states    (mapv :state detections)
                detected-emissions (mapv :emission detections)]]
    (is (= emissions detected-emissions) (str "For input: " chars))
    (is (= (node-set states) (node-set detected-states)) (str "For input: " chars))))

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
  (test-detections
    [["()"      [h/open-list h/close-list]                                  [h/-list h/-list]]
     ["(heh)"   [h/open-list h/function h/function h/function h/close-list] [h/-list h/-function h/-list]]
     [")heh"    [h/close-list h/text h/text h/text]                         [h/-list h/-text]]
     [")("      [h/close-list h/open-list]                                  [h/-list h/-list]]
     ["( ab )"  [h/open-list h/space h/text h/text h/space h/close-list]    [h/-list h/-text h/-text h/-text h/-list]]
     ["a (\n )" [h/text h/space h/open-list h/break h/space h/close-list]   [h/-text h/-text h/-list h/-text h/-text h/-list]]]))

(deftest detect-vectors
  (test-detections
    [["[]"      [h/open-vector h/close-vector]                                [h/-vector h/-vector]]
     ["[heh]"   [h/open-vector h/text h/text h/text h/close-vector]           [h/-vector h/-text h/-vector]]
     ["]["      [h/close-vector h/open-vector]                                [h/-vector h/-vector]]
     ["]heh"    [h/close-vector h/text h/text h/text]                         [h/-vector h/-text]]
     ["h[ "     [h/text h/open-vector h/space]                                [h/-text h/-vector h/-text]]
     ["a [\n ]" [h/text h/space h/open-vector h/break h/space h/close-vector] [h/-text h/-text h/-vector h/-text h/-text h/-vector]]]))

(deftest detect-maps
  (test-detections
    [["{}"      [h/open-map h/close-map]                                [h/-map h/-map]]
     ["{heh}"   [h/open-map h/text h/text h/text h/close-map]           [h/-map h/-text h/-map]]
     ["}{"      [h/close-map h/open-map]                                [h/-map h/-map]]
     ["}heh"    [h/close-map h/text h/text h/text]                      [h/-map h/-text]]
     ["a {\n }" [h/text h/space h/open-map h/break h/space h/close-map] [h/-text h/-text h/-map h/-text h/-text h/-map]]]))

(deftest detect-breaks
  (test-detections
    [["a\n\n"    [h/text h/break]                                         [h/-text h/-text]]
     ["a\nb\nc"  [h/text h/break h/text h/break h/text]                   [h/-text h/-text h/-text h/-text h/-text]]
     [";a \n\""  [h/com-ment h/com-ment h/com-ment h/break h/open-string] [h/-comment h/-text h/-string]]]))

(deftest detect-spaces
  (test-detections
    [["   " [h/space]               [h/-text]]
     ["a b" [h/text h/space h/text] [h/-text h/-text h/-text]]]))

(deftest detect-comments
  (test-detections
    [[";c 1"   [h/com-ment h/com-ment h/com-ment h/com-ment] [h/-comment]]
     [";;c("   [h/com-ment h/com-ment h/com-ment]            [h/-comment]]
     [";+1\""  [h/com-ment h/com-ment h/com-ment h/com-ment] [h/-comment]]
     ["; \n("  [h/com-ment h/com-ment h/break h/open-list]   [h/-comment h/-text h/-list]]]))

(deftest detect-functions
  (test-detections
    [["(he)"   [h/open-list h/function h/function h/close-list]      [h/-list h/-function h/-list]]
     ["(nil " [h/open-list h/function h/function h/function h/space] [h/-list h/-function h/-text]]]))

(deftest detect-chars
  (test-detections
    [["\\a" [h/character h/character]                [h/-char]]
     ["\\\\" [h/character h/character]               [h/-char]]
     ["\\abc" [h/character h/character h/character]  [h/-text]]
     ["\\" [h/character]                             [h/-text]]
     ["\\\\\\" [h/character h/character h/character] [h/-text]]]))

(deftest detect-keywords
  (test-detections
    [[":he"   [h/key-word h/key-word h/key-word]             [h/-keyword]]
     ["(:he"  [h/open-list h/key-word h/key-word h/key-word] [h/-list h/-keyword]]
     ["a:he"  [h/text]                                       [h/-text]]
     ["::he"  [h/key-word h/key-word h/key-word h/key-word]  [h/-keyword]]
     [":1:a"  [h/key-word h/key-word h/key-word h/key-word]  [h/-keyword]]
     [": \""  [h/key-word h/space h/open-string]             [h/-keyword h/-text h/-string]]
     [":\n\"" [h/key-word h/break h/open-string]             [h/-keyword h/-text h/-string]]]))

(deftest detect-numbers
  (test-detections
    [["123"  [h/number] [h/-number]]
     ["123a" [h/number] [h/-number]]
     ["+123" [h/number] [h/-number]]
     ["-123" [h/number] [h/-number]]
     ["+-12" [h/number] [h/-text]]
     ["-+12" [h/number] [h/-text]]
     ["+"    [h/number] [h/-text]]
     ["-"    [h/number] [h/-text]]
     ["a123" [h/text]   [h/-text]]
     ["a+1"  [h/text]   [h/-text]]
     ["a-1"  [h/text]   [h/-text]]]))

(deftest detect-strings
  (test-detections
    [["\"\""     [h/open-string h/close-string]                                           [h/-string h/-string]]
     ["\"h\""    [h/open-string h/open-string h/close-string]                             [h/-string h/-string]]
     ["\"h b\""  [h/open-string h/open-string h/open-string h/open-string h/close-string] [h/-string h/-string]]
     ["\"l"      [h/open-string h/open-string]                                            [h/-string]]
     ["\"l\nl\"" [h/open-string h/open-string h/open-string h/open-string h/close-string] [h/-string h/-string]]]))

(deftest detect-text
  (test-detections
    [["abc"  [h/text]                  [h/-text]]
     ["a1+"  [h/text]                  [h/-text]]
     ["a 1"  [h/text h/space h/number] [h/-text h/-text h/-number]]]))

(deftest detect-words
  (test-detections
    [["true"   [h/word h/word h/word h/word]               [h/-word]]
     ["false"  [h/word h/word h/word h/word h/word]        [h/-word]]
     ["nil"    [h/word h/word h/word]                      [h/-word]]
     ["truex"  [h/word h/word h/word h/word h/word]        [h/-text]]
     ["falsex" [h/word h/word h/word h/word h/word h/word] [h/-text]]
     ["nilx"   [h/word h/word h/word h/word]               [h/-text]]
     ["n:/1"   [h/word]                                    [h/-text]]]))

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
                  h/space
                  h/break
                  h/word]
     :disallowed [h/close-string
                  h/function
                  h/text
                  h/key-word
                  h/character
                  h/number]}))

(deftest space-transitions
  (test-transitions
    {:state      h/space
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
                  h/character
                  h/text]
     :disallowed [h/close-string
                  h/function]}))

(deftest break-transitions
  (test-transitions
    {:state      h/break
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
                  h/character
                  h/text]
     :disallowed [h/close-string
                  h/function]}))

(deftest character-transitions
  (test-transitions
    {:state      h/character
     :allowed    [h/space
                  h/break
                  h/character]
     :disallowed [h/open-list
                  h/close-list
                  h/open-vector
                  h/close-vector
                  h/open-map
                  h/close-map
                  h/open-string
                  h/com-ment
                  h/number
                  h/close-string
                  h/function
                  h/key-word
                  h/word
                  h/text]}))