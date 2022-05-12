(ns omnia.syntax-highlighting-test
  (:require [schema.core :as s]
            [omnia.repl.syntax-highlighting :as h]
            [omnia.schema.syntax :as sy]
            [clojure.test :refer [deftest is]]
            [clojure.set :refer [difference]]
            [omnia.schema.syntax-highlighting :refer [Node State]]
            [omnia.schema.syntax :refer [SyntaxElement]]))

(s/def TransitionCatalog
  {:allowed    #{Node}
   :disallowed #{Node}})

(s/def EmissionCatalog
  [{:state      State
    :emission   SyntaxElement
    :accumulate [Character]}])

;; because there aren't any explicit triggers for function and text, we provide some for testing purposes
(def triggers
  (assoc h/triggers
    (:node h/function) #{\x \^ \*}
    (:node h/text)     #{\x \^ \*}))

(defn node-set [states]
  (->> states (map :node) (set)))

(s/defn catalog-detections :- EmissionCatalog
  [string :- s/Str]
  (h/fold'
    (fn [catalog state emission accumulate]
      (conj catalog {:state      state
                     :emission   emission
                     :accumulate accumulate})) [] string))

(defn test-detections
  [specs]
  (doseq [[string states emissions] specs
          :let [detections         (-> string (catalog-detections) (rest)) ;; remove the initial `break` state
                detected-states    (mapv :state detections)
                detected-emissions (mapv :emission detections)]]
    (is (= emissions detected-emissions) (str "For input: " string))
    (is (= (node-set states) (node-set detected-states)) (str "For input: " string))))

(s/defn transition-catalog :- TransitionCatalog
  [state :- State]
  (let [f    (:transition state)
        init {:allowed #{} :disallowed #{}}]
    (reduce-kv
      (fn [catalog node chars]
        (reduce (fn [catalog char]
                  (let [node' (f char)]
                    (if (= node node')
                      (update catalog :allowed conj node)
                      (update catalog :disallowed conj node)))) catalog chars)) init triggers)))

(defn diffed-message [case expected-set actual-set]
  (if (> (count expected-set) (count actual-set))
    (format "In the %s case: `expected` set also contained: %s" case (difference expected-set actual-set))
    (format "In the %s case: `actual` set also contained: %s" case (difference actual-set expected-set))))

(defn test-transitions [{:keys [state allowed disallowed]}]
  (let [{actual-allowed    :allowed
         actual-disallowed :disallowed} (transition-catalog state)
        expected-allowed    (node-set allowed)
        expected-disallowed (node-set disallowed)]
    (is (= expected-allowed actual-allowed)
        (diffed-message "allowed" expected-allowed actual-allowed))
    (is (= expected-disallowed actual-disallowed)
        (diffed-message "disallowed" expected-disallowed actual-disallowed))))

(deftest detect-functions
  (test-detections
    [["(heh)"   [h/open-list h/function h/function h/function h/close-list] [sy/lists sy/functions sy/lists]]
     ["(+ab)"   [h/open-list h/number h/number h/number h/close-list]       [sy/lists sy/functions sy/lists]]
     ["(-ab)"   [h/open-list h/number h/number h/number h/close-list]       [sy/lists sy/functions sy/lists]]
     ["(nil "   [h/open-list h/function h/function h/function h/space]      [sy/lists sy/functions sy/texts]]]))

(deftest detect-lists
  (test-detections
    [["()"      [h/open-list h/close-list]                                  [sy/lists sy/lists]]
     [")heh"    [h/close-list h/text h/text h/text]                         [sy/lists sy/texts]]
     [")("      [h/close-list h/open-list]                                  [sy/lists sy/lists]]
     ["( ab )"  [h/open-list h/space h/text h/text h/space h/close-list]    [sy/lists sy/texts sy/texts sy/texts sy/lists]]
     ["a (\n )" [h/text h/space h/open-list h/break h/space h/close-list]   [sy/texts sy/texts sy/lists sy/texts sy/texts sy/lists]]]))

(deftest detect-vectors
  (test-detections
    [["[]"      [h/open-vector h/close-vector]                                [sy/vectors sy/vectors]]
     ["[heh]"   [h/open-vector h/text h/text h/text h/close-vector]           [sy/vectors sy/texts sy/vectors]]
     ["]["      [h/close-vector h/open-vector]                                [sy/vectors sy/vectors]]
     ["]heh"    [h/close-vector h/text h/text h/text]                         [sy/vectors sy/texts]]
     ["h[ "     [h/text h/open-vector h/space]                                [sy/texts sy/vectors sy/texts]]
     ["a [\n ]" [h/text h/space h/open-vector h/break h/space h/close-vector] [sy/texts sy/texts sy/vectors sy/texts sy/texts sy/vectors]]]))

(deftest detect-maps
  (test-detections
    [["{}"      [h/open-map h/close-map]                                [sy/maps sy/maps]]
     ["{heh}"   [h/open-map h/text h/text h/text h/close-map]           [sy/maps sy/texts sy/maps]]
     ["}{"      [h/close-map h/open-map]                                [sy/maps sy/maps]]
     ["}heh"    [h/close-map h/text h/text h/text]                      [sy/maps sy/texts]]
     ["a {\n }" [h/text h/space h/open-map h/break h/space h/close-map] [sy/texts sy/texts sy/maps sy/texts sy/texts sy/maps]]]))

(deftest detect-breaks
  (test-detections
    [["a\n\n"    [h/text h/break]                                         [sy/texts sy/texts]]
     ["a\nb\nc"  [h/text h/break h/text h/break h/text]                   [sy/texts sy/texts sy/texts sy/texts sy/texts]]
     [";a \n\""  [h/com-ment h/com-ment h/com-ment h/break h/open-string] [sy/comments sy/texts sy/strings]]]))

(deftest detect-spaces
  (test-detections
    [["   " [h/space]               [sy/texts]]
     ["a b" [h/text h/space h/text] [sy/texts sy/texts sy/texts]]]))

(deftest detect-comments
  (test-detections
    [[";c 1"   [h/com-ment h/com-ment h/com-ment h/com-ment] [sy/comments]]
     [";;c("   [h/com-ment h/com-ment h/com-ment]            [sy/comments]]
     [";+1\""  [h/com-ment h/com-ment h/com-ment h/com-ment] [sy/comments]]
     ["; \n("  [h/com-ment h/com-ment h/break h/open-list]   [sy/comments sy/texts sy/lists]]]))

(deftest detect-chars
  (test-detections
    [["\\a"        [h/escape h/character]                      [sy/characters sy/characters]]
     ["\\\\"       [h/escape h/character h/escape h/character] [sy/characters sy/characters]]
     ["\\abc"      [h/escape h/character h/text h/text]        [sy/characters sy/characters sy/texts]]
     ["\\"         [h/escape]                                  [sy/characters]]
     ["\\space"    [h/escape h/special-character]              [sy/characters sy/characters]]
     ["\\newline"  [h/escape h/special-character]              [sy/characters sy/characters]]
     ["\\spacex"   [h/escape h/special-character]              [sy/characters sy/texts]]
     ["\\newlinex" [h/escape h/special-character]              [sy/characters sy/texts]]
     ["[\\a \\]]"  [h/open-vector h/escape h/character h/space
                    h/escape h/character h/close-vector]       [sy/vectors sy/characters sy/characters
                                                               sy/texts sy/characters sy/characters sy/vectors]]]))

(deftest detect-keywords
  (test-detections
    [[":he"   [h/key-word h/key-word h/key-word]             [sy/keywords]]
     ["(:he"  [h/open-list h/key-word h/key-word h/key-word] [sy/lists sy/keywords]]
     ["a:he"  [h/text]                                       [sy/texts]]
     ["::he"  [h/key-word h/key-word h/key-word h/key-word]  [sy/keywords]]
     [":1:a"  [h/key-word h/key-word h/key-word h/key-word]  [sy/keywords]]
     [": \""  [h/key-word h/space h/open-string]             [sy/keywords sy/texts sy/strings]]
     [":\n\"" [h/key-word h/break h/open-string]             [sy/keywords sy/texts sy/strings]]]))

(deftest detect-numbers
  (test-detections
    [["123"   [h/number]             [sy/numbers]]
     ["123a"  [h/number]             [sy/numbers]]
     ["+123"  [h/number]             [sy/numbers]]
     ["-123"  [h/number]             [sy/numbers]]
     ["+-12"  [h/number]             [sy/texts]]
     ["-+12"  [h/number]             [sy/texts]]
     ["+"     [h/number]             [sy/texts]]
     ["-"     [h/number]             [sy/texts]]
     ["a123"  [h/text]               [sy/texts]]
     ["a+1"   [h/text]               [sy/texts]]
     ["a-1"   [h/text]               [sy/texts]]
     ["(+123" [h/open-list h/number] [sy/lists sy/numbers]] ;; FIXME
     ["(-123" [h/open-list h/number] [sy/lists sy/numbers]]]))

(deftest detect-strings
  (test-detections
    [["\"\""     [h/open-string h/close-string]                                           [sy/strings sy/strings]]
     ["\"h\""    [h/open-string h/open-string h/close-string]                             [sy/strings sy/strings]]
     ["\"h b\""  [h/open-string h/open-string h/open-string h/open-string h/close-string] [sy/strings sy/strings]]
     ["\"l"      [h/open-string h/open-string]                                            [sy/strings]]
     ["\"l\nl\"" [h/open-string h/open-string h/open-string h/open-string h/close-string] [sy/strings sy/strings]]]))

(deftest detect-text
  (test-detections
    [["abc"  [h/text]                  [sy/texts]]
     ["a1+"  [h/text]                  [sy/texts]]
     ["a 1"  [h/text h/space h/number] [sy/texts sy/texts sy/numbers]]]))

(deftest detect-words
  (test-detections
    [["true"   [h/word h/word h/word h/word]               [sy/words]]
     ["false"  [h/word h/word h/word h/word h/word]        [sy/words]]
     ["nil"    [h/word h/word h/word]                      [sy/words]]
     ["truex"  [h/word h/word h/word h/word h/word]        [sy/texts]]
     ["falsex" [h/word h/word h/word h/word h/word h/word] [sy/texts]]
     ["nilx"   [h/word h/word h/word h/word]               [sy/texts]]
     ["n:/1"   [h/word]                                    [sy/texts]]]))

(deftest detect-commas
  (test-detections
    [[","       [h/comma]                          [sy/commas]]
     ["a,b"     [h/text h/comma h/text]            [sy/texts sy/commas sy/texts]]
     ["1,2"     [h/number h/comma h/number]        [sy/numbers sy/commas sy/numbers]]
     [":a,1"    [h/key-word h/comma h/number]      [sy/keywords sy/commas sy/numbers]]
     ["(,)"     [h/open-list h/comma h/close-list] [sy/lists sy/commas sy/lists]]
     ["nil,"    [h/word h/comma]                   [sy/words sy/commas]]
     ["\\c,\\a" [h/escape h/character, h/comma h/escape, h/character]  [sy/characters sy/characters sy/commas sy/characters sy/characters]]]))

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
                  h/escape
                  h/comma]
     :disallowed [h/close-string
                  h/word
                  h/text
                  h/special-character]}))

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
                  h/escape
                  h/comma]
     :disallowed [h/close-string
                  h/function
                  h/special-character]}))

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
                  h/escape
                  h/comma]
     :disallowed [h/close-string
                  h/function
                  h/special-character]}))

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
                  h/escape
                  h/comma]
     :disallowed [h/close-string
                  h/function
                  h/special-character]}))

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
                  h/escape
                  h/comma]
     :disallowed [h/close-string
                  h/function
                  h/special-character]}))

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
                  h/escape
                  h/comma]
     :disallowed [h/close-string
                  h/function
                  h/special-character]}))

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
                  h/escape
                  h/function
                  h/comma
                  h/special-character]}))

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
                  h/escape
                  h/comma]
     :disallowed [h/close-string
                  h/function
                  h/special-character]}))

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
                  h/escape
                  h/comma]
     :disallowed [h/close-string
                  h/function
                  h/word
                  h/text
                  h/number
                  h/special-character]}))

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
                  h/escape
                  h/open-string
                  h/close-string
                  h/function
                  h/word
                  h/text
                  h/number
                  h/comma
                  h/special-character]}))

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
                  h/break
                  h/comma]
     :disallowed [h/close-string
                  h/function
                  h/word
                  h/text
                  h/key-word
                  h/escape
                  h/special-character]}))

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
                  h/word
                  h/comma]
     :disallowed [h/close-string
                  h/function
                  h/text
                  h/key-word
                  h/escape
                  h/number
                  h/special-character]}))

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
                  h/escape
                  h/text
                  h/comma]
     :disallowed [h/close-string
                  h/function
                  h/special-character]}))

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
                  h/escape
                  h/text
                  h/comma]
     :disallowed [h/close-string
                  h/function
                  h/special-character]}))

(deftest character-transitions
  (test-transitions
    {:state      h/escape
     :allowed    [h/space
                  h/break
                  h/special-character]
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
                  h/text
                  h/escape
                  h/comma]}))

(deftest comma-transitions
  (test-transitions
    {:state      h/comma
     :allowed    [h/open-list
                  h/close-list
                  h/open-vector
                  h/close-vector
                  h/open-map
                  h/close-map
                  h/open-string
                  h/com-ment
                  h/number
                  h/key-word
                  h/word
                  h/text
                  h/space
                  h/break
                  h/escape
                  h/comma]
     :disallowed [h/function
                  h/close-string
                  h/special-character]}))
