(ns omnia.syntax-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :refer [split]]
            [clojure.set :refer [difference]]
            [omnia.util.collection :refer [map-vals]]
            [omnia.repl.syntax :as h]
            [omnia.schema.syntax :as sh]))

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
    [["()"      [h/open-list h/close-list]                                  [sh/lists sh/lists]]
     ["(heh)"   [h/open-list h/function h/function h/function h/close-list] [sh/lists sh/functions sh/lists]]
     [")heh"    [h/close-list h/text h/text h/text]                         [sh/lists sh/texts]]
     [")("      [h/close-list h/open-list]                                  [sh/lists sh/lists]]
     ["( ab )"  [h/open-list h/space h/text h/text h/space h/close-list]    [sh/lists sh/texts sh/texts sh/texts sh/lists]]
     ["a (\n )" [h/text h/space h/open-list h/break h/space h/close-list]   [sh/texts sh/texts sh/lists sh/texts sh/texts sh/lists]]]))

(deftest detect-vectors
  (test-detections
    [["[]"      [h/open-vector h/close-vector]                                [sh/vectors sh/vectors]]
     ["[heh]"   [h/open-vector h/text h/text h/text h/close-vector]           [sh/vectors sh/texts sh/vectors]]
     ["]["      [h/close-vector h/open-vector]                                [sh/vectors sh/vectors]]
     ["]heh"    [h/close-vector h/text h/text h/text]                         [sh/vectors sh/texts]]
     ["h[ "     [h/text h/open-vector h/space]                                [sh/texts sh/vectors sh/texts]]
     ["a [\n ]" [h/text h/space h/open-vector h/break h/space h/close-vector] [sh/texts sh/texts sh/vectors sh/texts sh/texts sh/vectors]]]))

(deftest detect-maps
  (test-detections
    [["{}"      [h/open-map h/close-map]                                [sh/maps sh/maps]]
     ["{heh}"   [h/open-map h/text h/text h/text h/close-map]           [sh/maps sh/texts sh/maps]]
     ["}{"      [h/close-map h/open-map]                                [sh/maps sh/maps]]
     ["}heh"    [h/close-map h/text h/text h/text]                      [sh/maps sh/texts]]
     ["a {\n }" [h/text h/space h/open-map h/break h/space h/close-map] [sh/texts sh/texts sh/maps sh/texts sh/texts sh/maps]]]))

(deftest detect-breaks
  (test-detections
    [["a\n\n"    [h/text h/break]                                         [sh/texts sh/texts]]
     ["a\nb\nc"  [h/text h/break h/text h/break h/text]                   [sh/texts sh/texts sh/texts sh/texts sh/texts]]
     [";a \n\""  [h/com-ment h/com-ment h/com-ment h/break h/open-string] [sh/comments sh/texts sh/strings]]]))

(deftest detect-spaces
  (test-detections
    [["   " [h/space]               [sh/texts]]
     ["a b" [h/text h/space h/text] [sh/texts sh/texts sh/texts]]]))

(deftest detect-comments
  (test-detections
    [[";c 1"   [h/com-ment h/com-ment h/com-ment h/com-ment] [sh/comments]]
     [";;c("   [h/com-ment h/com-ment h/com-ment]            [sh/comments]]
     [";+1\""  [h/com-ment h/com-ment h/com-ment h/com-ment] [sh/comments]]
     ["; \n("  [h/com-ment h/com-ment h/break h/open-list]   [sh/comments sh/texts sh/lists]]]))

(deftest detect-functions
  (test-detections
    [["(he)"  [h/open-list h/function h/function h/close-list]      [sh/lists sh/functions sh/lists]]
     ["(nil " [h/open-list h/function h/function h/function h/space] [sh/lists sh/functions sh/texts]]]))

(deftest detect-chars
  (test-detections
    [["\\a"        [h/escape h/character]                      [sh/characters sh/characters]]
     ["\\\\"       [h/escape h/character h/escape h/character] [sh/characters sh/characters]]
     ["\\abc"      [h/escape h/character h/text h/text]        [sh/characters sh/characters sh/texts]]
     ["\\"         [h/escape]                                  [sh/characters]]
     ["\\space"    [h/escape h/special-character]              [sh/characters sh/characters]]
     ["\\newline"  [h/escape h/special-character]              [sh/characters sh/characters]]
     ["\\spacex"   [h/escape h/special-character]              [sh/characters sh/texts]]
     ["\\newlinex" [h/escape h/special-character]              [sh/characters sh/texts]]
     ["[\\a \\]]"  [h/open-vector h/escape h/character h/space
                    h/escape h/character h/close-vector]       [sh/vectors sh/characters sh/characters
                                                               sh/texts sh/characters sh/characters sh/vectors]]]))

(deftest detect-keywords
  (test-detections
    [[":he"   [h/key-word h/key-word h/key-word]             [sh/keywords]]
     ["(:he"  [h/open-list h/key-word h/key-word h/key-word] [sh/lists sh/keywords]]
     ["a:he"  [h/text]                                       [sh/texts]]
     ["::he"  [h/key-word h/key-word h/key-word h/key-word]  [sh/keywords]]
     [":1:a"  [h/key-word h/key-word h/key-word h/key-word]  [sh/keywords]]
     [": \""  [h/key-word h/space h/open-string]             [sh/keywords sh/texts sh/strings]]
     [":\n\"" [h/key-word h/break h/open-string]             [sh/keywords sh/texts sh/strings]]]))

(deftest detect-numbers
  (test-detections
    [["123"  [h/number] [sh/numbers]]
     ["123a" [h/number] [sh/numbers]]
     ["+123" [h/number] [sh/numbers]]
     ["-123" [h/number] [sh/numbers]]
     ["+-12" [h/number] [sh/texts]]
     ["-+12" [h/number] [sh/texts]]
     ["+"    [h/number] [sh/texts]]
     ["-"    [h/number] [sh/texts]]
     ["a123" [h/text]   [sh/texts]]
     ["a+1"  [h/text]   [sh/texts]]
     ["a-1"  [h/text]   [sh/texts]]]))

(deftest detect-strings
  (test-detections
    [["\"\""     [h/open-string h/close-string]                                           [sh/strings sh/strings]]
     ["\"h\""    [h/open-string h/open-string h/close-string]                             [sh/strings sh/strings]]
     ["\"h b\""  [h/open-string h/open-string h/open-string h/open-string h/close-string] [sh/strings sh/strings]]
     ["\"l"      [h/open-string h/open-string]                                            [sh/strings]]
     ["\"l\nl\"" [h/open-string h/open-string h/open-string h/open-string h/close-string] [sh/strings sh/strings]]]))

(deftest detect-text
  (test-detections
    [["abc"  [h/text]                  [sh/texts]]
     ["a1+"  [h/text]                  [sh/texts]]
     ["a 1"  [h/text h/space h/number] [sh/texts sh/texts sh/numbers]]]))

(deftest detect-words
  (test-detections
    [["true"   [h/word h/word h/word h/word]               [sh/words]]
     ["false"  [h/word h/word h/word h/word h/word]        [sh/words]]
     ["nil"    [h/word h/word h/word]                      [sh/words]]
     ["truex"  [h/word h/word h/word h/word h/word]        [sh/texts]]
     ["falsex" [h/word h/word h/word h/word h/word h/word] [sh/texts]]
     ["nilx"   [h/word h/word h/word h/word]               [sh/texts]]
     ["n:/1"   [h/word]                                    [sh/texts]]]))

(deftest detect-commas
  (test-detections
    [[","       [h/comma]                          [sh/commas]]
     ["a,b"     [h/text h/comma h/text]            [sh/texts sh/commas sh/texts]]
     ["1,2"     [h/number h/comma h/number]        [sh/numbers sh/commas sh/numbers]]
     [":a,1"    [h/key-word h/comma h/number]      [sh/keywords sh/commas sh/numbers]]
     ["(,)"     [h/open-list h/comma h/close-list] [sh/lists sh/commas sh/lists]]
     ["nil,"    [h/word h/comma]                   [sh/words sh/commas]]
     ["\\c,\\a" [h/escape h/character, h/comma h/escape, h/character]  [sh/characters sh/characters sh/commas sh/characters sh/characters]]]))

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