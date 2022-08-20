(ns omnia.syntax-highlighting-test
  (:require [schema.core :as s]
            [omnia.repl.syntax-highlighting :as h]
            [omnia.schema.syntax-highlighting :as sh]
            [omnia.schema.syntax :as sy]
            [clojure.test :refer [deftest is]]
            [clojure.set :refer [difference]]
            [omnia.schema.syntax-highlighting :refer [Node]]
            [omnia.schema.syntax :refer [SyntaxElement]]))

(s/def TransitionCatalog
  {:allowed    #{Node}
   :disallowed #{Node}})

(s/def EmissionCatalog
  [{:node       Node
    :emission   SyntaxElement
    :accumulate [Character]}])

;; because there aren't any explicit triggers for function and text, we provide some for testing purposes
(s/def triggers :- {Node #{Character}}
  (assoc h/triggers
    sh/-function #{\x \^ \*}
    sh/-text     #{\x \^ \*}))

(s/defn catalog-detections :- EmissionCatalog
  [string :- s/Str]
  (h/fold'
    (fn [catalog node emission accumulate]
      (conj catalog {:node       node
                     :emission   emission
                     :accumulate accumulate})) [] string))

(defn test-detections [specs]
  (doseq [[string nodes emissions] specs
          :let [detections         (-> string (catalog-detections) (rest)) ;; remove the initial `-break` node
                detected-nodes     (mapv :node detections)
                detected-emissions (mapv :emission detections)]]
    (is (= emissions detected-emissions) (str "For input: " string))
    (is (= (set nodes) (set detected-nodes)) (str "For input: " string))))

(s/defn transition-catalog :- TransitionCatalog
  [node :- Node]
  (let [default    (get h/defaults node)
        transition (get h/trigger-transitions node)
        init       {:allowed #{} :disallowed #{}}]
    (reduce-kv
      (fn [catalog node chars]
        (reduce (fn [catalog char]
                  (let [node' (transition char default)]
                    (if (= node node')
                      (update catalog :allowed conj node)
                      (update catalog :disallowed conj node)))) catalog chars)) init triggers)))

(defn diffed-message [case expected-set actual-set]
  (if (> (count expected-set) (count actual-set))
    (format "In the %s case: `expected` set also contained: %s" case (difference expected-set actual-set))
    (format "In the %s case: `actual` set also contained: %s" case (difference actual-set expected-set))))

(defn test-transitions [{:keys [node allowed disallowed]}]
  (let [{actual-allowed    :allowed
         actual-disallowed :disallowed} (transition-catalog node)]
    (is (= allowed actual-allowed)
        (diffed-message "allowed" allowed actual-allowed))
    (is (= disallowed actual-disallowed)
        (diffed-message "disallowed" disallowed actual-disallowed))))

(deftest detect-functions
  (test-detections
    [["(heh)"   [sh/-open-list sh/-function sh/-function sh/-function sh/-closed-list] [sy/lists sy/functions sy/lists]]
     ["(+ab)"   [sh/-open-list sh/-number sh/-number sh/-number sh/-closed-list]       [sy/lists sy/functions sy/lists]]
     ["(-ab)"   [sh/-open-list sh/-number sh/-number sh/-number sh/-closed-list]       [sy/lists sy/functions sy/lists]]
     ["(nil "   [sh/-open-list sh/-function sh/-function sh/-function sh/-space]       [sy/lists sy/functions sy/texts]]]))

(deftest detect-lists
  (test-detections
    [["()"      [sh/-open-list sh/-closed-list]                                          [sy/lists sy/lists]]
     [")heh"    [sh/-closed-list sh/-text sh/-text sh/-text]                             [sy/lists sy/texts]]
     [")("      [sh/-closed-list sh/-open-list]                                          [sy/lists sy/lists]]
     ["( ab )"  [sh/-open-list sh/-space sh/-text sh/-text sh/-space sh/-closed-list]    [sy/lists sy/texts sy/texts sy/texts sy/lists]]
     ["a (\n )" [sh/-text sh/-space sh/-open-list sh/-break sh/-space sh/-closed-list]   [sy/texts sy/texts sy/lists sy/texts sy/texts sy/lists]]]))

(deftest detect-vectors
  (test-detections
    [["[]"      [sh/-open-vector sh/-closed-vector]                                        [sy/vectors sy/vectors]]
     ["[heh]"   [sh/-open-vector sh/-text sh/-text sh/-text sh/-closed-vector]             [sy/vectors sy/texts sy/vectors]]
     ["]["      [sh/-closed-vector sh/-open-vector]                                        [sy/vectors sy/vectors]]
     ["]heh"    [sh/-closed-vector sh/-text sh/-text sh/-text]                             [sy/vectors sy/texts]]
     ["h[ "     [sh/-text sh/-open-vector sh/-space]                                       [sy/texts sy/vectors sy/texts]]
     ["a [\n ]" [sh/-text sh/-space sh/-open-vector sh/-break sh/-space sh/-closed-vector] [sy/texts sy/texts sy/vectors sy/texts sy/texts sy/vectors]]]))

(deftest detect-maps
  (test-detections
    [["{}"      [sh/-open-map sh/-closed-map]                                        [sy/maps sy/maps]]
     ["{heh}"   [sh/-open-map sh/-text sh/-text sh/-text sh/-closed-map]             [sy/maps sy/texts sy/maps]]
     ["}{"      [sh/-closed-map sh/-open-map]                                        [sy/maps sy/maps]]
     ["}heh"    [sh/-closed-map sh/-text sh/-text sh/-text]                          [sy/maps sy/texts]]
     ["a {\n }" [sh/-text sh/-space sh/-open-map sh/-break sh/-space sh/-closed-map] [sy/texts sy/texts sy/maps sy/texts sy/texts sy/maps]]]))

(deftest detect-breaks
  (test-detections
    [["a\n\n"    [sh/-text sh/-break]                                            [sy/texts sy/texts]]
     ["a\nb\nc"  [sh/-text sh/-break sh/-text sh/-break sh/-text]                [sy/texts sy/texts sy/texts sy/texts sy/texts]]
     [";a \n\""  [sh/-comment sh/-comment sh/-comment sh/-break sh/-open-string] [sy/comments sy/texts sy/strings]]]))

(deftest detect-spaces
  (test-detections
    [["   " [sh/-space]                   [sy/texts]]
     ["a b" [sh/-text sh/-space sh/-text] [sy/texts sy/texts sy/texts]]]))

(deftest detect-comments
  (test-detections
    [[";c 1"   [sh/-comment sh/-comment sh/-comment sh/-comment]  [sy/comments]]
     [";;c("   [sh/-comment sh/-comment sh/-comment]              [sy/comments]]
     [";+1\""  [sh/-comment sh/-comment sh/-comment sh/-comment]  [sy/comments]]
     ["; \n("  [sh/-comment sh/-comment sh/-break sh/-open-list]  [sy/comments sy/texts sy/lists]]]))

(deftest detect-chars
  (test-detections
    [["\\a"        [sh/-escape-char sh/-char]                          [sy/characters sy/characters]]
     ["\\\\"       [sh/-escape-char sh/-char sh/-escape-char sh/-char] [sy/characters sy/characters]]
     ["\\abc"      [sh/-escape-char sh/-char sh/-text sh/-text]        [sy/characters sy/characters sy/texts]]
     ["\\"         [sh/-escape-char]                                   [sy/characters]]
     ["\\space"    [sh/-escape-char sh/-special-char]                  [sy/characters sy/characters]]
     ["\\newline"  [sh/-escape-char sh/-special-char]                  [sy/characters sy/characters]]
     ["\\spacex"   [sh/-escape-char sh/-special-char]                  [sy/characters sy/texts]]
     ["\\newlinex" [sh/-escape-char sh/-special-char]                  [sy/characters sy/texts]]
     ["[\\a \\]]"  [sh/-open-vector sh/-escape-char sh/-char sh/-space
                    sh/-escape-char sh/-char sh/-closed-vector]        [sy/vectors sy/characters sy/characters
                                                                        sy/texts sy/characters sy/characters sy/vectors]]]))

(deftest detect-keywords
  (test-detections
    [[":he"   [sh/-keyword sh/-keyword sh/-keyword]             [sy/keywords]]
     ["(:he"  [sh/-open-list sh/-keyword sh/-keyword sh/-keyword] [sy/lists sy/keywords]]
     ["a:he"  [sh/-text]                                       [sy/texts]]
     ["::he"  [sh/-keyword sh/-keyword sh/-keyword sh/-keyword]  [sy/keywords]]
     [":1:a"  [sh/-keyword sh/-keyword sh/-keyword sh/-keyword]  [sy/keywords]]
     [": \""  [sh/-keyword sh/-space sh/-open-string]             [sy/keywords sy/texts sy/strings]]
     [":\n\"" [sh/-keyword sh/-break sh/-open-string]             [sy/keywords sy/texts sy/strings]]]))

(deftest detect-numbers
  (test-detections
    [["123"   [sh/-number]             [sy/numbers]]
     ["123a"  [sh/-number]             [sy/numbers]]
     ["+123"  [sh/-number]             [sy/numbers]]
     ["-123"  [sh/-number]             [sy/numbers]]
     ["+-12"  [sh/-number]             [sy/texts]]
     ["-+12"  [sh/-number]             [sy/texts]]
     ["+"     [sh/-number]             [sy/texts]]
     ["-"     [sh/-number]             [sy/texts]]
     ["a123"  [sh/-text]               [sy/texts]]
     ["a+1"   [sh/-text]               [sy/texts]]
     ["a-1"   [sh/-text]               [sy/texts]]
     ["(+123" [sh/-open-list sh/-number] [sy/lists sy/numbers]]
     ["(-123" [sh/-open-list sh/-number] [sy/lists sy/numbers]]]))

(deftest detect-strings
  (test-detections
    [["\"\""     [sh/-open-string sh/-closed-string]                                                 [sy/strings sy/strings]]
     ["\"h\""    [sh/-open-string sh/-open-string sh/-closed-string]                                 [sy/strings sy/strings]]
     ["\"h b\""  [sh/-open-string sh/-open-string sh/-open-string sh/-open-string sh/-closed-string] [sy/strings sy/strings]]
     ["\"l"      [sh/-open-string sh/-open-string]                                                   [sy/strings]]
     ["\"l\nl\"" [sh/-open-string sh/-open-string sh/-open-string sh/-open-string sh/-closed-string] [sy/strings sy/strings]]]))

(deftest detect-text
  (test-detections
    [["abc"  [sh/-text]                  [sy/texts]]
     ["a1+"  [sh/-text]                  [sy/texts]]
     ["a 1"  [sh/-text sh/-space sh/-number] [sy/texts sy/texts sy/numbers]]]))

(deftest detect-words
  (test-detections
    [["true"   [sh/-word sh/-word sh/-word sh/-word]                   [sy/words]]
     ["false"  [sh/-word sh/-word sh/-word sh/-word sh/-word]          [sy/words]]
     ["nil"    [sh/-word sh/-word sh/-word]                            [sy/words]]
     ["truex"  [sh/-word sh/-word sh/-word sh/-word sh/-word]          [sy/texts]]
     ["falsex" [sh/-word sh/-word sh/-word sh/-word sh/-word sh/-word] [sy/texts]]
     ["nilx"   [sh/-word sh/-word sh/-word sh/-word]                   [sy/texts]]
     ["n:/1"   [sh/-word]                                              [sy/texts]]]))

(deftest detect-commas
  (test-detections
    [[","       [sh/-comma]                                                      [sy/commas]]
     ["a,b"     [sh/-text sh/-comma sh/-text]                                    [sy/texts sy/commas sy/texts]]
     ["1,2"     [sh/-number sh/-comma sh/-number]                                [sy/numbers sy/commas sy/numbers]]
     [":a,1"    [sh/-keyword sh/-comma sh/-number]                               [sy/keywords sy/commas sy/numbers]]
     ["(,)"     [sh/-open-list sh/-comma sh/-closed-list]                        [sy/lists sy/commas sy/lists]]
     ["nil,"    [sh/-word sh/-comma]                                             [sy/words sy/commas]]
     ["\\c,\\a" [sh/-escape-char sh/-char, sh/-comma sh/-escape-char, sh/-char]  [sy/characters sy/characters sy/commas sy/characters sy/characters]]]))

(deftest open-list-transitions
  (test-transitions
    {:node       sh/-open-list
     :allowed    #{sh/-open-list
                   sh/-closed-list
                   sh/-open-vector
                   sh/-closed-vector
                   sh/-open-map
                   sh/-closed-map
                   sh/-open-string
                   sh/-keyword
                   sh/-comment
                   sh/-number
                   sh/-space
                   sh/-break
                   sh/-function
                   sh/-escape-char
                   sh/-comma}
     :disallowed #{sh/-closed-string
                   sh/-word
                   sh/-text
                   sh/-special-char}}))

(deftest closed-list-transitions
  (test-transitions
    {:node       sh/-closed-string
     :allowed    #{sh/-open-list
                   sh/-closed-list
                   sh/-open-vector
                   sh/-closed-vector
                   sh/-open-map
                   sh/-closed-map
                   sh/-open-string
                   sh/-keyword
                   sh/-comment
                   sh/-number
                   sh/-space
                   sh/-break
                   sh/-word
                   sh/-text
                   sh/-escape-char
                   sh/-comma}
     :disallowed #{sh/-closed-string
                   sh/-function
                   sh/-special-char}}))

(deftest open-vector-transitions
  (test-transitions
    {:node       sh/-open-vector
     :allowed    #{sh/-open-list
                   sh/-closed-list
                   sh/-open-vector
                   sh/-closed-vector
                   sh/-open-map
                   sh/-closed-map
                   sh/-open-string
                   sh/-keyword
                   sh/-comment
                   sh/-number
                   sh/-space
                   sh/-break
                   sh/-word
                   sh/-text
                   sh/-escape-char
                   sh/-comma}
     :disallowed #{sh/-closed-string
                   sh/-function
                   sh/-special-char}}))

(deftest close-vector-transitions
  (test-transitions
    {:node       sh/-closed-vector
     :allowed    #{sh/-open-list
                   sh/-closed-list
                   sh/-open-vector
                   sh/-closed-vector
                   sh/-open-map
                   sh/-closed-map
                   sh/-open-string
                   sh/-keyword
                   sh/-comment
                   sh/-number
                   sh/-space
                   sh/-break
                   sh/-word
                   sh/-text
                   sh/-escape-char
                   sh/-comma}
     :disallowed #{sh/-closed-string
                   sh/-function
                   sh/-special-char}}))

(deftest open-map-transitions
  (test-transitions
    {:node       sh/-open-map
     :allowed    #{sh/-open-list
                   sh/-closed-list
                   sh/-open-vector
                   sh/-closed-vector
                   sh/-open-map
                   sh/-closed-map
                   sh/-open-string
                   sh/-keyword
                   sh/-comment
                   sh/-number
                   sh/-space
                   sh/-break
                   sh/-word
                   sh/-text
                   sh/-escape-char
                   sh/-comma}
     :disallowed #{sh/-closed-string
                   sh/-function
                   sh/-special-char}}))

(deftest close-map-transitions
  (test-transitions
    {:node      sh/-closed-map
     :allowed    #{sh/-open-list
                   sh/-closed-list
                   sh/-open-vector
                   sh/-closed-vector
                   sh/-open-map
                   sh/-closed-map
                   sh/-open-string
                   sh/-keyword
                   sh/-comment
                   sh/-number
                   sh/-space
                   sh/-break
                   sh/-word
                   sh/-text
                   sh/-escape-char
                   sh/-comma}
     :disallowed #{sh/-closed-string
                   sh/-function
                   sh/-special-char}}))

(deftest open-string-transitions
  (test-transitions
    {:node      sh/-open-string
     :allowed    #{sh/-closed-string}
     :disallowed #{sh/-open-list
                   sh/-closed-list
                   sh/-open-vector
                   sh/-closed-vector
                   sh/-open-map
                   sh/-closed-map
                   sh/-open-string
                   sh/-keyword
                   sh/-comment
                   sh/-number
                   sh/-space
                   sh/-break
                   sh/-word
                   sh/-text
                   sh/-escape-char
                   sh/-function
                   sh/-comma
                   sh/-special-char}}))

(deftest close-string-transitions
  (test-transitions
    {:node       sh/-closed-string
     :allowed    #{sh/-open-list
                   sh/-closed-list
                   sh/-open-vector
                   sh/-closed-vector
                   sh/-open-map
                   sh/-closed-map
                   sh/-open-string
                   sh/-keyword
                   sh/-comment
                   sh/-number
                   sh/-space
                   sh/-break
                   sh/-word
                   sh/-text
                   sh/-escape-char
                   sh/-comma}
     :disallowed #{sh/-closed-string
                   sh/-function
                   sh/-special-char}}))

(deftest keyword-transitions
  (test-transitions
    {:node       sh/-keyword
     :allowed    #{sh/-open-list
                   sh/-closed-list
                   sh/-open-vector
                   sh/-closed-vector
                   sh/-open-map
                   sh/-closed-map
                   sh/-open-string
                   sh/-keyword
                   sh/-comment
                   sh/-space
                   sh/-break
                   sh/-escape-char
                   sh/-comma}
     :disallowed #{sh/-closed-string
                   sh/-function
                   sh/-word
                   sh/-text
                   sh/-number
                   sh/-special-char}}))

(deftest comment-transitions
  (test-transitions
    {:node       sh/-comment
     :allowed    #{sh/-break
                   sh/-comment}
     :disallowed #{sh/-open-list
                   sh/-closed-list
                   sh/-open-vector
                   sh/-closed-vector
                   sh/-open-map
                   sh/-closed-map
                   sh/-open-string
                   sh/-keyword
                   sh/-space
                   sh/-escape-char
                   sh/-closed-string
                   sh/-function
                   sh/-word
                   sh/-text
                   sh/-number
                   sh/-comma
                   sh/-special-char}}))

(deftest number-transitions
  (test-transitions
    {:node       sh/-number
     :allowed    #{sh/-open-list
                   sh/-closed-list
                   sh/-open-vector
                   sh/-closed-vector
                   sh/-open-map
                   sh/-closed-map
                   sh/-open-string
                   sh/-comment
                   sh/-number
                   sh/-space
                   sh/-break
                   sh/-comma}
     :disallowed #{sh/-closed-string
                   sh/-function
                   sh/-word
                   sh/-text
                   sh/-keyword
                   sh/-escape-char
                   sh/-special-char}}))

(deftest word-transitions
  (test-transitions
    {:node       sh/-word
     :allowed    #{sh/-open-list
                   sh/-closed-list
                   sh/-open-vector
                   sh/-closed-vector
                   sh/-open-map
                   sh/-closed-map
                   sh/-open-string
                   sh/-comment
                   sh/-space
                   sh/-break
                   sh/-word
                   sh/-comma}
     :disallowed #{sh/-closed-string
                   sh/-function
                   sh/-text
                   sh/-keyword
                   sh/-escape-char
                   sh/-number
                   sh/-special-char}}))

(deftest space-transitions
  (test-transitions
    {:node       sh/-space
     :allowed    #{sh/-open-list
                   sh/-closed-list
                   sh/-open-vector
                   sh/-closed-vector
                   sh/-open-map
                   sh/-closed-map
                   sh/-open-string
                   sh/-comment
                   sh/-number
                   sh/-space
                   sh/-break
                   sh/-word
                   sh/-keyword
                   sh/-escape-char
                   sh/-text
                   sh/-comma}
     :disallowed #{sh/-closed-string
                   sh/-function
                   sh/-special-char}}))

(deftest break-transitions
  (test-transitions
    {:node       sh/-break
     :allowed    #{sh/-open-list
                   sh/-closed-list
                   sh/-open-vector
                   sh/-closed-vector
                   sh/-open-map
                   sh/-closed-map
                   sh/-open-string
                   sh/-comment
                   sh/-number
                   sh/-space
                   sh/-break
                   sh/-word
                   sh/-keyword
                   sh/-escape-char
                   sh/-text
                   sh/-comma}
     :disallowed #{sh/-closed-string
                   sh/-function
                   sh/-special-char}}))

(deftest character-transitions
  (test-transitions
    {:node       sh/-escape-char
     :allowed    #{sh/-space
                   sh/-break
                   sh/-special-char}
     :disallowed #{sh/-open-list
                   sh/-closed-list
                   sh/-open-vector
                   sh/-closed-vector
                   sh/-open-map
                   sh/-closed-map
                   sh/-open-string
                   sh/-comment
                   sh/-number
                   sh/-closed-string
                   sh/-function
                   sh/-keyword
                   sh/-word
                   sh/-text
                   sh/-escape-char
                   sh/-comma}}))

(deftest comma-transitions
  (test-transitions
    {:node       sh/-comma
     :allowed    #{sh/-open-list
                   sh/-closed-list
                   sh/-open-vector
                   sh/-closed-vector
                   sh/-open-map
                   sh/-closed-map
                   sh/-open-string
                   sh/-comment
                   sh/-number
                   sh/-keyword
                   sh/-word
                   sh/-text
                   sh/-space
                   sh/-break
                   sh/-escape-char
                   sh/-comma}
     :disallowed #{sh/-function
                   sh/-closed-string
                   sh/-special-char}}))
