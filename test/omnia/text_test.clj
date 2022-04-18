(ns omnia.text-test
  (:require [clojure.test :refer [deftest is testing]]
            [omnia.text.core :as i]
            [omnia.util.debug :refer [time-return]]
            [omnia.util.generator :refer [one many]]
            [omnia.util.collection :refer [run-indexed!]]
            [omnia.test-utils :refer :all]))

;; 0. Reading

(deftest reads-strings
  (->> [{:input  "123"
         :expect (i/seeker [[\1 \2 \3]])}
        {:input  "1\n\n"
         :expect (i/seeker [[\1] []])}
        {:input  "1\n2"
         :expect (i/seeker [[\1] [\2]])}
        {:input  "1\n2\n"
         :expect (i/seeker [[\1] [\2]])}
        {:input  "\n\n"
         :expect (i/seeker [[] []])}]
       (run! (fn [{:keys [input expect]}]
               (is (= expect (i/from-string input)))))))

(deftest reads-strings-with-cursors
  (->> [{:input  "|123"
         :expect (i/seeker [[\1 \2 \3]] [0 0])}
        {:input  "12|3"
         :expect (i/seeker [[\1 \2 \3]] [2 0])}
        {:input  "1\n|\n"
         :expect (i/seeker [[\1] []] [0 1])}
        {:input  "1\n12\n\n3|2\n4"
         :expect (i/seeker [[\1] [\1 \2] [] [\3 \2] [\4]] [1 3])}
        {:input  "1\n2|"
         :expect (i/seeker [[\1] [\2]] [1 1])}
        {:input  "1|\n2\n"
         :expect (i/seeker [[\1] [\2]] [1 0])}]
       (run! (fn [{:keys [input expect]}]
               (is (= expect (i/from-cursored-string input)))))))

(deftest reads-marked-texts
  (->> [{:input  ["123"]
         :expect {:lines     [[\1 \2 \3]]
                  :cursor    [0 0]
                  :selection nil}}
        {:input  ["1" "2|"]
         :expect {:lines     [[\1] [\2]]
                  :cursor    [1 1]
                  :selection nil}}

        {:input  ["1|" "2"]
         :expect {:lines     [[\1] [\2]]
                  :cursor    [1 0]
                  :selection nil}}

        {:input  ["123" ""]
         :expect {:lines     [[\1 \2 \3] []]
                  :cursor    [0 0]
                  :selection nil}}

        {:input  ["12" "" "3|2" "4"]
         :expect {:lines     [[\1 \2] [] [\3 \2] [\4]]
                  :cursor    [1 2]
                  :selection nil}}

        {:input  ["1<2>|3"]
         :expect {:lines     [[\1 \2 \3]]
                  :cursor    [2 0]
                  :selection {:start [1 0] :end [2 0]}}}

        {:input  ["1<23" ">|45"]
         :expect {:lines     [[\1 \2 \3] [\4 \5]]
                  :cursor    [0 1]
                  :selection {:start [1 0] :end [0 1]}}}

        {:input  ["123<" ">|45"]
         :expect {:lines     [[\1 \2 \3] [\4 \5]]
                  :cursor    [0 1]
                  :selection {:start [3 0] :end [0 1]}}}

        {:input  ["1<23" ">|"]
         :expect {:lines     [[\1 \2 \3] []]
                  :cursor    [0 1]
                  :selection {:start [1 0] :end [0 1]}}}]
       (run! (fn [{:keys [input expect]}]
               (let [text (i/from-marked-text input)]
                 (is (= (:lines text) (:lines expect)))
                 (is (= (:cursor text) (:cursor expect)))
                 (is (= (:selection text) (:selection expect))))))))

;; I. Peering

(deftest peering-keeps-current-line-on-right-hand-side
  (let [text (-> "hello\nw|orld\ntoday"
                 (i/from-cursored-string)
                 (i/peer (fn [left [current & rest]] [current]))
                 (:lines))]
    (is (= text [[\w \o \r \l \d]]))))

(deftest peering-at-start-leaves-left-hand-side-empty
  (let [text (-> "|hello\nworld"
                 (i/from-cursored-string)
                 (i/peer (fn [left right] right))
                 (:lines))]
    (is (= text [[\h \e \l \l \o] [\w \o \r \l \d]]))))

(deftest peering-at-end-keeps-only-last-line-on-right
  (let [text (-> "hello\nworld\ntoday|"
                 (i/from-cursored-string)
                 (i/peer (fn [left right] left))
                 (:lines))]
    (is (= text [[\h \e \l \l \o] [\w \o \r \l \d]]))))

(deftest peering-allows-inserting-new-lines
  (let [lines (-> "new\nlines" (i/from-string) (:lines))
        text (-> "hello\nworld\nto|day"
                 (i/from-cursored-string)
                 (i/peer (fn [left right]
                           (concat left lines right)))
                 (:lines))]
    (is (= text [[\h \e \l \l \o]
                 [\w \o \r \l \d]
                 [\n \e \w]
                 [\l \i \n \e \s]
                 [\t \o \d \a \y]]))))

(deftest peering-allows-removing-lines
  (let [current  (-> "hello\nw|orld\ntoday"
                     (i/from-cursored-string)
                     (i/peer (fn [left [current & right]]
                               (concat left right)))
                     (:lines))
        previous (-> "hello\nw|orld\ntoday"
                     (i/from-cursored-string)
                     (i/peer (fn [left right] right))
                     (:lines))
        next     (-> "hello\nw|orld\ntoday"
                     (i/from-cursored-string)
                     (i/peer (fn [left [current & right]]
                               (concat left [current])))
                     (:lines))]
    (is (= current [[\h \e \l \l \o] [\t \o \d \a \y]]))
    (is (= previous [[\w \o \r \l \d] [\t \o \d \a \y]]))
    (is (= next [[\h \e \l \l \o] [\w \o \r \l \d]]))))

(deftest peering-merges-lines-when-empty
  (let [lines (-> "new\nlines" (i/from-string))
        text  (-> i/empty-seeker
                  (i/peer (fn [l r] (concat l (:lines lines) r)))
                  (:lines))]
    (is (= text [[\n \e \w] [\l \i \n \e \s]]))))

;;; II. Splitting

(deftest splitting-splits-line-into-future-and-past-characters
  (let [left  (-> "hello\nwo|rld"
                  (i/from-cursored-string)
                  (i/split (fn [left right] [left]))
                  (i/current-line))
        right (-> "hello\nwo|rld"
                  (i/from-cursored-string)
                  (i/split (fn [left right] [right]))
                  (i/current-line))]
    (is (= left  [\w \o]))
    (is (= right [\r \l \d]))))

(deftest splitting-can-create-new-lines
  (let [text (-> "hello\nwor|ld"
                 (i/from-cursored-string)
                 (i/split (fn [l r] [(concat l r) [\t \o \d \a \y]]))
                 (:lines))]
    (is (= text [[\h \e \l \l \o] [\w \o \r \l \d] [\t \o \d \a \y]]))))

(deftest splitting-can-alter-the-current-line
  (let [replaced (-> "hell|o\nworld"
                     (i/from-cursored-string)
                     (i/split (constantly [[\n \o]]))
                     (:lines))
        enhanced (-> "hel|lo\nworld"
                     (i/from-cursored-string)
                     (i/split (fn [l r] [(concat l [\p])]))
                     (:lines))]
    (is (= replaced [[\n \o] [\w \o \r \l \d]]))
    (is (= enhanced [[\h \e \l \p] [\w \o \r \l \d]]))))

(deftest splitting-provides-empty-lines-when-empty
  (let [text (-> i/empty-seeker
                 (i/split (fn [l r] [(concat l [\y \e \s] r)]))
                 (:lines))]
    (is (= text [[\y \e \s]]))))

;;; III. Slicing

(deftest slicing-slices-the-line-into-future-and-past-characters
  (let [left  (-> "hello\nwo|rld"
                  (i/from-cursored-string)
                  (i/slice (fn [left right] left))
                  (i/current-line))
        right (-> "hello\nwo|rld"
                  (i/from-cursored-string)
                  (i/slice (fn [left right] right))
                  (i/current-line))]
    (is (= left  [\w \o]))
    (is (= right [\r \l \d]))))

(deftest slicing-can-alter-the-current-line
  (let [replaced (-> "hell|o\nworld"
                     (i/from-cursored-string)
                     (i/slice (constantly [\n \o]))
                     (:lines))
        enhanced (-> "hel|lo\nworld"
                     (i/from-cursored-string)
                     (i/slice (fn [l r] (concat l [\p])))
                     (:lines))]
    (is (= replaced [[\n \o] [\w \o \r \l \d]]))
    (is (= enhanced [[\h \e \l \p] [\w \o \r \l \d]]))))

(deftest slicing-provides-empty-lines-when-empty
  (let [text (-> i/empty-seeker
                 (i/slice (fn [l r] (concat l [\y \e \s] r)))
                 (:lines))]
    (is (= text [[\y \e \s]]))))

;; IV. Moving

(deftest moves-right
  (let [text (-> "h|ello"
                 (i/from-cursored-string)
                 (i/move-right)
                 (i/current-char))]
    (is (= text \l))))

(deftest moves-right-between-lines
  (let [text (-> "hell|o\nworld"
                 (i/from-cursored-string)
                 (i/move-right)
                 (i/move-right)
                 (i/current-char))]
    (is (= text \w))))

(deftest stops-moving-right-at-text-end
  (let [text          (-> "hello\nworld|"
                          (i/from-cursored-string)
                          (i/move-right)
                          (i/move-right)
                          (i/move-right))
        current-char  (i/current-char text)
        previous-char (i/previous-char text)]
    (is (= current-char nil))
    (is (= previous-char \d))))

(deftest moves-left
  (let [text (-> "h|ello"
                 (i/from-cursored-string)
                 (i/move-left)
                 (i/current-char))]
    (is (= text \h))))

(deftest moves-left-between-lines
  (let [text          (-> "hello\nw|orld"
                          (i/from-cursored-string)
                          (i/move-left)
                          (i/move-left))
        current-char  (i/current-char text)
        previous-char (i/previous-char text)]
    (is (= current-char nil))
    (is (= previous-char \o))))

(deftest stops-moving-left-at-text-start
  (let [text          (-> "|hello\nworld"
                          (i/from-cursored-string)
                          (i/move-left)
                          (i/move-left)
                          (i/move-left))
        current-char  (i/current-char text)
        previous-char (i/previous-char text)]
    (is (= current-char \h))
    (is (= previous-char nil))))

(deftest moves-up
  (testing "Moves up from middle of the line, when line above is smaller"
    (let [text         (i/from-cursored-string "ha\nwor|ld")
          moved        (process' text [up])
          cursor       (:cursor moved)
          current-char (i/current-char moved)]
      (is (= current-char nil))
      (is (= cursor [2 0]))))

  (testing "Moves up from middle of the line, when line above >= in size"
    (let [text         (i/from-cursored-string "hello\nwor|ld")
          moved        (process' text [up])
          cursor       (:cursor moved)
          current-char (i/current-char moved)]
      (is (= current-char \l))
      (is (= cursor [3 0])))))

(deftest moves-up-from-the-end-of-line
  (let [text          (-> "hello\nworld|"
                          (i/from-cursored-string)
                          (i/move-up))
        current-char  (i/current-char text)
        previous-char (i/previous-char text)]
    (is (= current-char nil))
    (is (= previous-char \o))))

(deftest moves-up-from-the-start-of-line
  (let [text          (-> "hello\n|world"
                          (i/from-cursored-string)
                          (i/move-up))
        current-char  (i/current-char text)
        previous-char (i/previous-char text)]
    (is (= current-char \h))
    (is (= previous-char nil))))

(deftest moves-up-to-line-end-when-previous-line-smaller
  (let [text          (-> "you\nhello|"
                          (i/from-cursored-string)
                          (i/move-up))
        current-char  (i/current-char text)
        previous-char (i/previous-char text)]
    (is (= current-char nil))
    (is (= previous-char \u))))

(deftest stops-moving-up-when-text-ends
  (let [text          (-> "he|llo\nworld"
                          (i/from-cursored-string)
                          (i/move-up)
                          (i/move-up)
                          (i/move-up))
        current-char  (i/current-char text)
        previous-char (i/previous-char text)]
    (is (= current-char \l))
    (is (= previous-char \e))))

(deftest moves-down
  (let [current-char (-> "hell|o\nworld"
                         (i/from-cursored-string)
                         (i/move-down)
                         (i/current-char))]
    (is (= current-char \d))))

(deftest moves-down-from-the-end-of-line
  (let [text          (-> "hello|\nworld"
                          (i/from-cursored-string)
                          (i/move-down))
        current-char  (i/current-char text)
        previous-char (i/previous-char text)]
    (is (= current-char nil))
    (is (= previous-char \d))))

(deftest moves-down-from-the-start-of-line
  (let [text          (-> "|hello\nworld"
                          (i/from-cursored-string)
                          (i/move-down))
        current-char  (i/current-char text)
        previous-char (i/previous-char text)]
    (is (= current-char \w))
    (is (= previous-char nil))))

(deftest moves-down-to-line-end-when-next-line-smaller
  (let [text          (-> "hello|\nyou"
                          (i/from-cursored-string)
                          (i/move-down))
        current-char  (i/current-char text)
        previous-char (i/previous-char text)]
    (is (= current-char nil))
    (is (= previous-char \u))))

(deftest stops-moving-down-when-text-ends
  (let [text          (-> "hello\nwor|ld"
                          (i/from-cursored-string)
                          (i/move-down)
                          (i/move-down)
                          (i/move-down))
        current-char  (i/current-char text)
        previous-char (i/previous-char text)]
    (is (= current-char \l))
    (is (= previous-char \r))))

;; V. Retrieving

(deftest gets-previous-char
  (let [somewhere    (-> "hell|o" (i/from-cursored-string) (i/previous-char))
        at-start     (-> "|hello" (i/from-cursored-string) (i/previous-char))
        between-line (-> "hello\n|world" (i/from-cursored-string) (i/previous-char))
        at-end       (-> "hello|" (i/from-cursored-string) (i/previous-char))]
    (is (= somewhere \l))
    (is (= at-start nil))
    (is (= between-line nil))
    (is (= at-end \o))))

(deftest gets-current-char
  (let [somewhere    (-> "hell|o" (i/from-cursored-string) (i/current-char))
        at-start     (-> "|hello" (i/from-cursored-string) (i/current-char))
        between-line (-> "hello|\nworld" (i/from-cursored-string) (i/current-char))
        at-end       (-> "hello|" (i/from-cursored-string) (i/current-char))]
    (is (= somewhere \o))
    (is (= at-start \h))
    (is (= between-line nil))
    (is (= at-end nil))))

(deftest gets-next-char
  (let [somewhere    (-> "hel|lo" (i/from-cursored-string) (i/next-char))
        at-start     (-> "|hello" (i/from-cursored-string) (i/next-char))
        between-line (-> "hello|\nworld" (i/from-cursored-string) (i/next-char))
        at-end       (-> "hello|" (i/from-cursored-string) (i/next-char))]
    (is (= somewhere \o))
    (is (= at-start \e))
    (is (= between-line \w))
    (is (= at-end nil))))

;; VI. Deleting

(deftest deletes-previous-character
  (testing "Deletes previous character within a line"
    (let [text   (-> "hell|o"
                     (i/from-cursored-string)
                     (process' [delete-previous]))
          lines  (:lines text)
          cursor (:cursor text)]
      (is (= lines [[\h \e \l \o]]))
      (is (= cursor [3 0]))))

  (testing "Deletes previous characters over multiple lines"
    (let [text   (-> "hello\nw|orld"
                     (i/from-cursored-string)
                     (process' [delete-previous delete-previous delete-previous]))
          lines  (:lines text)
          cursor (:cursor text)]
      (is (= lines [[\h \e \l \l \o \r \l \d]]))
      (is (= cursor [4 0]))))

  (testing "Stops deleting previous character when a bound is reached"
    (let [text   (-> "|hello"
                     (i/from-cursored-string)
                     (process' [delete-previous]))
          lines  (:lines text)
          cursor (:cursor text)]
      (is (= lines [[\h \e \l \l \o]]))
      (is (= cursor [0 0])))))

(deftest deletes-current-character
  (testing "Deletes current character within a line"
    (let [text   (-> "hell|o"
                     (i/from-cursored-string)
                     (process' [delete-current]))
          lines  (:lines text)
          cursor (:cursor text)]
      (is (= lines [[\h \e \l \l]]))
      (is (= cursor [4 0]))))

  (testing "Deletes current characters over multiple lines"
    (let [text   (-> "hell|o\nworld"
                     (i/from-cursored-string)
                     (process' [delete-current delete-current delete-current]))
          lines  (:lines text)
          cursor (:cursor text)]
      (is (= lines [[\h \e \l \l \o \r \l \d]]))
      (is (= cursor [4 0]))))

  (testing "Stops deleting current character when a bound is reached"
    (let [text   (-> "hello|"
                     (i/from-cursored-string)
                     (process' [delete-current]))
          lines  (:lines text)
          cursor (:cursor text)]
      (is (= lines [[\h \e \l \l \o]]))
      (is (= cursor [5 0])))))

(deftest deletes-pairs
  (testing "Deletes pairs when next to each other"
    (->> [[\( \)]
          [\[ \]]
          [\{ \}]
          [\" \"]]
         (run! (fn [[l r]]
                 (let [text            (-> (str l "|" r) (i/from-cursored-string))
                       previous        (-> text (process' [delete-previous]))
                       previous-lines  (:lines previous)
                       previous-cursor (:cursor previous)
                       current         (-> text (process' [delete-current]))
                       current-lines   (:lines current)
                       current-cursor  (:cursor current)]
                   (is (= previous-lines current-lines [[]]))
                   (is (= previous-cursor current-cursor [0 0])))))))

  (testing "Does not delete orphaned pairs"
    (->> [\( \) \{ \} \[ \] \"]
         (run! (fn [orphan]
                 (let [previous        (-> (str orphan "|")
                                           (i/from-cursored-string)
                                           (process' [delete-previous]))
                       current         (-> (str "|" orphan)
                                           (i/from-cursored-string)
                                           (process' [delete-current]))
                       previous-lines  (:lines previous)
                       previous-cursor (:cursor previous)
                       current-lines   (:lines current)
                       current-cursor  (:cursor current)]
                   (is (= previous-lines current-lines [[orphan]]))
                   (is (= previous-cursor current-cursor [0 0]))))))))

(deftest deletes-selections
  (testing "Deletes selection by means of deleting previous character"
    (let [text   (-> "he|llo"
                     (i/from-cursored-string)
                     (process' [select-right select-right delete-previous]))
          cursor (:cursor text)
          lines  (:lines text)]
      (is (= lines [[\h \e \o]]))
      (is (= cursor [2 0]))))

  (testing "Deletes selection by means of deleting the current character"
    (let [text   (-> "he|llo"
                     (i/from-cursored-string)
                     (process' [select-right select-right delete-current]))
          cursor (:cursor text)
          lines  (:lines text)]
      (is (= lines [[\h \e \o]]))
      (is (= cursor [2 0])))))

(deftest deletes-selections-over-lines
  (testing "Deletes selections merging lines downward"
    (let [text     (i/from-cursored-string "hello\nworld|\nyou")
          selected (process' text [select-up delete-previous])
          cursor   (:cursor selected)
          lines    (:lines selected)]
      (is (= lines [[\h \e \l \l \o] [\y \o \u]]))
      (is (= cursor [5 0]))))

  (testing "Deletes selections merging lines upward"
    (let [text     (i/from-cursored-string "hello\n|world\nyou")
          selected (process' text [select-down delete-previous])
          cursor   (:cursor selected)
          lines    (:lines selected)]
      (is (= lines [[\h \e \l \l \o] [\y \o \u]]))
      (is (= cursor [0 1])))))

(deftest deletes-selected-pairs
  (->> [[\( \)]
        [\[ \]]
        [\{ \}]
        [\" \"]]
       (run! (fn [[l r]]
               (let [text           (-> [(str l "|" r)] (i/from-marked-text))
                     previous-left  (-> text (process' [select-left delete-previous]) (:lines))
                     current-left   (-> text (process' [select-left delete-current]) (:lines))
                     previous-right (-> text (process' [select-right delete-previous]) (:lines))
                     current-right  (-> text (process' [select-right delete-current]) (:lines))]
                 (is (= previous-left current-left [[r]]))
                 (is (= previous-right current-right [[l]])))))))

(deftest deletes-selections-over-multiple-lines-and-merges
  (let [text     (-> "hello\nworld\nto|day" (i/from-cursored-string) (i/select-up))
        previous (-> text (i/delete-previous) (:lines))
        current  (-> text (i/delete-current) (:lines))]
    (is (= previous current [[\h \e \l \l \o] [\w \o \d \a \y]]))))

;; VII. Inserting

(deftest inserts-literals
  (let [text (i/from-cursored-string "|some text")
        num  (-> text (i/insert \1) (i/previous-char))
        char (-> text (i/insert \a) (i/previous-char))]
    (is (= num \1))
    (is (= char \a))))

(deftest inserts-pairs
  (run!
    (fn [[l r]]
      (let [left-pair (-> i/empty-seeker (i/insert l) (i/current-line))
            right-pair (-> i/empty-seeker (i/insert r) (i/current-line))]
        (is (= left-pair right-pair [l r]))))
    [[\( \)] [\[ \]] [\{ \}] [\" \"]]))

(deftest inserts-ignoring-existing-neighouring-closed-parens
  (run!
    (fn [[l r]]
      (let [text (-> (str l "text" "|" r)
                     (i/from-cursored-string)
                     (i/insert r)
                     (i/current-line))]
        (is (= text [l \t \e \x \t r]))))
    [[\( \)] [\[ \]] [\{ \}]]))

(deftest inserts-creating-new-open-parens-when-neighouring-open-parens
  (run!
    (fn [[l r]]
      (let [text (-> (str "|" l "text" r)
                     (i/from-cursored-string)
                     (i/insert l)
                     (i/current-line))]
        (is (= text [l r l \t \e \x \t r]))))
    [[\( \)] [\[ \]] [\{ \}]]))

(deftest inserts-ignoring-existing-existing-neighbouring-string-pair
  (let [left-hand  (-> "|\"text\""
                       (i/from-cursored-string)
                       (i/insert \")
                       (i/current-line))
        right-hand (-> "\"text|\""
                       (i/from-cursored-string)
                       (i/insert \")
                       (i/current-line))]
    (is (= left-hand right-hand [\" \t \e \x \t \"]))))

(deftest inserts-replacing-selection-in-line
  (let [text (-> "|one line"
                 (i/from-cursored-string)
                 (i/select-jump-right)
                 (i/insert \a)
                 (i/current-line))]
    (is (= text [\a \space \l \i \n \e]))))

(deftest inserts-replacing-selection-between-lines
  (let [text (-> "one |line\ntwo lines"
                 (i/from-cursored-string)
                 (i/select-jump-right)
                 (i/select-jump-right)
                 (i/select-jump-right)
                 (i/insert \a)
                 (:lines))]
    (is (= text [[\o \n \e \space \a \space \l \i \n \e \s]]))))

;; VIII. Jumping

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

;; IX. Selecting
(deftest selects-indempotently-over-single-chars
  (let [region (-> ["|12"]
                   (i/from-marked-text)
                   (process' [right select-left select-left select-right])
                   (:selection))]
    (is (nil? region))))

(deftest selects-when-jumping-right
  (testing "Selects jumping right within a line"
    (let [text     (i/from-cursored-string "hello |world today")
          selected (process' text [jump-select-right jump-select-right jump-select-right])
          cursor   (:cursor selected)
          lines    (-> selected (i/extract) (:lines))]
      (is (= lines [[\w \o \r \l \d \space \t \o \d \a \y]]))
      (is (= cursor [17 0]))))

  (testing "Selects jumping right over lines until the beginning of the next line"
    (let [text     (i/from-cursored-string "hello |world\nsome day")
          selected (process' text [jump-select-right jump-select-right])
          cursor   (:cursor selected)
          lines    (-> selected (i/extract) (:lines))]
      (is (= lines [[\w \o \r \l \d] []]))
      (is (= cursor [0 1]))))

  (testing "Selects jumping right over lines until some text of the next line is covered"
    (let [text     (i/from-cursored-string "hello |world\nsome day")
          selected (process' text [jump-select-right jump-select-right jump-select-right])
          cursor   (:cursor selected)
          lines    (-> selected (i/extract) (:lines))]
      (is (= lines [[\w \o \r \l \d] [\s \o \m \e]]))
      (is (= cursor [4 1])))))

(deftest selects-when-jumping-left
  (testing "Selects jumping left within a line"
    (let [text     (i/from-cursored-string "hello world| today")
          selected (process' text [jump-select-left jump-select-left jump-select-left])
          cursor   (:cursor selected)
          lines    (-> selected (i/extract) (:lines))]
      (is (= lines [[\h \e \l \l \o \space \w \o \r \l \d]]))
      (is (= cursor [0 0]))))

  (testing "Selects jumping left over lines until end of previous line"
    (let [text     (i/from-cursored-string "hello world\nsome| day")
          selected (process' text [jump-select-left jump-select-left])
          cursor   (:cursor selected)
          lines    (-> selected (i/extract) (:lines))]
      (is (= lines [[] [\s \o \m \e]]))
      (is (= cursor [11 0]))))

  (testing "Selects jumping left over lines until some text of the previous line is covered"
    (let [text     (i/from-cursored-string "hello world\nsome| day")
          selected (process' text [jump-select-left jump-select-left jump-select-left])
          cursor   (:cursor selected)
          lines    (-> selected (i/extract) (:lines))]
      (is (= lines [[\w \o \r \l \d] [\s \o \m \e]]))
      (is (= cursor [6 0])))))

(deftest selects-up
  (testing "LINE START -"
    (testing "INCREASING -"
      (testing "initial"
        (let [actual    (-> ["hello"
                             "|world"]
                            (i/from-marked-text)
                            (process' [select-up]))
              expected  (-> ["|<hello"
                             ">world"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[\h \e \l \l \o] []]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected)))))

      (testing "continuously"
        (let [actual    (-> ["hello"
                             "world"
                             "|today"]
                            (i/from-marked-text)
                            (process' [select-up select-up]))
              expected  (-> ["|<hello"
                             "world"
                             ">today"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[\h \e \l \l \o] [\w \o \r \l \d] []]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected))))))

    (testing "DECREASING -"
      (testing "initial"
        (let [actual    (-> ["<hello"
                             "world"
                             ">|day"]
                            (i/from-marked-text)
                            (process' [select-up]))
              expected  (-> ["<hello"
                             ">|world"
                             "day"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[\h \e \l \l \o] []]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected)))))

      (testing "continuously"
        (let [actual    (-> ["<hello"
                             "world"
                             "today"
                             ">|you"]
                            (i/from-marked-text)
                            (process' [select-up select-up]))
              expected  (-> ["<hello"
                             ">|world"
                             "today"
                             "you"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[\h \e \l \l \o] []]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected))))))

    (testing "DEACTIVATION -"
      (let [actual    (-> ["<hello"
                           ">|world"]
                          (i/from-marked-text)
                          (process' [select-up select-up]))
            expected  (-> ["|hello"
                           "world"]
                          (i/from-marked-text))
            extracted (-> actual (i/extract) (:lines))]
        (is (= extracted nil))
        (is (= (:cursor actual) (:cursor expected)))
        (is (= (:selection actual) (:selection expected))))))

  (testing "LINE MIDDLE -"
    (testing "INCREASING -"
      (testing "initial"
        (let [actual    (-> ["hello"
                             "wo|rld"]
                            (i/from-marked-text)
                            (process' [select-up]))
              expected (-> ["he|<llo"
                            "wo>rld"]
                           (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[\l \l \o] [\w \o]]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected)))))

      (testing "continuously"
        (let [actual    (-> ["hello"
                             "world"
                             "to|day"]
                            (i/from-marked-text)
                            (process' [select-up select-up]))
              expected (-> ["he|<llo"
                            "world"
                            "to>day"]
                           (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[\l \l \o] [\w \o \r \l \d] [\t \o]]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected)))))

      (testing "after decreasing"
        (let [actual    (-> ["hel<lo"
                             "world"
                             "d>|ay"]
                            (i/from-marked-text)
                            (process' [select-up select-up]))
              expected (-> ["h|<el>lo"
                            "world"
                            "day"]
                           (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[\e \l]]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected))))))

    (testing "DECREASING -"
      (testing "initial"
        (let [actual    (-> ["<hello"
                             "world"
                             "da>|y"]
                            (i/from-marked-text)
                            (process' [select-up]))
              expected (-> ["<hello"
                            "wo>|rld"
                            "day"]
                           (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[\h \e \l \l \o] [\w \o]]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected))))))

    (testing "continuously"
      (let [actual    (-> ["<hello"
                           "world"
                           "to>|day"]
                          (i/from-marked-text)
                          (process' [select-up select-up]))
            expected (-> ["<he>|llo"
                          "world"
                          "today"]
                         (i/from-marked-text))
            extracted (-> actual (i/extract) (:lines))]
        (is (= extracted [[\h \e]]))
        (is (= (:cursor actual) (:cursor expected)))
        (is (= (:selection actual) (:selection expected)))))

    (testing "DEACTIVATION -"
      (let [actual    (-> ["hi"
                           "so<me"
                           "da>|y"]
                          (i/from-marked-text)
                          (process' [select-up]))
            expected  (-> ["hi"
                           "so|me"
                           "day"]
                          (i/from-marked-text))
            extracted (-> actual (i/extract) (:lines))]
        (is (= extracted nil))
        (is (= (:cursor actual) (:cursor expected)))
        (is (= (:selection actual) (:selection expected))))))

  (testing "LINE MIDDLE, where LINE ABOVE SMALLER -"
    (testing "INCREASING -"
      (testing "initial"
        (let [actual    (-> ["hi"
                             "som|e"]
                            (i/from-marked-text)
                            (process' [select-up]))
              expected  (-> ["hi|<"
                             "som>e"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[] [\s \o \m]]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected)))))

      (testing "continuously"
        (let [actual    (-> ["hi"
                             "some"
                             "thursd|ay"]
                            (i/from-marked-text)
                            (process' [select-up select-up]))
              expected  (-> ["hi|<"
                             "some"
                             "thursd>ay"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[] [\s \o \m \e] [\t \h \u \r \s \d]]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected))))))

    (testing "DECREASING -"
      (testing "initial"
        (let [actual    (-> ["<hi"
                             "som>|e"]
                            (i/from-marked-text)
                            (process' [select-up]))
              expected  (-> ["<hi>|"
                             "some"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[\h \i]]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected)))))

      (testing "continuously"
        (let [actual    (-> ["<hello"
                             "hi"
                             "som>|e"]
                            (i/from-marked-text)
                            (process' [select-up select-up]))
              expected  (-> ["<he>|llo"
                             "hi"
                             "some"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[\h \e]]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected))))))

    (testing "DEACTIVATION -"
      (let [actual    (-> ["hi<"
                           "som>|e"]
                          (i/from-marked-text)
                          (process' [select-up]))
            expected  (-> ["hi|"
                           "some"]
                          (i/from-marked-text))
            extracted (-> actual (i/extract) (:lines))]
        (is (= extracted nil))
        (is (= (:cursor actual) (:cursor expected)))
        (is (= (:selection actual) (:selection expected))))))

  (testing "LINE END -"
    (testing "INCREASING -"
      (testing "initial"
        (let [actual    (-> ["heyy"
                             "some|"]
                            (i/from-marked-text)
                            (process' [select-up]))
              expected  (-> ["heyy|<"
                             "some>"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[] [\s \o \m \e]]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected)))))

      (testing "continuously"
        (let [actual    (-> ["heyy"
                             "some"
                             "days|"]
                            (i/from-marked-text)
                            (process' [select-up select-up]))
              expected  (-> ["heyy|<"
                             "some"
                             "days>"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[] [\s \o \m \e] [\d \a \y \s]]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected))))))

    (testing "DECREASING -"
      (testing "initial"
        (let [actual    (-> ["<hello"
                             "world>|"]
                            (i/from-marked-text)
                            (process' [select-up]))
              expected  (-> ["<hello>|"
                             "world"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[\h \e \l \l \o]]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected)))))

      (testing "continuously"
        (let [actual    (-> ["<hello"
                             "world"
                             "today>|"]
                            (i/from-marked-text)
                            (process' [select-up select-up]))
              expected  (-> ["<hello>|"
                             "world"
                             "today"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[\h \e \l \l \o]]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected))))))

    (testing "DEACTIVATION -"
      (let [actual    (-> ["hi<"
                           "some"
                           "today>|"]
                          (i/from-marked-text)
                          (process' [select-up select-up select-up]))
            expected  (-> ["hi|"
                           "some"
                           "today"]
                          (i/from-marked-text))
            extracted (-> actual (i/extract) (:lines))]
        (is (= extracted nil))
        (is (= (:cursor actual) (:cursor expected)))
        (is (= (:selection actual) (:selection expected))))))

  (testing "LINE END, where LINE ABOVE SMALLER -"
    (testing "INCREASING -"
      (testing "initial"
        (let [actual    (-> ["hey"
                             "some|"]
                            (i/from-marked-text)
                            (process' [select-up]))
              expected  (-> ["hey|<"
                             "some>"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[] [\s \o \m \e]]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected)))))

      (testing "continuously"
        (let [actual    (-> ["hi"
                             "some"
                             "today|"]
                            (i/from-marked-text)
                            (process' [select-up select-up]))
              expected  (-> ["hi|<"
                             "some"
                             "today>"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[] [\s \o \m \e] [\t \o \d \a \y]]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected))))))

    (testing "DECREASING -"
      (testing "initial"
        (let [actual    (-> ["<hey"
                             "some>|"]
                            (i/from-marked-text)
                            (process' [select-up]))
              expected  (-> ["<hey>|"
                             "some"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[\h \e \y]]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected)))))

      (testing "continuously"
        (let [actual    (-> ["<hi"
                             "some"
                             "today>|"]
                            (i/from-marked-text)
                            (process' [select-up select-up]))
              expected  (-> ["<hi>|"
                             "some"
                             "today"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[\h \i]]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected))))))

    (testing "DEACTIVATION"
      (let [actual    (-> ["hi<"
                           "today>|"]
                          (i/from-marked-text)
                          (process' [select-up select-up]))
            expected  (-> ["hi|"
                           "today"]
                          (i/from-marked-text))
            extracted (-> actual (i/extract) (:lines))]
        (is (= extracted nil))
        (is (= (:cursor actual) (:cursor expected)))
        (is (= (:selection actual) (:selection expected))))))

  (testing "LINE END, where LINE ABOVE LARGER -"
    (testing "INCREASING -"
      (testing "initial"
        (let [actual    (-> ["bonjour"
                             "world|"]
                            (i/from-marked-text)
                            (process' [select-up select-up]))
              expected  (-> ["bonjo|<ur"
                             "world>"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[\u \r] [\w \o \r \l \d]]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected)))))

      (testing "continuously"
        (let [actual    (-> ["fantastic"
                             "bonjour"
                             "world|"]
                            (i/from-marked-text)
                            (process' [select-up select-up]))
              expected  (-> ["fanta|<stic"
                             "bonjour"
                             "world>"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[\s \t \i \c] [\b \o \n \j \o \u \r] [\w \o \r \l \d]]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected)))))

      (testing "after decrease"
        (let [actual    (-> ["fantasti<c"
                             "bonjour"
                             "world>|"]
                            (i/from-marked-text)
                            (process' [select-up select-up]))
              expected  (-> ["fanta|<sti>c"
                             "bonjour"
                             "world"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[\s \t \i]]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected))))))

    (testing "DECREASING -"
      (testing "initial"
        (let [actual    (-> ["<bonjour"
                             "world>|"]
                            (i/from-marked-text)
                            (process' [select-up]))
              expected  (-> ["<bonjo>|ur"
                             "world"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[\b \o \n \j \o]]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected)))))

      (testing "continuously"
        (let [actual    (-> ["<fantastic"
                             "bonjour"
                             "world>|"]
                            (i/from-marked-text)
                            (process' [select-up select-up]))
              expected  (-> ["<fanta>|stic"
                             "bonjour"
                             "world"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[\f \a \n \t \a]]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected))))))))

(deftest selects-down
  (testing "LINE START -"
    (testing "INCREASE -"
      (testing "initial"
        (let [actual    (-> ["|hello"
                             "world"]
                            (i/from-marked-text)
                            (process' [select-down]))
              expected  (-> ["<hello"
                             ">|world"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[\h \e \l \l \o] []]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected)))))

      (testing "continuous"
        (let [actual    (-> ["|hello"
                             "world"
                             "you"]
                            (i/from-marked-text)
                            (process' [select-down select-down]))
              expected  (-> ["<hello"
                             "world"
                             ">|you"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[\h \e \l \l \o] [\w \o \r \l \d] []]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected))))))

    (testing "DECREASE -"
      (testing "initial"
        (let [actual    (-> ["|<hello"
                             "world>"]
                            (i/from-marked-text)
                            (process' [select-down]))
              expected  (-> ["hello"
                             "|<world>"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[\w \o \r \l \d]]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected)))))

      (testing "continuous"
        (let [actual    (-> ["|<hello"
                             "some"
                             "day>"]
                            (i/from-marked-text)
                            (process' [select-down select-down]))
              expected  (-> ["hello"
                             "some"
                             "|<day>"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[\d \a \y]]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected))))))

    (testing "DEACTIVATION -"
      (let [actual    (-> ["|<hello"
                           ">some"]
                          (i/from-marked-text)
                          (process' [select-down]))
            expected  (-> ["hello"
                           "|some"]
                          (i/from-marked-text))
            extracted (-> actual (i/extract) (:lines))]
        (is (= extracted nil))
        (is (= (:cursor actual) (:cursor expected)))
        (is (= (:selection actual) (:selection expected))))))

  (testing "LINE MIDDLE -"
    (testing "INCREASE -"
      (testing "initial"
        (let [actual    (-> ["hel|lo"
                             "world"]
                            (i/from-marked-text)
                            (process' [select-down]))
              expected  (-> ["hel<lo"
                             "wor>|ld"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[\l \o] [\w \o \r]]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected)))))

      (testing "continuous"
        (let [actual    (-> ["hel|lo"
                             "world"
                             "someday"]
                            (i/from-marked-text)
                            (process' [select-down select-down]))
              expected  (-> ["hel<lo"
                             "world"
                             "som>|eday"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[\l \o] [\w \o \r \l \d] [\s \o \m]]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected)))))

      (testing "after decrease"
        (let [actual    (-> ["hel|<lo"
                             "some"
                             "s>omeday"]
                            (i/from-marked-text)
                            (process' [select-down select-down]))
              expected  (-> ["hello"
                             "some"
                             "s<om>|eday"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[\o \m]]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected))))))

    (testing "DECREASE -"
      (testing "initial"
        (let [actual    (-> ["h|<ello"
                             "world>"]
                            (i/from-marked-text)
                            (process' [select-down]))
              expected  (-> ["hello"
                             "w|<orld>"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[\o \r \l \d]]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected)))))

      (testing "continuous"
        (let [actual    (-> ["h|<ello"
                             "world"
                             "someday>"]
                            (i/from-marked-text)
                            (process' [select-down select-down]))
              expected  (-> ["hello"
                             "world"
                             "s|<omeday>"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[\o \m \e \d \a \y]]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected))))))

    (testing "DEACTIVATION -"
      (let [actual    (-> ["he|<llo"
                           "wo>rld"]
                          (i/from-marked-text)
                          (process' [select-down]))
            expected  (-> ["hello"
                           "wo|rld"]
                          (i/from-marked-text))
            extracted (-> actual (i/extract) (:lines))]
        (is (= extracted nil))
        (is (= (:cursor actual) (:cursor expected)))
        (is (= (:selection actual) (:selection expected))))))

  (testing "LINE MIDDLE, where LINE BELOW SMALLER -"
    (testing "INCREASE -"
      (testing "initial"
        (let [actual    (-> ["hell|o"
                             "ha"]
                            (i/from-marked-text)
                            (process' [select-down]))
              expected  (-> ["hell<o"
                             "ha>|"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[\o] [\h \a]]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected)))))

      (testing "continuous"
        (let [actual    (-> ["bonjo|ur"
                             "some"
                             "hey"]
                            (i/from-marked-text)
                            (process' [select-down select-down]))
              expected  (-> ["bonjo<ur"
                             "some"
                             "hey>|"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[\u \r] [\s \o \m \e] [\h \e \y]]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected))))))

    (testing "DECREASE -"
      (testing "initial"
        (let [actual    (-> ["hell|<o"
                             "ha>"]
                            (i/from-marked-text)
                            (process' [select-down]))
              expected  (-> ["hello"
                             "ha|"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted nil))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected)))))

      (testing "continuous"
        (let [actual    (-> ["hel|<lo"
                             "world"
                             "some>"]
                            (i/from-marked-text)
                            (process' [select-down select-down]))
              expected  (-> ["hello"
                             "world"
                             "som|<e>"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[\e]]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected)))))))

  (testing "LINE END -"
    (testing "INCREASE -"
      (testing "initial"
        (let [actual    (-> ["hello|"
                             "someday"]
                            (i/from-marked-text)
                            (process' [select-down]))
              expected  (-> ["hello<"
                             "somed>|ay"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[] [\s \o \m \e \d]]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected)))))

      (testing "continuous"
        (let [actual    (-> ["hello|"
                             "world"
                             "thursday"]
                            (i/from-marked-text)
                            (process' [select-down select-down]))
              expected  (-> ["hello<"
                             "world"
                             "thurs>|day"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[] [\w \o \r \l \d] [\t \h \u \r \s]]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected))))))

    (testing "DECREASE -"
      (testing "initial"
        (let [actual    (-> ["hello|<"
                             "world"
                             "today>"]
                            (i/from-marked-text)
                            (process' [select-down]))
              expected  (-> ["hello"
                             "world|<"
                             "today>"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[] [\t \o \d \a \y]]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected)))))

      (testing "continuous"
        (let [actual    (-> ["hello|<"
                             "world"
                             "thursday"
                             "someday>"]
                            (i/from-marked-text)
                            (process' [select-down select-down]))
              expected  (-> ["hello"
                             "world"
                             "thurs|<day"
                             "someday>"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[\d \a \y] [\s \o \m \e \d \a \y]]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected))))))

    (testing "DEACTIVATION -"
      (let [actual    (-> ["howdy|<"
                           "somed>ay"]
                          (i/from-marked-text)
                          (process' [select-down]))
            expected  (-> ["howdy"
                           "somed|ay"]
                          (i/from-marked-text))
            extracted (-> actual (i/extract) (:lines))]
        (is (= extracted nil))
        (is (= (:cursor actual) (:cursor expected)))
        (is (= (:selection actual) (:selection expected))))))

  (testing "LINE END, where LINE BELOW SMALLER -"
    (testing "INCREASE -"
      (testing "initial"
        (let [actual    (-> ["hello|"
                             "you"]
                            (i/from-marked-text)
                            (process' [select-down]))
              expected  (-> ["hello<"
                             "you>|"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[] [\y \o \u]]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected)))))

      (testing "continuous"
        (let [actual    (-> ["hello|"
                             "you"
                             "ha"]
                            (i/from-marked-text)
                            (process' [select-down select-down]))
              expected  (-> ["hello<"
                             "you"
                             "ha>|"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[] [\y \o \u] [\h \a]]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected))))))

    (testing "DECREASE -"
      (testing "initial"
        (let [actual    (-> ["howdy|<"
                             "thursday>"]
                            (i/from-marked-text)
                            (process' [select-down]))
              expected  (-> ["howdy"
                             "thurs|<day>"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[\d \a \y]]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected)))))

      (testing "continuous"
        (let [actual    (-> ["howdy|<"
                             "some"
                             "thursday>"]
                            (i/from-marked-text)
                            (process' [select-down select-down]))
              expected  (-> ["howdy"
                             "some"
                             "thur|<sday>"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[\s \d \a \y]]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected))))))

    (testing "DEACTIVATION"
      (let [actual    (-> ["howdy|<"
                           "some>"]
                          (i/from-marked-text)
                          (process' [select-down select-down]))
            expected  (-> ["howdy"
                           "some|"]
                          (i/from-marked-text))
            extracted (-> actual (i/extract) (:lines))]
        (is (= extracted nil))
        (is (= (:cursor actual) (:cursor expected)))
        (is (= (:selection actual) (:selection expected)))))))

(deftest selects-left
  (testing "LINE MIDDLE -"
    (testing "INCREASING -"
      (testing "initial"
        (let [actual    (-> ["hello|"
                             "world"]
                            (i/from-marked-text)
                            (process' [select-left]))
              expected  (-> ["hell|<o>"
                             "world"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[\o]]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected)))))

      (testing "continuously"
        (let [actual    (-> ["hello|"
                             "world"]
                            (i/from-marked-text)
                            (process' [select-left select-left]))
              expected  (-> ["hel|<lo>"
                             "world"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[\l \o]]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected))))))

    (testing "DECREASING -"
      (testing "initial"
        (let [actual    (-> ["h<ell>|o"
                             "world"]
                            (i/from-marked-text)
                            (process' [select-left]))
              expected  (-> ["h<el>|lo"
                             "world"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[\e \l]]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected)))))

      (testing "continuously"
        (let [actual    (-> ["h<ell>|o"
                             "world"]
                            (i/from-marked-text)
                            (process' [select-left select-left]))
              expected  (-> ["h<e>|llo"
                             "world"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[\e]]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected)))))))

  (testing "DEACTIVATION -"
    (let [actual    (-> ["he<l>|lo"
                         "world"]
                        (i/from-marked-text)
                        (process' [select-left]))
          expected  (-> ["he|llo"
                         "world"]
                        (i/from-marked-text))
          extracted (-> actual (i/extract) (:lines))]
      (is (= extracted nil))
      (is (= (:cursor actual) (:cursor expected)))
      (is (= (:selection actual) (:selection expected)))))

  (testing "LINE START -"
    (testing "INCREASING -"
      (testing "initial"
        (let [actual    (-> ["hello"
                             "|world"]
                            (i/from-marked-text)
                            (process' [select-left]))
              expected  (-> ["hello|<"
                             ">world"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[]]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected)))))

      (testing "continuously"
        (let [actual    (-> ["hello"
                             "|world"]
                            (i/from-marked-text)
                            (process' [select-left select-left]))
              expected  (-> ["hell|<o"
                             ">world"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[\o] []]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected))))))

    (testing "DECREASING -"
      (testing "initial"
        (let [actual    (-> ["hel<lo"
                             ">|world"]
                            (i/from-marked-text)
                            (process' [select-left]))
              expected  (-> ["hel<lo>|"
                             "world"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[\l \o]]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected)))))

      (testing "continously"
        (let [actual    (-> ["hel<lo"
                             ">|world"]
                            (i/from-marked-text)
                            (process' [select-left select-left]))
              expected  (-> ["hel<l>|o"
                             "world"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[\l]]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected))))))

    (testing "DEACTIVATION -"
      (let [actual    (-> ["hello<"
                           ">|world"]
                          (i/from-marked-text)
                          (process' [select-left]))
            expected  (-> ["hello|"
                           "world"]
                          (i/from-marked-text))
            extracted (-> actual (i/extract) (:lines))]
        (is (= extracted nil))
        (is (= (:cursor actual) (:cursor expected)))
        (is (= (:selection actual) (:selection expected)))))))

(deftest selects-right
  (testing "LINE MIDDLE -"
    (testing "INCREASING -"
      (testing "initial"
        (let [actual    (-> ["he|llo"
                             "world"]
                            (i/from-marked-text)
                            (process' [select-right]))
              expected  (-> ["he<l>|lo"
                             "world"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[\l]]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected)))))

      (testing "continuously"
        (let [actual    (-> ["he|llo"
                             "world"]
                            (i/from-marked-text)
                            (process' [select-right select-right]))
              expected  (-> ["he<ll>|o"
                             "world"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[\l \l]]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected))))))

    (testing "DECREASING -"
      (testing "initial"
        (let [actual    (-> ["he|<llo>"
                             "world"]
                            (i/from-marked-text)
                            (process' [select-right]))
              expected  (-> ["hel|<lo>"
                             "world"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[\l \o]]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected)))))

      (testing "continuously"
        (let [actual    (-> ["he|<llo>"
                             "world"]
                            (i/from-marked-text)
                            (process' [select-right select-right]))
              expected  (-> ["hell|<o>"
                             "world"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[\o]]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected))))))

    (testing "DEACTIVATION -"
      (let [actual    (-> ["he|<l>lo"
                           "world"]
                          (i/from-marked-text)
                          (process' [select-right]))
            expected  (-> ["hel|lo"
                           "world"]
                          (i/from-marked-text))
            extracted (-> actual (i/extract) (:lines))]
        (is (= extracted nil))
        (is (= (:cursor actual) (:cursor expected)))
        (is (= (:selection actual) (:selection expected))))))

  (testing "LINE END -"
    (testing "INCREASING -"
      (testing "initial"
        (let [actual    (-> ["hello|"
                             "world"]
                            (i/from-marked-text)
                            (process' [select-right]))
              expected  (-> ["hello<"
                             ">|world"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[]]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected)))))

      (testing "continuously"
        (let [actual    (-> ["hello|"
                             "world"]
                            (i/from-marked-text)
                            (process' [select-right select-right]))
              expected  (-> ["hello<"
                             "w>|orld"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[] [\w]]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected))))))

    (testing "DECREASING -"
      (testing "initial"
        (let [actual    (-> ["hello|<"
                             "wor>ld"]
                            (i/from-marked-text)
                            (process' [select-right]))
              expected  (-> ["hello"
                             "|<wor>ld"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[\w \o \r]]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected)))))

      (testing "continuously"
        (let [actual    (-> ["hello|<"
                             "wor>ld"]
                            (i/from-marked-text)
                            (process' [select-right select-right]))
              expected  (-> ["hello"
                             "w|<or>ld"]
                            (i/from-marked-text))
              extracted (-> actual (i/extract) (:lines))]
          (is (= extracted [[\o \r]]))
          (is (= (:cursor actual) (:cursor expected)))
          (is (= (:selection actual) (:selection expected))))))

    (testing "DEACTIVATION -"
      (let [actual    (-> ["hello|<"
                           ">world"]
                          (i/from-marked-text)
                          (process' [select-right]))
            expected  (-> ["hello"
                           "|world"]
                          (i/from-marked-text))
            extracted (-> actual (i/extract) (:lines))]
        (is (= extracted nil))
        (is (= (:cursor actual) (:cursor expected)))
        (is (= (:selection actual) (:selection expected)))))))

(deftest selects-everything
  (let [actual    (-> ["hello"
                       "wor|ld"]
                      (i/from-marked-text)
                      (process' [select-all]))
        expected  (-> ["<hello"
                       "world>|"]
                      (i/from-marked-text))
        extracted (-> actual (i/extract) (:lines))]
    (is (= extracted [[\h \e \l \l \o] [\w \o \r \l \d]]))
    (is (= (:cursor actual) (:cursor expected)))
    (is (= (:selection actual) (:selection expected)))))

;; X. Joining

(deftest joins
  (let [text1     (-> ["hel|lo" "world"] (i/from-marked-text) (process' [select-right]))
        text2     (-> ["n|ew" "lines"] (i/from-marked-text) (process' [select-right]))
        conjoined (i/join-many text1 text2)
        text      (:lines conjoined)
        cursor    (:cursor conjoined)
        selection (:selection conjoined)]
    (is (= text [[\h \e \l \l \o]
                 [\w \o \r \l \d]
                 [\n \e \w]
                 [\l \i \n \e \s]]))
    (is (= cursor [2 2]))
    (is (= selection {:start [1 2] :end [2 2]}))))

;; XI. Expansion

(deftest proper-match-parens
  (testing "Closed pair expansion"
    (let [pair     (-> ["[1 2 3|]"]
                       (i/from-marked-text)
                       (i/find-pair))
          expected {:start [0 0]
                    :end   [7 0]}]
      (is (= pair expected))))

  (testing "open pair expansion"
    (let [pair     (-> ["|[1 2 3]"]
                       (i/from-marked-text)
                       (i/find-pair))
          expected {:start [0 0]
                    :end   [7 0]}]
      (is (= pair expected)))))

(deftest expands-to-words
  (let [word1-start     (-> ["|some line"] (i/from-marked-text) (process' [expand]) (i/extract) (:lines))
        word1-middle    (-> ["so|me line"] (i/from-marked-text) (process' [expand]) (i/extract) (:lines))
        sentence-middle (-> ["some| line"] (i/from-marked-text) (process' [expand]) (i/extract) (:lines))
        word2-start     (-> ["some |line"] (i/from-marked-text) (process' [expand]) (i/extract) (:lines))
        word2-middle    (-> ["some li|ne"] (i/from-marked-text) (process' [expand]) (i/extract) (:lines))
        word2-end       (-> ["some line|"] (i/from-marked-text) (process' [expand]) (i/extract) (:lines))]
    (is (= word1-start word1-middle [[\s \o \m \e]]))
    (is (= word2-start word2-middle word2-end [[\l \i \n \e]]))
    (is (= sentence-middle [[\s \o \m \e \space \l \i \n \e]]))))

(deftest expands-over-multiple-lines-from-space
  (let [from-space (-> ["first| "
                        "second"]
                       (i/from-marked-text)
                       (process' [expand])
                       (i/extract)
                       (:lines))]
    (is (= from-space  [[\f \i \r \s \t \space] [\s \e \c \o \n \d]]))))

(deftest expands-over-exprs
  (->> [[\( \)] [\[ \]] [\{ \}]]
       (run! (fn [[l r]]
               (let [start         (-> [(str "|" l l "some  word" r r)]
                                       (i/from-marked-text)
                                       (process' [expand])
                                       (i/extract)
                                       (:lines))
                     end           (-> [(str l l "some  word" r r "|")]
                                       (i/from-marked-text)
                                       (process' [expand])
                                       (i/extract)
                                       (:lines))
                     middle        (-> [(str l l "some | word" r r)]
                                       (i/from-marked-text)
                                       (process' [expand])
                                       (i/extract)
                                       (:lines))
                     between-start (-> [(str l "|" l "some  word" r r)]
                                       (i/from-marked-text)
                                       (process' [expand])
                                       (i/extract)
                                       (:lines))
                     between-end   (-> [(str l l "some  word" r "|" r)]
                                       (i/from-marked-text)
                                       (process' [expand])
                                       (i/extract)
                                       (:lines))]
                 (is (= start end [[l l \s \o \m \e \space \space \w \o \r \d r r]]))
                 (is (= middle between-start between-end [[l \s \o \m \e \space \space \w \o \r \d r]])))))))

(deftest expands-from-words-to-exprs
  (let [parens [[\( \)] [\[ \]] [\{ \}]]]
    (run!
      (fn [[ol or]]
        (run!
          (fn [[il ir]]
            (let [text   (-> [(str ol il "|some word" ir or)]
                             (i/from-marked-text))
                  word   (-> text
                             (process' [expand])
                             (i/extract)
                             (i/current-line))
                  expr   (-> text
                             (process' [expand expand])
                             (i/extract)
                             (i/current-line))
                  o-expr (-> text
                             (process' [expand expand expand])
                             (i/extract)
                             (i/current-line))]
              (is (= word [\s \o \m \e]))
              (is (= expr [il \s \o \m \e \space \w \o \r \d ir]))
              (is (= o-expr [ol il \s \o \m \e \space \w \o \r \d ir or]))))
          parens))
      parens)))

(deftest expands-from-words-to-exprs-over-multiple-lines
  (let [parens [[\( \)] [\[ \]] [\{ \}]]]
    (run!
      (fn [[ol or]]
        (run!
          (fn [[il ir]]
            (let [text   (-> (str ol "\n" il "|some\nword" ir "\n" or)
                             (i/from-cursored-string))
                  word   (-> text
                             (process' [expand])
                             (i/extract)
                             (:lines))
                  expr   (-> text
                             (process' [expand expand])
                             (i/extract)
                             (:lines))
                  o-expr (-> text
                             (process' [expand expand expand])
                             (i/extract)
                             (:lines))]
              (is (= word [[\s \o \m \e]]))
              (is (= expr [[il \s \o \m \e] [\w \o \r \d ir]]))
              (is (= o-expr [[ol] [il \s \o \m \e] [\w \o \r \d ir] [or]]))))
          parens))
      parens)))

;; XII. Copying

(deftest copies-within-line
  (let [text (-> "hello |world"
                 (i/from-cursored-string)
                 (i/select-right)
                 (i/select-right)
                 (i/copy)
                 (:clipboard)
                 (:lines))]
    (is (= text [[\w \o]]))))

(deftest copies-over-lines
  (let [text (-> "hel|lo\nworld"
                 (i/from-cursored-string)
                 (i/select-jump-right)
                 (i/select-jump-right)
                 (i/select-right)
                 (i/copy)
                 (:clipboard)
                 (:lines))]
    (is (= text [[\l \o] [\w]]))))

(deftest does-not-copy-when-nothing-selected
  (let [text (-> "hello |world"
                 (i/from-cursored-string)
                 (i/copy)
                 (:clipboard))]
    (is (= text nil))))

;;; XIII. Cutting

(deftest cuts-within-line
  (let [text    (-> "hello |world"
                    (i/from-cursored-string)
                    (i/select-right)
                    (i/select-right)
                    (i/cut))
        section (-> text (:clipboard) (:lines))
        cut     (-> text (:lines))]
    (is (= section [[\w \o]]))
    (is (= cut [[\h \e \l \l \o \space \r \l \d]]))))

(deftest cuts-over-lines
  (let [text    (-> "hello\nwor|ld\ntoday"
                    (i/from-cursored-string)
                    (i/select-jump-right)
                    (i/select-jump-right)
                    (i/select-right)
                    (i/cut))
        section (-> text (:clipboard) (:lines))
        cut     (-> text (:lines))]
    (is (= section [[\l \d] [\t]]))
    (is (= cut [[\h \e \l \l \o] [\w \o \r \o \d \a \y]]))))

(deftest does-not-cut-when-nothing-selected
  (let [text    (-> "hello |world"
                    (i/from-cursored-string)
                    (i/cut))
        section (:clipboard text)
        cut     (:lines text)]
    (is (= section nil))
    (is (= cut [[\h \e \l \l \o \space \w \o \r \l \d]]))))

;;; XIV. Pasting

(deftest pastes-within-line
  (let [text   (-> "hel|lo world" (i/from-cursored-string) (i/select-jump-right))
        copied (-> text (i/copy) (i/deselect) (i/paste) (:lines))
        cut    (-> text (i/cut) (i/deselect) (i/paste) (:lines))]
    (is (= copied [[\h \e \l \l \o \l \o \space \w \o \r \l \d]]))
    (is (= cut [[\h \e \l \l \o \space \w \o \r \l \d]]))))

(deftest pastes-over-lines
  (let [text   (-> "hello\nwor|ld\ntoday"
                   (i/from-cursored-string)
                   (i/select-jump-right)
                   (i/select-jump-right)
                   (i/select-right))
        copied (-> text (i/copy) (i/deselect) (i/paste) (:lines))
        cut    (-> text (i/cut) (i/deselect) (i/paste) (:lines))]
    (is (= copied [[\h \e \l \l \o] [\w \o \r \l \d] [\t \l \d] [\t \o \d \a \y]]))
    (is (= cut [[\h \e \l \l \o] [\w \o \r \l \d] [\t \o \d \a \y]]))))

(deftest pastes-overwriting-selections-within-line
  (let [text (-> "h|ello world"
                 (i/from-cursored-string)
                 (i/select-right)
                 (i/select-right)
                 (i/copy)
                 (i/deselect)
                 (i/select-right)
                 (i/paste)
                 (:lines))]
    (is (= text [[\h \e \l \e \l \o \space \w \o \r \l \d]]))))

(deftest pastes-overwriting-selections-over-lines
  (let [text (-> "hell|o\nworld\ntoday"
                 (i/from-cursored-string)
                 (i/select-right)
                 (i/select-right)
                 (i/select-right)
                 (i/copy)
                 (i/deselect)
                 (i/select-right)
                 (i/paste)
                 (:lines))]
    (is (= text [[\h \e \l \l \o] [\w \o] [\w \r \l \d] [\t \o \d \a \y]]))))

(deftest what-parens
  (-> ["|()"]
      (i/from-marked-text)
      (i/find-pair)
      (println)))

;; XV. Parens matching

(deftest matches-parens
  "This doesn't test anything properly"
  ;; s1 mismatches
  (is (= nil (-> ["|[[[45[]]"] (i/from-marked-text) (i/find-pair))))
  (is (= nil (-> ["[|[[45[]]"] (i/from-marked-text) (i/find-pair))))

  ;; s1 matches
  (is (not= nil (-> "[[|[45[]]" (i/from-cursored-string) (i/find-pair))))
  (is (not= nil (-> "[[[|45[]]" (i/from-cursored-string) (i/find-pair))))
  (is (not= nil (-> "[[[45|[]]" (i/from-cursored-string) (i/find-pair))))
  (is (not= nil (-> "[[[45[|]]" (i/from-cursored-string) (i/find-pair))))
  (is (not= nil (-> "[[[45[]|]" (i/from-cursored-string) (i/find-pair))))
  (is (not= nil (-> "[[[45[]]|" (i/from-cursored-string) (i/find-pair))))

  ;; s2 mismatches
  (is (= nil (-> "[(a| {} b)]" (i/from-cursored-string) (i/find-pair))))
  (is (= nil (-> "[(a {} |b)]" (i/from-cursored-string) (i/find-pair))))

  ;; s2 matches
  (is (not= nil (-> "|[(a {\n} b)]" (i/from-cursored-string) (i/find-pair))))
  (is (not= nil (-> "[|(a {\n} b)]" (i/from-cursored-string) (i/find-pair))))
  (is (not= nil (-> "[(|a {\n} b)]" (i/from-cursored-string) (i/find-pair))))
  (is (not= nil (-> "[(a |{\n} b)]" (i/from-cursored-string) (i/find-pair))))
  (is (not= nil (-> "[(a {|\n} b)]" (i/from-cursored-string) (i/find-pair))))
  (is (not= nil (-> "[(a {\n}| b)]" (i/from-cursored-string) (i/find-pair))))
  (is (not= nil (-> "[(a {\n} b|)]" (i/from-cursored-string) (i/find-pair))))
  (is (not= nil (-> "[(a {\n} b)|]" (i/from-cursored-string) (i/find-pair))))
  (is (not= nil (-> "[(a {\n} b)]|" (i/from-cursored-string) (i/find-pair))))

  ;; s3 mismatches
  (is (= nil (-> "[)(}|{)][" (i/from-cursored-string) (i/find-pair))))
  (is (= nil (-> "[)(}{|)][" (i/from-cursored-string) (i/find-pair))))
  (is (= nil (-> "[)(}{)]|[" (i/from-cursored-string) (i/find-pair))))
  (is (= nil (-> "[)(}{)][|" (i/from-cursored-string) (i/find-pair))))

  ;; s3 matches
  (is (not= nil (-> "|[)(}{)][" (i/from-cursored-string) (i/find-pair))))
  (is (not= nil (-> "[|)(}{)][" (i/from-cursored-string) (i/find-pair))))
  (is (not= nil (-> "[)|(}{)][" (i/from-cursored-string) (i/find-pair))))
  (is (not= nil (-> "[)(|}{)][" (i/from-cursored-string) (i/find-pair))))
  (is (not= nil (-> "[)(}{)|][" (i/from-cursored-string) (i/find-pair))))


  ;; s4 mismatches
  (is (= nil (-> "|ab4()" (i/from-cursored-string) (i/find-pair))))
  (is (= nil (-> "a|b4()" (i/from-cursored-string) (i/find-pair))))
  (is (= nil (-> "ab|4()" (i/from-cursored-string) (i/find-pair))))

  ;; s4 matches
  (is (not= nil (-> "ab4|()" (i/from-cursored-string) (i/find-pair))))
  (is (not= nil (-> "ab4(|)" (i/from-cursored-string) (i/find-pair))))
  (is (not= nil (-> "ab4()|" (i/from-cursored-string) (i/find-pair)))))

;; XVI. Extracting
(deftest extracts-regions
  (testing "Extracts with new lines lower"
    (let [text (-> ["he<llo"
                    ">|world"]
                   (i/from-marked-text)
                   (i/extract))
          expected (-> ["llo" ""]
                       (i/from-marked-text))]
      (is (:lines text) (:lines expected))))

  (testing "Extracts with new lines upper"
    (let [text (-> ["hello<"
                    "wor>|ld"]
                   (i/from-marked-text)
                   (i/extract))
          expected (-> ["" "wor"]
                       (i/from-marked-text))]
      (is (:lines text) (:lines expected))))

  (testing "Extracts single characters"
    (let [text (-> ["he<l>|lo"
                    "world"]
                   (i/from-marked-text)
                   (i/extract))
          expected (-> ["l"]
                       (i/from-marked-text))]
      (is (:lines text) (:lines expected))))

  (testing "Extracts words"
    (let [text (-> ["<hello>|"
                    "world"]
                   (i/from-marked-text)
                   (i/extract))
          expected (-> ["hello"]
                       (i/from-marked-text))]
      (is (:lines text) (:lines expected))))

  (testing "Extracts over lines"
    (let [text (-> ["hel<lo"
                    "wor>|ld"]
                   (i/from-marked-text)
                   (i/extract))
          expected (-> ["lo" "wor"]
                       (i/from-marked-text))]
      (is (:lines text) (:lines expected))))

  (testing "Extracts until end"
    (let [text (-> ["<hello"
                    "world>|"]
                   (i/from-marked-text)
                   (i/extract))
          expected (-> ["hello" "world"]
                       (i/from-marked-text))]
      (is (:lines text) (:lines expected)))))

;; XVII. Undoing / Redoing

(deftest undos-and-redos
  (let [text     (-> "hello w|orld" (i/from-cursored-string) (i/remember))
        delete   (-> text (i/delete-previous))
        original (-> text (:lines))
        deleted  (-> delete (:lines))
        undone   (-> delete (i/undo) (:lines))
        redone   (-> delete (i/undo) (i/redo) (:lines))]
    (is (= undone original))
    (is (= redone deleted))))

(deftest undos-are-limited
  (let [history (one (gen-history {:prefilled-size 50 :element-size 3}))
        text    (-> "hello world|"
                    (i/from-cursored-string)
                    (i/reset-history history (reverse history))
                    (i/new-line)
                    (i/remember)
                    (i/insert \a)
                    (i/remember)
                    (:history))
        size (-> text (count))
        action (-> text (first) (:lines))]
    (is (= size 50))
    (is (= action [[\h \e \l \l \o \space \w \o \r \l \d] [\a]]))))

(deftest undos-and-redos-are-balanced
  (let [text            (-> "hello |world"
                            (i/from-cursored-string)
                            (i/remember)
                            (i/insert \a)
                            (i/remember)
                            (i/insert \b))
        initial-history (:history text)
        initial-rhistory (:rhistory text)
        undo1-history   (-> text (i/undo) (:history))
        undo1-rhistory  (-> text (i/undo) (:rhistory))
        undo2-history   (-> text (i/undo) (i/undo) (:history))
        undo2-rhistory  (-> text (i/undo) (i/undo) (:rhistory))]
    (is (= (count initial-history) 2))
    (is (= (count initial-rhistory) 0))
    (is (= (count undo1-history) 1))
    (is (= (count undo1-rhistory) 1))
    (is (= (count initial-history) 2))
    (is (= (count undo2-history) 0))
    (is (= (count undo2-rhistory) 2))))

(deftest undos-and-redos-dont-affect-clipboard
  (let [text     (-> "hel|lo world"
                     (i/from-cursored-string)
                     (i/select-jump-right)
                     (i/remember)
                     (i/insert \a))
        original (:clipboard text)
        undone   (-> text (i/undo) (:clipboard))
        redone   (-> text (i/undo) (i/redo) (:clipboard))]
    (is (= original undone redone))))

(deftest undos-and-redos-are-preserved-between-each-other
  (let [text (-> "hello w|orld"
                 (i/from-cursored-string)
                 (process' [delete-previous
                            delete-previous
                            delete-previous
                            undo
                            undo
                            redo
                            redo])
                 (:lines))]
    (is (= text [[\h \e \l \l \o \r \l \d]]))))