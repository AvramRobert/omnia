(ns omnia.formatting
  (:gen-class))

;; FIXME: find some simple way to format code

(comment
  "This will transform a fipp document into a proper seeker.
   The formatting is also correct
   One important thing to note is that a line is broken only if the input
   exceeds the limit imposed for that line.

   The groupings appropriately takes care of the formatting,
   but the way the formatting happens, I need to find a way to enforce the line breaks.
   I will most definitely need to update the formatting options for each key input."

  (require [fipp.engine :as e])
  (use omnia.input)

  (->> document
       (e/serialize)
       (eduction
         e/annotate-rights
         (e/annotate-begins {:width 10})                    ;; the width dictates if new lines should be created
         (e/format-nodes {:width 10}))
       (partition-by #(= % "\n"))
       (map #(apply str %))
       (filter #(not= % "\n"))
       (map str->lines)
       (reduce concat)
       (vec)
       (seeker)))
