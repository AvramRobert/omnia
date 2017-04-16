(ns omnia.formatting
  (:gen-class)
  (require
    [omnia.input :as i]
    [fipp.engine :as e]
    [clojure.core.match :as m]
    [clojure.string :as s]
    [instaparse.core :as p]))

(comment
  "
  The general idea in fipp is that you can align groups of other groups.
  An :align block will be align all its children based on the current level
  of nesting.
  A :group block denotes an aggregation of a number of various children that will
  be considered as 1 thing by the parent node.")

(comment
  "Rules:
  Note: fipp takes care of nested, far-right newline indentations.

  1. s-exprs (of any kind) are defined in terms of:
     [:group [:align <nr> ..]

   Example:"
  [:group "(" "<call> " [:align "..."] ")"]

  "
  2. New lines in existing s-exprs are introduced with:
    :line ... (never with :break)

  Example:"
  [:group [:align ".." :line ".."] :line ".."]

  "
  3. Collections are themselves to be handled like s-exprs.")

(def parse (p/parser
             "group =  align              |
                      '(' align ')'       |
                      '[' align ']'       |
                      '{' align '}'
              align =  recur
              <recur> = recur recur |
                        text  |
                        line  |
                        group |
                        align
              line  = '\n'
              <text>  = #'[^()\\[\\]\\{\\}\\n]*'"))

(defn normalise [orig formatted]
  (let [seeker (assoc formatted :cursor (:cursor orig))
        spaces (fn [s] (->> (i/line s)
                            (take-while #(= % \space))
                            (count)))]
    (i/move-x seeker #(-> % (+ (spaces seeker)) (- (spaces orig))))))

(defn educe [document]
  (->> document
       (e/serialize)
       (eduction
         e/annotate-rights
         (e/annotate-begins {:width 10})
         (e/format-nodes {:width 10}))))

(defn format-seeker [seeker]
  (try (->> seeker
            (i/stringify)
            (parse)
            (educe)
            (apply str)                        ;; i think this step can be omitted
            (i/str->lines)
            (i/seeker)
            (normalise seeker))
       (catch Exception _ seeker)))



