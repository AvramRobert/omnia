(ns omnia.repl.format
  (:require [fipp.visit :refer [visit]]
            [omnia.repl.text :as i]
            [omnia.util.arithmetic :refer [--]]
            [fipp.engine :as e]
            [fipp.edn :as edn]
            [clojure.edn :as clj-edn]
            [clojure.string :as s]
            [instaparse.core :as p]
            [halfling.task :as t]))

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

(defn spaces [seeker]
  (->> (i/current-line seeker)
       (take-while i/space?)
       (count)))

(defn normalise [original formatted]
  "The update order must be kept!
  First selection, then rebase, then movement!"
  (let [clipboard   (:clipboard original)
        form-indent (-> formatted (assoc :cursor (:cursor original)) (spaces))
        real-indent (spaces original)]
    (-> original
        (i/rebase (fn [_] (:lines formatted)))
        (i/move-x #(-> % (+ form-indent) (- real-indent)))
        (assoc :clipboard clipboard))))

(defn deform [seeker]
  (let [cursor-onset (spaces seeker)]
    (-> seeker
        (i/rebase (fn [lines]
                    (mapv #(->> % (drop-while i/space?) (vec)) lines)))
        (i/move-x #(-- % cursor-onset)))))

(defn edn-document
  ([x] (edn-document x {}))
  ([x options]
   (let [defaults {:symbols      {}
                   :print-length *print-length*
                   :print-level  *print-level*
                   :print-meta   *print-meta*}
         printer (edn/map->EdnPrinter (merge defaults options))]
     (visit printer x))))

(defn educe [document]
  (->> document
       (e/serialize)
       (eduction
         e/annotate-rights
         (e/annotate-begins {:width 10})
         (e/format-nodes {:width 10}))))

(defn edn? [string]
  (let [trimmed (s/trim string)
        starts-with? (fn [pattern] (s/starts-with? trimmed pattern))]
    (or (starts-with? "(")
        (starts-with? "[")
        (starts-with? "{")
        (starts-with? "#{"))))

(defn- fmt-lisp [sexprs]
  (->> sexprs (parse) (educe) (apply str)))

(defn- fmt-edn [edn-str]
  (->> edn-str
       (clj-edn/read-string)
       (edn-document)
       (educe)
       (apply str)))

(defn- fmt [x f]
  @(-> (t/task (f x))
       (t/recover (constantly x))
       (t/run)))

(defn format-str [expr]
  (fmt expr #(if (edn? %)
               (fmt-edn %)
               (fmt-lisp %))))

(defn format-seeker [seeker]
  (fmt seeker
       #(let [original (deform %)]
          (->> (i/as-string original)
               (fmt-lisp)
               (i/from-string)
               (normalise original)))))
