(ns omnia.highlight
  (require [omnia.more :refer [map-vals]]))

(def ^:const fnc :function)
(def ^:const fnc* :function*)
(def ^:const lst :list)
(def ^:const vct :vector)
(def ^:const hmp :map)
(def ^:const kwd :keyword)
(def ^:const stg :string)
(def ^:const chr :char)
(def ^:const nr :number)
(def ^:const cmt :comment)
(def ^:const txt :text)
(def ^:const txt* :txt*)
(def ^:const slc-bg :selection)

(def ^:const s0 txt)

(def syntax-colourscheme
  {lst :white
   vct :white
   hmp :white
   fnc :yellow
   kwd :cyan
   stg :green
   chr :green
   nr  :blue
   cmt :magenta
   txt :white})

(def ops-colourscheme
  {slc-bg :blue})

(defn no-colourscheme [cs]
  (map-vals (constantly :white) cs))

(defn selection-scheme [cs]
  (-> (no-colourscheme cs)
      (assoc slc-bg (cs slc-bg))))

(def default-cs
  (merge syntax-colourscheme ops-colourscheme))

(def default-selection-cs
  (selection-scheme default-cs))

(def ^:private nrs #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})
(defn ->start-list? [c] (= c \())
(defn ->end-list? [c] (= c \)))
(defn ->start-vector? [c] (= c \[))
(defn ->end-vector? [c] (= c \]))
(defn ->start-map? [c] (= c \{))
(defn ->end-map? [c] (= c \}))
(defn ->keyword? [c] (= c \:))
(defn ->string? [c] (= c \"))
(defn ->char? [c] (= c \\))
(defn ->comment? [c] (= c \;))
(defn ->break? [c] (= c \newline))
(defn ->number? [c] (contains? nrs c))

(defn ->decimal? [c] (or (= c \.)
                         (= c \f)))

(defn ->reset? [c] (or (->break? c)
                       (= c \space)))

(defmacro deftrans [name & transitions]
  (assert (even? (count transitions)) "`deftrans` expects an even number of forms")
  (let [character (gensym)
        colourscheme (gensym)]
    `(def ~name
       (fn [~character ~colourscheme]
         (cond
           ~@(->> transitions
                  (partition 2)
                  (map
                    (fn [[pred# [state# ckey#]]]
                      [(if (= :else pred#) :else (list pred# character))
                       [state# (list colourscheme ckey# :white)]]))
                  (reduce concat)))))))

(deftrans ->comment
          ->break? [txt cmt]
          :else [cmt cmt])

(deftrans ->standard
          ->reset? [txt txt]
          ->comment? [cmt cmt]
          ->start-list? [fnc lst]
          ->end-list? [txt lst]
          ->start-vector? [txt vct]
          ->end-vector? [txt vct]
          ->start-map? [txt hmp]
          ->end-map? [txt hmp]
          ->keyword? [kwd kwd]
          ->string? [stg stg]
          ->char? [chr chr]
          ->number? [nr nr]
          :else [txt* txt])

(deftrans ->standard*
          ->reset? [txt txt]
          ->comment? [cmt cmt]
          ->start-list? [fnc lst]
          ->end-list? [txt lst]
          ->start-vector? [txt vct]
          ->end-vector? [txt vct]
          ->start-map? [txt hmp]
          ->end-map? [txt hmp]
          ->string? [stg stg]
          ->char? [chr chr]
          :else [txt* txt])

(deftrans ->function
          ->comment? [cmt cmt]
          ->start-list? [fnc lst]
          ->end-list? [txt lst]
          ->start-vector? [txt vct]
          ->end-vector? [txt vct]
          ->start-map? [txt hmp]
          ->end-map? [txt hmp]
          ->keyword? [kwd kwd]
          ->number? [nr nr]
          ->char? [chr chr]
          ->string? [stg stg]
          ->reset? [txt txt]
          :else [fnc* fnc])

(deftrans ->function*
          ->comment? [cmt cmt]
          ->start-list? [fnc lst]
          ->end-list? [txt lst]
          ->start-vector? [txt vct]
          ->end-vector? [txt vct]
          ->start-map? [txt hmp]
          ->end-map? [txt hmp]
          ->reset? [txt txt]
          :else [fnc* fnc])

(deftrans ->keyword
          ->comment? [cmt cmt]
          ->start-list? [fnc lst]
          ->end-list? [txt lst]
          ->start-vector? [txt vct]
          ->end-vector? [txt vct]
          ->start-map? [txt hmp]
          ->end-map? [txt hmp]
          ->reset? [txt txt]
          :else [kwd kwd])

(deftrans ->string
          ->string? [txt stg]
          :else [stg stg])

(deftrans ->character
          ->reset? [txt txt]
          ->start-list? [fnc lst]
          ->end-list? [txt lst]
          ->start-vector? [txt vct]
          ->end-vector? [txt vct]
          ->start-map? [txt hmp]
          ->end-map? [txt hmp]
          :else [chr chr])

(deftrans ->number
          ->comment? [cmt cmt]
          ->start-list? [fnc lst]
          ->end-list? [txt lst]
          ->start-vector? [txt vct]
          ->end-vector? [txt vct]
          ->start-map? [txt hmp]
          ->end-map? [txt hmp]
          ->number? [nr nr]
          ->decimal? [nr nr]
          :else [txt txt])

(def state-machine {txt  ->standard
                    txt* ->standard*
                    fnc  ->function
                    fnc* ->function*
                    kwd  ->keyword
                    stg  ->string
                    chr  ->character
                    nr   ->number
                    cmt  ->comment})

(defn process
  ([input state]
    (process input state default-cs))
  ([input state colourscheme]
   (if-let [transition (state-machine state)]
     (transition input colourscheme)
     [s0 (colourscheme s0)])))
