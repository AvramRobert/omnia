(ns omnia.highlight)

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
(def ^:const std :standard)
(def ^:const std* :standard*)
(def ^:const slc-bg :selection-bg)

(def ^:const s0 std)

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
   std :white})

(def ops-colourscheme
  {slc-bg :blue})

(def default-colourscheme
  (merge syntax-colourscheme ops-colourscheme))

(defn ->start-list? [c] (= c \())
(defn ->end-list? [c] (= c \)))
(defn ->start-vector? [c] (= c \[))
(defn ->end-vector? [c] (= c \]))
(defn ->start-map? [c] (= c \{))
(defn ->end-map? [c] (= c \}))
(defn ->keyword? [c] (= c \:))
(defn ->string? [c] (= c \"))
(defn ->char? [c] (= c \\))
(defn ->number? [c]
  (try
    (number? (Integer/parseInt (str c)))
    (catch Exception _ false)))

(defn ->decimal? [c] (or (= c \.)
                         (= c \f)))
(defn ->comment? [c] (= c \;))

(defn ->break? [c] (= c \newline))

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
                       [state# (list colourscheme ckey#)]]))
                  (reduce concat)))))))

(deftrans ->comment
          ->break? [std cmt]
          :else [cmt cmt])

(deftrans ->standard
          ->reset? [std std]
          ->comment? [cmt cmt]
          ->start-list? [fnc lst]
          ->end-list? [std lst]
          ->start-vector? [std vct]
          ->end-vector? [std vct]
          ->start-map? [std hmp]
          ->end-map? [std hmp]
          ->keyword? [kwd kwd]
          ->string? [stg stg]
          ->char? [chr chr]
          ->number? [nr nr]
          :else [std* std])

(deftrans ->standard*
          ->reset? [std std]
          ->comment? [cmt cmt]
          ->start-list? [fnc lst]
          ->end-list? [std lst]
          ->start-vector? [std vct]
          ->end-vector? [std vct]
          ->start-map? [std hmp]
          ->end-map? [std hmp]
          ->string? [stg stg]
          ->char? [chr chr]
          :else [std* std])

(deftrans ->function
          ->comment? [cmt cmt]
          ->start-list? [fnc lst]
          ->end-list? [std lst]
          ->start-vector? [std vct]
          ->end-vector? [std vct]
          ->start-map? [std hmp]
          ->end-map? [std hmp]
          ->keyword? [kwd kwd]
          ->number? [nr nr]
          ->char? [chr chr]
          ->string? [stg stg]
          ->reset? [std std]
          :else [fnc* fnc])

(deftrans ->function*
          ->comment? [cmt cmt]
          ->start-list? [fnc lst]
          ->end-list? [std lst]
          ->start-vector? [std vct]
          ->end-vector? [std vct]
          ->start-map? [std hmp]
          ->end-map? [std hmp]
          ->reset? [std std]
          :else [fnc* fnc])

(deftrans ->keyword
          ->comment? [cmt cmt]
          ->start-list? [fnc lst]
          ->end-list? [std lst]
          ->start-vector? [std vct]
          ->end-vector? [std vct]
          ->start-map? [std hmp]
          ->end-map? [std hmp]
          ->reset? [std std]
          :else [kwd kwd])

(deftrans ->string
          ->string? [std stg]
          :else [stg stg])

(deftrans ->character
          ->reset? [std std]
          ->start-list? [fnc lst]
          ->end-list? [std lst]
          ->start-vector? [std vct]
          ->end-vector? [std vct]
          ->start-map? [std hmp]
          ->end-map? [std hmp]
          :else [chr chr])

(deftrans ->number
          ->comment? [cmt cmt]
          ->start-list? [fnc lst]
          ->end-list? [std lst]
          ->start-vector? [std vct]
          ->end-vector? [std vct]
          ->start-map? [std hmp]
          ->end-map? [std hmp]
          ->number? [nr nr]
          ->decimal? [nr nr]
          :else [std std])

(def state-machine {std  ->standard
                    std* ->standard*
                    fnc  ->function
                    fnc* ->function*
                    kwd  ->keyword
                    stg  ->string
                    chr  ->character
                    nr   ->number
                    cmt  ->comment})

(defn process
  ([input state]
    (process input state default-colourscheme))
  ([input state colourscheme]
   (if-let [transition (state-machine state)]
     (transition input colourscheme)
     [s0 (colourscheme s0)])))
