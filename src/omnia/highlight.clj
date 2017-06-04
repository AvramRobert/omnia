(ns omnia.highlight)

(def ^:const stx :syntax)
(def ^:const stx* :syntax*)
(def ^:const dst :structure)
(def ^:const kwd :keyword)
(def ^:const stg :string)
(def ^:const chr :char)
(def ^:const nr :number)
(def ^:const std :standard)
(def ^:const std* :standard*)

(def ^:const s0 std)

(def default-colorscheme
  {stx :yellow
   dst :white
   kwd :cyan
   stg :green
   chr :green
   nr  :blue
   std :white})

(defn ->syntax? [c] (= c \())
(defn ->structure? [c] (or (= c \{)
                           (= c \[)))
(defn ->keyword? [c] (= c \:))
(defn ->standard? [c] (or (= c \space)
                          (= c \newline)
                          (= c \))
                          (= c \])
                          (= c \})))

(defn ->string? [c] (= c \"))
(defn ->char? [c] (= c \\))
(defn ->number? [c]
  (try
    (number? (Integer/parseInt (str c)))
    (catch Exception _ false)))

(defn ->decimal? [c] (or (= c \.)
                         (= c \f)))

(defmacro deftrans [name & transitions]
  (assert (even? (count transitions)) "Transition excepts an even number of forms")
  (let [character (gensym)
        colourscheme (gensym)]
    `(def ~name
       (fn [~character ~colourscheme]
         (cond
           ~@(->> transitions
                  (partition 2)
                  (map
                    (fn [[pred# [next-state# colour-key#]]]
                      [(list pred# character)
                       [next-state# (list colourscheme colour-key#)]]))
                  (reduce concat)))))))


(deftrans standard
          ->syntax? [stx std]
          ->structure? [dst dst]
          ->keyword? [kwd kwd]
          ->string? [stg stg]
          ->char? [chr chr]
          ->number? [nr nr]
          ->standard? [std std]
          :else [std std])

(deftrans standard*
          ->syntax? [stx std]
          ->string? [stg stg]
          ->standard? [std std]
          :else [std* std])

(deftrans datastructure
          ->syntax? [stx std]
          ->keyword? [kwd kwd]
          ->number? [nr nr]
          ->string? [stg stg]
          :else [std std])

(deftrans syntax
          ->syntax? [stx std]
          ->keyword? [kwd kwd]
          ->number? [nr nr]
          ->char? [chr chr]
          ->string? [stg stg]
          ->standard? [std std]
          :else [stx* stx])

(deftrans syntax*
          ->syntax? [stx std]
          ->standard? [std std]
          :else [stx* stx])

(deftrans keywords
          ->syntax? [stx std]
          ->standard? [std std]
          :else [kwd kwd])

(deftrans string
          ->string? [std stg]
          :else [stg stg])

(deftrans character
          :else [std stg])

(deftrans number
          ->syntax? [stx std]
          ->number? [nr nr]
          ->decimal? [nr nr]
          :else [std std])

(def state-machine {std  standard
                    std* standard*
                    stx  syntax
                    stx* syntax*
                    dst  datastructure
                    kwd  keywords
                    stg  string
                    chr  character
                    nr   number})

(defn process
  ([state input]
    (process state input default-colorscheme))
  ([state input colourscheme]
   (if-let [transition (state-machine state)]
     (transition input colourscheme)
     [s0 (colourscheme s0)])))
