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

(def colour-map {stx :yellow
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

(defn emit [state]
  (get colour-map state))

(defn ->std [c]
  (cond
    (->syntax? c) [stx (emit std)]
    (->structure? c) [dst (emit dst)]
    (->keyword? c) [kwd (emit kwd)]
    (->string? c) [stg (emit stg)]
    (->char? c) [chr (emit chr)]
    (->number? c) [nr (emit nr)]
    (->standard? c) [std (emit std)]
    :else [std* (emit std)]))

(defn ->std* [c]
  (cond
    (->syntax? c) [stx (emit std)]
    (->string? c) [stg (emit stg)]
    (->standard? c) [std (emit std)]
    :else [std* (emit std)]))

(defn ->dst [c]
  (cond
    (->syntax? c) [stx (emit std)]
    (->keyword? c) [kwd (emit kwd)]
    (->number? c) [nr (emit nr)]
    (->string? c) [stg (emit stg)]
    :else [std (emit std)]))

(defn ->stx [c]
  (cond
    (->syntax? c) [stx (emit std)]
    (->keyword? c) [kwd (emit kwd)]
    (->number? c) [nr (emit nr)]
    (->char? c) [chr (emit chr)]
    (->string? c) [stg (emit stg)]
    (->standard? c) [std (emit std)]
    :else [stx* (emit stx)]))

(defn ->stx* [c]
  (cond
    (->syntax? c) [stx (emit std)]
    (->standard? c) [std (emit std)]
    :else [stx* (emit stx)]))

(defn ->kwd [c]
  (cond
    (->syntax? c) [stx (emit std)]
    (->standard? c) [std (emit std)]
    :else [kwd (emit kwd)]))

(defn ->stg [c]
  (cond
    (->string? c) [std (emit stg)]
    :else [stg (emit stg)]))

(defn ->chr [c]
  [std (emit stg)])

(defn ->nr [c]
  (cond
    (->syntax? c) [stx (emit std)]
    (->number? c) [nr (emit nr)]
    (->decimal? c) [nr (emit nr)]
    :else [std (emit std)]))

(def state-machine {std  ->std
                    std* ->std*
                    stx  ->stx
                    stx* ->stx*
                    dst  ->dst
                    kwd  ->kwd
                    stg  ->stg
                    chr  ->chr
                    nr   ->nr})

(def ^:const s0 std)
(defn reset [state] s0)

(defn process [state input]
  (if-let [transition (get state-machine state)]
    (transition input)
    [s0 (emit s0)]))
