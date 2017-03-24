(ns omnia.syntax
  (:gen-class))

(comment
  "Syntax glitches:
    1234asdasdas):bla => :bla processed as a keyword because ( -> default
  ")

(def ^:const syntax :syntax)
(def ^:const syntax* :syntax*)
(def ^:const keywords :keywords)
(def ^:const strings :strings)
(def ^:const numbers :numbers)
(def ^:const default :default)
(def ^:const default* :default*)
(def ^:const characters :chars)

(def colour-map {syntax   :yellow
                 keywords :magenta
                 strings  :green
                 numbers  :blue
                 default  :white})

(defn ->syntax? [c] (= c \())
(defn ->keyword? [c] (= c \:))
(defn ->default? [c] (or (= c \space)
                         (= c \newline)
                         (= c \))))

(defn ->string? [c] (= c \"))
(defn ->character? [c] (= c \\))
(defn ->number? [c]
  (try
    (number? (Integer/parseInt (str c)))
    (catch Exception _ false)))

(defn ->decimal? [c] (or (= c \.)
                         (= c \f)))

(defn emit [state]
  (get colour-map state))

(def default-t (fn [c]
                 (cond
                   (->syntax? c) [syntax (emit default)]
                   (->keyword? c) [keywords (emit keywords)]
                   (->string? c) [strings (emit strings)]
                   (->character? c) [characters (emit strings)]
                   (->number? c) [numbers (emit numbers)]
                   (->default? c) [default (emit default)]
                   :else [default* (emit default)])))

(def default*-t (fn [c]
                  (cond
                    (->syntax? c) [syntax (emit default)]
                    (->string? c) [strings (emit strings)]
                    (->default? c) [default (emit default)]
                    :else [default* (emit default)])))

(def syntax-t (fn [c]
                (cond
                  (->syntax? c) [syntax (emit default)]
                  (->keyword? c) [keywords (emit keywords)]
                  (->default? c) [default (emit default)]
                  :else [syntax (emit syntax)])))

(def syntax*-t (fn [c]
                 (cond
                   (->syntax? c) [syntax (emit default)]
                   (->default? c) [default (emit default)]
                   :else [syntax* (emit syntax*)])))

(def keywords-t (fn [c]
                  (cond
                    (->syntax? c) [syntax (emit default)]
                    (->default? c) [default (emit default)]
                    :else [keywords (emit keywords)])))

(def strings-t (fn [c]
                 (cond
                   (->string? c) [default (emit strings)]
                   :else [strings (emit strings)])))

(def characters-t (fn [c] [default (emit strings)]))

(def numbers-t (fn [c]
                 (cond
                   (->syntax? c) [syntax (emit default)]
                   (->number? c) [numbers (emit numbers)]
                   (->decimal? c) [numbers (emit numbers)]
                   :else [default (emit default)])))

(def state-machine {default     default-t
                    default*    default*-t
                    syntax      syntax-t
                    syntax*     syntax*-t
                    keywords    keywords-t
                    strings     strings-t
                    characters  characters-t
                    numbers     numbers-t})

(def s0 default)

(defn process [state input]
  (if-let [transition (get state-machine state)]
    (transition input)
    [default (emit default)]))

(defn reset [state] s0)
