(ns omnia.highlight
  (use glow.terminal)
  (require [glow.regex :as r]
           [instaparse.core :as p]
           [lanterna.constants :refer [colors]]
           [glow.ansi :as ansi]
           [clj-antlr.core :as a]
           [clojure.core.match :as m]
           [omnia.input :as i]
           [omnia.more :refer [split-by]]))

(comment
  ; FIXME
  "The parser is very strict and picky.
  If any of the special symbols like ~ ` ' @ ~@ # are used
  without any expression or something that completes the clause,
  the parser fails, because it doesn't default them to simple
  symbols when they are on their own

  I fixed all that seem obvious from the grammar. New ones might however appear.")

(def grammar (a/parser (slurp "resources/clojure.g4")))

(defn parse [s]
  (a/parse grammar s))

(defn valid? [colour]
  (contains? colors colour))

(def default-colorscheme
  {:definition :yellow
   :core-fn :yellow
   :special-form :yellow
   :macro :yellow

   :nil :cyan
   :boolean :cyan
   :number :blue
   :string :green
   :character :green
   :keyword :magenta
   :regex :green

   :comment :yellow
   :s-exp :white
   :variable :blue
   :exception :red
   :repeat  :green
   :conditional :green
   :reader-char :red
   :blank :default
   :default :white})

(defn pair [colourscheme type elem]
  (let [colour (get colourscheme type :white)]
    (if (valid? colour)
      [elem colour]
      [elem :white])))

(defn paint-arbitrary [colourscheme]
  (fn [s]
    (cond
      (r/match-macro s) (pair colourscheme :macro s)
      (r/match-special s) (pair colourscheme :special-form s)
      (r/match-reader-char s) (pair colourscheme :reader-char s)
      (r/match-definition s) (pair colourscheme :definition s)
      (r/match-core-fn s) (pair colourscheme :core-fn s)
      (r/match-variable s) (pair colourscheme :variable s)
      (r/match-conditional s) (pair colourscheme :conditional s)
      (r/match-repeat s) (pair colourscheme :repeat s)
      (r/match-exception s) (pair colourscheme :exception s)
      :else (pair colourscheme :default s))))

(defn paint-collection [colourscheme]
  (fn [& args]
    [(pair colourscheme :s-exp (first args))
     (->> args rest (apply concat) vec)]))

(defn paint-close [colourscheme]
  (fn [parens]
    (pair colourscheme :s-exp parens)))

(defn paint-reader-macro [colorscheme]
  (fn [& args]
    [(pair colorscheme :reader-char (first args))
     (vec (rest args))]))

(defn reverse-paint-reader-macro [colorscheme]
  (fn [& args]
    [(pair colorscheme :default (first args))
     (pair colorscheme :reader-char (second args))]))

(defn paint-whitespace [colourscheme]
  (fn [s]
    (->> (i/str->lines s)
         (mapv
           #(m/match [%]
                     [[]] [:newline]
                     [[\space & _]] (pair colourscheme :blank (apply str %)))))))

(defn paint-symbol [colorscheme type]
  (fn [s] (pair colorscheme type s)))

(defn unite [& args] (->> args (apply concat) (vec)))
(defn pass-on [& args] args)


(defn transmute [colourscheme parsed-tree]
  (->> parsed-tree
       (p/transform
         {:simple_sym str
          :simple_keyword str
          :macro_keyword str
          :ns_symbol str

          ;; characters
          :named_char str
          :any_char str
          :u_hex_quad str

          ;; literals
          :literal unite
          :string (paint-symbol colourscheme :string)
          :regex (paint-symbol colourscheme :regex)
          :number (paint-symbol colourscheme :number)
          :character (paint-symbol colourscheme :character)
          :nil (paint-symbol colourscheme :nil)
          :boolean (paint-symbol colourscheme :boolean)
          :keyword (paint-symbol colourscheme :keyword)
          :param_name (paint-symbol colourscheme :reader-char)
          :symbol (paint-arbitrary colourscheme)

          ;; reader macro characters
          :reader_macro unite
          :quote (paint-reader-macro colourscheme)
          :backtick (paint-reader-macro colourscheme)
          :unquote (paint-reader-macro colourscheme)
          :unquote_splicing (paint-reader-macro colourscheme)
          :tag (paint-reader-macro colourscheme)
          :deref (paint-reader-macro colourscheme)
          :gensym (reverse-paint-reader-macro colourscheme)
          :lambda (paint-reader-macro colourscheme)
          :meta_data (paint-reader-macro colourscheme)
          :var_quote (paint-reader-macro colourscheme)
          :host_expr (paint-reader-macro colourscheme)
          :discard (paint-reader-macro colourscheme)
          :dispatch (paint-reader-macro colourscheme)
          :close (paint-close colourscheme)

          ;; top level
          :file unite
          :forms unite
          :form pass-on

          ;; collections
          :map (paint-collection colourscheme)
          :list (paint-collection colourscheme)
          :vector (paint-collection colourscheme)
          :set (paint-collection colourscheme)

          ;; extras
          :comment (paint-symbol colourscheme :comment)
          :whitespace (paint-whitespace colourscheme)})
       (flatten)
       (split-by #(not= :newline %))
       (mapv #(->> % (partition 2) (map vec)))
       (mapv vec)))

(defn highlight
  ([text]
   (highlight default-colorscheme text))
  ([colourscheme text]
   (->> text (parse) (vec) (transmute colourscheme))))

(defn highlight-seeker
  ([seeker]
    (highlight-seeker default-colorscheme seeker))
  ([colourscheme seeker]
   (->> seeker (i/stringify) (highlight))))
