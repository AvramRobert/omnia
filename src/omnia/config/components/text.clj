(ns omnia.config.components.text
  (:require [schema.core :as s]
            [omnia.text.highlighting :as h]))

(def lists h/-list)
(def vectors h/-vector)
(def maps h/-map)
(def numbers h/-number)
(def characters h/-char)
(def strings h/-string)
(def keywords h/-keyword)
(def comments h/-comment)
(def words h/-word)
(def functions h/-function)
(def texts h/-text)
(def commas h/-comma)
(def selections :selection)
(def backgrounds :background)
(def foregrounds :foreground)

(def SyntaxElement
  (s/enum lists
          vectors
          maps
          numbers
          characters
          strings
          keywords
          comments
          words
          functions
          texts
          commas
          selections
          backgrounds
          foregrounds))


(def default :default)
(def font :font)
(def font-size :font-size)
(def palette :palette)

(def Default (s/eq default))

(def PredefinedColour
  (s/enum :default :black :white :red :green :blue :cyan :magenta :yellow))

(def CustomColour
  (s/constrained [(s/pred #(< % 255))] #(= 3 (count %))))

(def Colour
  (s/cond-pre PredefinedColour CustomColour))

(def Style (s/enum :bold :blinking :underline :strikethrough))

(def Palette Default)

(def Font
  (s/cond-pre Default s/Str))

(def FontSize
  (s/cond-pre Default s/Num))

(s/defn syntax-element :- SyntaxElement
  [emission :- h/Emission]
  emission)