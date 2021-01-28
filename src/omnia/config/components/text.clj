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

(def bold :bold)
(def blinking :blinking)
(def strikethrough :strikethrough)
(def underline :underline)

(def ColouredElement
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

(def font-path :font-path)
(def font-size :font-size)
(def palette :palette)

(def black :black)
(def white :white)
(def red :red)
(def green :green)
(def blue :blue)
(def cyan :cyan)
(def magenta :magenta)
(def yellow :yellow)
(def default :default)

(def PresetColour
  (s/enum black white red green blue cyan magenta yellow default))

(def RGBColour
  (s/constrained [(s/pred #(< % 255))] #(= 3 (count %))))

(def Colour
  (s/cond-pre PresetColour RGBColour))

(def Style (s/enum bold blinking underline strikethrough))

(def Palette s/Any)

(def FontPath s/Str)

(def FontSize s/Num)

(s/defn coloured-element :- ColouredElement
  [emission :- h/Emission]
  emission)