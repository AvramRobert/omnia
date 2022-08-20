(ns omnia.schema.syntax
  (:require [schema.core :as s]))

(def ^:const lists :lists)
(def ^:const vectors :vectors)
(def ^:const maps :maps)
(def ^:const sets :sets)
(def ^:const numbers :numbers)
(def ^:const characters :characters)
(def ^:const keywords :keywords)
(def ^:const texts :texts)
(def ^:const strings :strings)
(def ^:const comments :comments)
(def ^:const functions :functions)
(def ^:const words :words)
(def ^:const commas :commas)
(def ^:const selections :selections)
(def ^:const backgrounds :backgrounds)
(def ^:const foregrounds :foregrounds)

(def bold :bold)
(def blinking :blinking)
(def strikethrough :strikethrough)
(def underline :underline)

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

(def elements
  #{lists
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
    foregrounds})

(def styles
  #{bold
    blinking
    strikethrough
    underline})

(def colours
  #{black
    white
    red
    blue
    cyan
    magenta
    yellow
    green
    default})

(def SyntaxElement
  (apply s/enum elements))

(def Style (apply s/enum styles))

(def PresetColour
  (apply s/enum colours))

(def RGBColour
  (s/constrained [(s/pred #(<= % 255))] #(= 3 (count %))))

(def Colour
  (s/cond-pre PresetColour RGBColour))

(def Palette s/Any)

(def FontPath s/Str)

(def FontSize s/Num)
