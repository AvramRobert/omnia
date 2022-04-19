(ns omnia.config.schema
  (:require [schema.core :as s]
            [omnia.components.syntax :as t]
            [omnia.components.keys :as i]
            [omnia.components.actions :as e]
            [omnia.util.collection :refer [map-vals]]))

(def UserKeyBinding
  {:key                    i/Key
   (s/optional-key :alt)   s/Bool
   (s/optional-key :ctrl)  s/Bool
   (s/optional-key :shift) s/Bool})

(def UserKeyMap
  {e/docs              UserKeyBinding
   e/signature         UserKeyBinding
   e/expand            UserKeyBinding
   e/undo              UserKeyBinding
   e/redo              UserKeyBinding
   e/paste             UserKeyBinding
   e/copy              UserKeyBinding
   e/cut               UserKeyBinding
   e/select-all        UserKeyBinding
   e/move-up           UserKeyBinding
   e/move-down         UserKeyBinding
   e/move-left         UserKeyBinding
   e/move-right        UserKeyBinding
   e/jump-left         UserKeyBinding
   e/jump-right        UserKeyBinding
   e/select-up         UserKeyBinding
   e/select-down       UserKeyBinding
   e/select-left       UserKeyBinding
   e/select-right      UserKeyBinding
   e/jump-select-left  UserKeyBinding
   e/jump-select-right UserKeyBinding
   e/delete-previous   UserKeyBinding
   e/delete-current    UserKeyBinding
   e/new-line          UserKeyBinding
   e/paren-match       UserKeyBinding
   e/suggest           UserKeyBinding
   e/scroll-up         UserKeyBinding
   e/scroll-down       UserKeyBinding
   e/prev-eval         UserKeyBinding
   e/next-eval         UserKeyBinding
   e/indent            UserKeyBinding
   e/clear             UserKeyBinding
   e/evaluate          UserKeyBinding
   e/exit              UserKeyBinding})

(def UserHighlighting
  {t/lists       t/Colour
   t/vectors     t/Colour
   t/maps        t/Colour
   t/numbers     t/Colour
   t/characters  t/Colour
   t/strings     t/Colour
   t/keywords    t/Colour
   t/comments    t/Colour
   t/words       t/Colour
   t/functions   t/Colour
   t/texts       t/Colour
   t/commas      t/Colour})

(def UserTerminal
  {(s/optional-key t/font-path) t/FontPath
   (s/optional-key t/font-size) t/FontSize
   (s/optional-key t/palette)   t/Palette})

(def UserConfig
  {:keymap                    UserKeyMap
   :syntax                    UserHighlighting
   (s/optional-key :terminal) UserTerminal})

(def KeyBinding
  {:key   i/Key
   :alt   s/Bool
   :ctrl  s/Bool
   :shift s/Bool})

(def KeyMap
  (map-vals (constantly KeyBinding) UserKeyMap))

(def Highlighting
  {t/SyntaxElement t/RGBColour})

(def Syntax
  {:standard  Highlighting
   :selection Highlighting
   :clean-up  Highlighting})

(def Terminal
  {t/font-path                t/FontPath
   t/font-size                t/FontSize
   (s/optional-key t/palette) t/Palette})

(def Config
  {:keymap   KeyMap
   :syntax   Syntax
   :terminal Terminal})