(ns omnia.schema.config
  (:require [schema.core :as s]
            [omnia.schema.syntax :as st]
            [omnia.schema.event :as e]
            [omnia.schema.keymap :as k]
            [omnia.util.collection :refer [map-vals]]))

(def UserKeyBinding
  {:key                    k/Key
   (s/optional-key :alt)   s/Bool
   (s/optional-key :ctrl)  s/Bool
   (s/optional-key :shift) s/Bool})

(def UserKeyMap
  {e/docs              UserKeyBinding
   e/signature         UserKeyBinding
   e/expand-select     UserKeyBinding
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
  {st/lists      st/Colour
   st/vectors    st/Colour
   st/maps       st/Colour
   st/numbers    st/Colour
   st/characters st/Colour
   st/strings    st/Colour
   st/keywords   st/Colour
   st/comments   st/Colour
   st/words      st/Colour
   st/functions  st/Colour
   st/texts      st/Colour
   st/commas     st/Colour})

(def UserTerminal
  {(s/optional-key st/font-path) st/FontPath
   (s/optional-key st/font-size) st/FontSize
   (s/optional-key st/palette)   st/Palette})

(def UserConfig
  {:keymap                    UserKeyMap
   :syntax                    UserHighlighting
   (s/optional-key :terminal) UserTerminal})

(def KeyBinding
  {:key   k/Key
   :alt   s/Bool
   :ctrl  s/Bool
   :shift s/Bool})

(def KeyMap
  (map-vals (constantly KeyBinding) UserKeyMap))

(def Highlighting
  {st/SyntaxElement st/RGBColour})

(def Syntax
  {:standard  Highlighting
   :selection Highlighting
   :clean-up  Highlighting})

(def Terminal
  {st/font-path                st/FontPath
   st/font-size                st/FontSize
   (s/optional-key st/palette) st/Palette})

(def Config
  {:keymap   KeyMap
   :syntax   Syntax
   :terminal Terminal})