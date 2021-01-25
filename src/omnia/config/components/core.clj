(ns omnia.config.components.core
  (:require [schema.core :as s]
            [omnia.config.components.text :as t]
            [omnia.config.components.input :as i]
            [omnia.config.components.event :as e]
            [omnia.util.collection :refer [map-vals]]))

(def OS
  (s/enum :linux :mac :windows))

(def KeyBindingConfig
  {:key                    i/Key
   (s/optional-key :alt)   s/Bool
   (s/optional-key :ctrl)  s/Bool
   (s/optional-key :shift) s/Bool})

(def KeyMapConfig
  {e/docs              KeyBindingConfig
   e/signature         KeyBindingConfig
   e/expand            KeyBindingConfig
   e/undo              KeyBindingConfig
   e/redo              KeyBindingConfig
   e/paste             KeyBindingConfig
   e/copy              KeyBindingConfig
   e/cut               KeyBindingConfig
   e/select-all        KeyBindingConfig
   e/up                KeyBindingConfig
   e/down              KeyBindingConfig
   e/left              KeyBindingConfig
   e/right             KeyBindingConfig
   e/jump-left         KeyBindingConfig
   e/jump-right        KeyBindingConfig
   e/select-up         KeyBindingConfig
   e/select-down       KeyBindingConfig
   e/select-left       KeyBindingConfig
   e/select-right      KeyBindingConfig
   e/jump-select-left  KeyBindingConfig
   e/jump-select-right KeyBindingConfig
   e/backspace         KeyBindingConfig
   e/delete            KeyBindingConfig
   e/break             KeyBindingConfig
   e/match             KeyBindingConfig
   e/suggest           KeyBindingConfig
   e/scroll-up         KeyBindingConfig
   e/scroll-down       KeyBindingConfig
   e/prev-eval         KeyBindingConfig
   e/next-eval         KeyBindingConfig
   e/indent            KeyBindingConfig
   e/clear             KeyBindingConfig
   e/evaluate          KeyBindingConfig
   e/exit              KeyBindingConfig})

(def SyntaxConfig
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
   t/commas      t/Colour
   t/selections  t/Colour
   t/backgrounds t/Colour})

(def TerminalConfig
  {(s/optional-key t/font)      t/Font
   (s/optional-key t/font-size) t/FontSize
   (s/optional-key t/palette)   t/Palette})

(def KeyBinding
  {:key   i/Key
   :alt   s/Bool
   :ctrl  s/Bool
   :shift s/Bool})

(def KeyMap
  (map-vals (constantly KeyBinding) KeyMapConfig))

(def Syntax
  {:standard  SyntaxConfig
   :selection SyntaxConfig
   :clean-up  SyntaxConfig})

(def Terminal
  {t/font      t/Font
   t/font-size t/FontSize
   t/palette   t/Palette})