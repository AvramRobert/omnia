(ns omnia.config.defaults
  (:require [schema.core :as s]
            [omnia.util.collection :refer [map-vals]]
            [omnia.config.components.core :refer :all]
            [omnia.config.components.event :as e]
            [omnia.config.components.text :as t]))

(s/def default-os :- OS
  :linux)

(s/def default-user-keymap :- UserKeyMap
  {e/docs              {:key \i :alt true}
   e/signature         {:key \p :alt true}
   e/expand            {:key \w :ctrl true}
   e/undo              {:key \z :alt true}
   e/redo              {:key \y :alt true}
   e/paste             {:key \v :alt true}
   e/copy              {:key \c :alt true}
   e/cut               {:key \x :alt true}
   e/select-all        {:key \a :ctrl true}
   e/up                {:key :up}
   e/down              {:key :down}
   e/left              {:key :left}
   e/right             {:key :right}
   e/jump-left         {:key :left :ctrl true}
   e/jump-right        {:key :right :ctrl true}
   e/select-up         {:key :up :shift true}
   e/select-down       {:key :down :shift true}
   e/select-left       {:key :left :shift true}
   e/select-right      {:key :right :shift true}
   e/jump-select-left  {:key :left :shift true :ctrl true}
   e/jump-select-right {:key :right :shift true :ctrl true}
   e/backspace         {:key :backspace}
   e/delete            {:key :delete}
   e/break             {:key :enter}
   e/match             {:key \p :ctrl true}
   e/suggest           {:key :tab}
   e/scroll-up         {:key :page-up}
   e/scroll-down       {:key :page-down}
   e/prev-eval         {:key :up :alt true}
   e/next-eval         {:key :down :alt true}
   e/indent            {:key \l :ctrl true :alt true}
   e/clear             {:key \r :ctrl true}
   e/evaluate          {:key \e :alt true}
   e/exit              {:key \d :ctrl true}})

(s/def default-user-highlighting :- UserHighlighting
  {t/lists       t/white
   t/vectors     t/white
   t/maps        t/white
   t/numbers     t/blue
   t/characters  t/green
   t/strings     t/green
   t/keywords    t/cyan
   t/comments    t/magenta
   t/words       t/yellow
   t/functions   t/yellow
   t/texts       t/white
   t/commas      t/white})

(s/def default-user-terminal :- UserTerminal
  {t/font-path "./default_font.otf"
   t/font-size  15})

;; The default colour should always be the background colour taken from the palette, otherwise a lot of shit may get rendered badly
(s/def default-colours :- {t/PresetColour t/RGBColour}
  {t/black   [0 0 0]
   t/white   [171 174 168]
   t/red     [170 0 0]
   t/green   [0 170 0]
   t/blue    [33 66 131]
   t/cyan    [0 170 170]
   t/magenta [170 0 170]
   t/yellow  [180 148 6]
   t/default [46 52 54]})