(ns omnia.config.defaults
  (:require [schema.core :as s]
            [omnia.util.collection :refer [map-vals]]
            [omnia.config.schema :refer :all]
            [omnia.components.actions :as a]
            [omnia.components.syntax :as t]))

(s/def default-user-keymap :- UserKeyMap
  {a/docs              {:key \i :alt true}
   a/signature         {:key \p :alt true}
   a/expand            {:key \w :ctrl true}
   a/undo              {:key \z :alt true}
   a/redo              {:key \y :alt true}
   a/paste             {:key \v :alt true}
   a/copy              {:key \c :alt true}
   a/cut               {:key \x :alt true}
   a/select-all        {:key \a :ctrl true}
   a/move-up           {:key :up}
   a/move-down         {:key :down}
   a/move-left         {:key :left}
   a/move-right        {:key :right}
   a/jump-left         {:key :left :ctrl true}
   a/jump-right        {:key :right :ctrl true}
   a/select-up         {:key :up :shift true}
   a/select-down       {:key :down :shift true}
   a/select-left       {:key :left :shift true}
   a/select-right      {:key :right :shift true}
   a/jump-select-left  {:key :left :shift true :ctrl true}
   a/jump-select-right {:key :right :shift true :ctrl true}
   a/delete-previous   {:key :backspace}
   a/delete-current    {:key :delete}
   a/new-line          {:key :enter}
   a/paren-match       {:key \p :ctrl true}
   a/suggest           {:key :tab}
   a/scroll-up         {:key :page-up}
   a/scroll-down       {:key :page-down}
   a/prev-eval         {:key :up :alt true}
   a/next-eval         {:key :down :alt true}
   a/indent            {:key \l :ctrl true :alt true}
   a/clear             {:key \r :ctrl true}
   a/evaluate          {:key \e :alt true}
   a/exit              {:key \d :ctrl true}})

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