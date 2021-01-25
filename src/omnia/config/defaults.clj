(ns omnia.config.defaults
  (:require [schema.core :as s]
            [omnia.util.collection :refer [map-vals]]
            [omnia.config.components.core :refer :all]
            [omnia.config.components.event :as e]
            [omnia.config.components.text :as t]))

(s/def default-os :- OS
  :linux)

(s/def default-keymap :- KeyMapConfig
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

(s/def default-syntax :- SyntaxConfig
  {t/lists       :white
   t/vectors     :white
   t/maps        :white
   t/numbers     :blue
   t/characters  :green
   t/strings     :green
   t/keywords    :cyan
   t/comments    :magenta
   t/words       :yellow
   t/functions   :yellow
   t/texts       :white
   t/commas      :white
   t/selections  :blue
   t/backgrounds :default})

(s/def default-terminal :- TerminalConfig
  {:font      "Hasklig-Regular.otf"
   :font-size 15
   :palette   :default})