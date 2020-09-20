(ns omnia.config
  (:require [omnia.more :refer [map-vals gulp-or-else]]
            [clojure.string :refer [join]]
            [clojure.set :refer [map-invert]]
            [halfling.task :as t]
            [omnia.highlight :as h]
            [schema.core :as s]))

(def default
  (s/eq :default))

(def OS
  (s/enum :linux :mac :windows))

(def KeyAction
  (s/enum :docs :signature :expand :undo :redo
          :paste :copy :cut :select-all :up
          :down :left :right :jump-left :jump-right
          :select-up :select-down :select-left :select-right
          :jump-select-left :jump-select-right :backspace
          :delete :newline :match :suggest :scroll-up
          :scroll-down :prev-eval :next-eval :format
          :clear :eval :exit))

(def ControlKey
  (s/enum :up :down :left :right :delete :enter :backspace :tab :page-up :page-down))

(def CharKey
  (s/pred char?))

(def Key
  (s/cond-pre CharKey ControlKey))

(def KeyBinding
  {:key                    Key
   (s/optional-key :alt)   s/Bool
   (s/optional-key :ctrl)  s/Bool
   (s/optional-key :shift) s/Bool})

(def InternalKeyBinding
  {:key   Key
   :alt   s/Bool
   :ctrl  s/Bool
   :shift s/Bool})

(def InternalKeyMap
  {InternalKeyBinding KeyAction})

(def KeyMap
  {KeyAction KeyBinding})

(def PredefinedColour
  (s/enum :black :white :red :green :blue :cyan :magenta :yellow))

(def CustomColour
  (s/constrained [(s/pred #(< % 255))] #(= 3 (count %))))

(def Colour
  (s/cond-pre default PredefinedColour CustomColour))

(def Syntax
  {h/-list       Colour
   h/-vector     Colour
   h/-map        Colour
   h/-number     Colour
   h/-char       Colour
   h/-string     Colour
   h/-keyword    Colour
   h/-comment    Colour
   h/-word       Colour
   h/-function   Colour
   h/-text       Colour
   h/-select     Colour
   h/-space      Colour
   h/-break      Colour
   h/-back       Colour})

(def InternalSyntax
  (merge Syntax {h/-string* Colour}))

(def Palette default)

(def Font
  (s/cond-pre default s/Str))

(def FontSize
  (s/cond-pre default s/Num))

(def TerminalConfig
  {:font      Font
   :font-size FontSize
   :palette   Palette})

(def Config
  {:os                        OS
   :keymap                    KeyMap
   :syntax                    Syntax
   (s/optional-key :terminal) TerminalConfig})

(def InternalConfig
  {:os       OS
   :keymap   InternalKeyMap
   :syntax   InternalSyntax
   :terminal TerminalConfig})

(s/def default-os :- OS
  :linux)

(s/def default-keymap :- KeyMap
  {:docs              {:key \i :alt true}
   :signature         {:key \p :alt true}
   :expand            {:key \w :ctrl true}
   :undo              {:key \z :alt true}
   :redo              {:key \y :alt true}
   :paste             {:key \v :alt true}
   :copy              {:key \c :alt true}
   :cut               {:key \x :alt true}
   :select-all        {:key \a :ctrl true}
   :up                {:key :up}
   :down              {:key :down}
   :left              {:key :left}
   :right             {:key :right}
   :jump-left         {:key :left :ctrl true}
   :jump-right        {:key :right :ctrl true}
   :select-up         {:key :up :shift true}
   :select-down       {:key :down :shift true}
   :select-left       {:key :left :shift true}
   :select-right      {:key :right :shift true}
   :jump-select-left  {:key :left :shift true :ctrl true}
   :jump-select-right {:key :right :shift true :ctrl true}
   :backspace         {:key :backspace}
   :delete            {:key :delete}
   :newline           {:key :enter}
   ;; --- HUD KEYMAP ---
   :match             {:key \p :ctrl true}
   :suggest           {:key :tab}
   :scroll-up         {:key :page-up}
   :scroll-down       {:key :page-down}
   :prev-eval         {:key :up :alt true}
   :next-eval         {:key :down :alt true}
   :format            {:key \l :ctrl true :alt true}
   :clear             {:key \r :ctrl true}
   :eval              {:key \e :alt true}
   :exit              {:key \d :ctrl true}})

(s/def default-syntax :- Syntax
  {h/-list       :white
   h/-vector     :white
   h/-map        :white
   h/-number     :blue
   h/-char       :green
   h/-string     :green
   h/-keyword    :cyan
   h/-comment    :magenta
   h/-word       :yellow
   h/-function   :yellow
   h/-text       :white
   h/-select     :blue
   h/-space      :default
   h/-back       :default
   h/-break      :default})

(s/def default-terminal-config :- TerminalConfig
  {:font      :default
   :font-size :default
   :palette   :default})

(s/def default-config :- Config
  {:os     default-os
   :keymap default-keymap
   :syntax default-syntax})

(s/defn check-duplicates! [keymap :- KeyMap]
  (letfn [(report! [errs]
            (if (empty? errs)
              (t/success keymap)
              (t/failure (str "Duplicate bindings in keymap:" (join "\n" errs)))))]
    (->> keymap
         (group-by val)
         (vals)
         (filter #(> (count %) 1))
         (mapv
           (fn [actions]
             (format "Actions %s share the same key binding %s"
                     (join "," (map first actions))
                     (-> actions first second))))
         (report!))))

(defn validate! [config]
  (s/validate KeyMap (:keymap config))
  (s/validate Syntax (:syntax config))
  (check-duplicates! (:keymap config)))

(s/defn normalise-binding [binding :- KeyBinding] :- InternalKeyBinding
  {:key   (:key binding)
   :ctrl  (:ctrl binding false)
   :alt   (:alt binding false)
   :shift (:shift binding false)})

(s/defn normalise-palette [palette :- Syntax] :- InternalSyntax
  (assoc palette h/-string* (palette h/-string :green)))

(s/defn enhance-keymap [provided-keymap :- KeyMap] :- InternalKeyMap
  (->> default-keymap
       (merge-with (fn [a b] (or a b)) provided-keymap)
       (map-vals normalise-binding)
       (map-invert)))

(s/defn enhance-syntax [provided-syntax :- Syntax] :- InternalSyntax
  (->> provided-syntax
       (merge-with (fn [a b] (or a b)) default-syntax)
       (normalise-palette)))

(s/defn convert [config :- Config] :- InternalConfig
  {:os           (config :os default-os)
   :keymap       (-> config (:keymap) (enhance-keymap))
   :syntax       (-> config (:palette) (enhance-syntax))
   :terminal     (config :terminal default-terminal-config)})

(s/defn read-config [path :- String]
  (t/do-tasks
    [config (gulp-or-else path default-config)
     _      (validate! config)
     result (convert config)]
    result))