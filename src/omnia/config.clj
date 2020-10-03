(ns omnia.config
  (:require [omnia.more :refer [map-vals gulp-or-else]]
            [clojure.string :refer [join]]
            [clojure.set :refer [map-invert]]
            [omnia.event :as e]
            [halfling.task :as t]
            [omnia.highlight :as h]
            [schema.core :as s]))

(def default
  (s/eq :default))

(def OS
  (s/enum :linux :mac :windows))

(def KeyEvent
  (s/enum e/docs e/signature e/expand e/undo e/redo
          e/paste e/copy e/cut e/select-all e/up
          e/down e/left e/right e/jump-left e/jump-right
          e/select-up e/select-down e/select-left e/select-right
          e/jump-select-left e/jump-select-right e/backspace
          e/delete e/break e/match e/suggest e/scroll-up
          e/scroll-down e/prev-eval e/next-eval e/indent
          e/clear e/evaluate e/exit))

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

(def KeyMapConfig
  {InternalKeyBinding KeyEvent})

(def KeyMap
  {KeyEvent KeyBinding})

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

(def SyntaxConfig
  {:standard  InternalSyntax
   :clean-up  InternalSyntax
   :selection InternalSyntax})

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
   :keymap   KeyMapConfig
   :syntax   SyntaxConfig
   :terminal TerminalConfig})

(s/def default-os :- OS
  :linux)

(s/def default-keymap :- KeyMap
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
   ;; --- HUD KEYMAP ---
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

(s/defn enhance-keymap [provided-keymap :- KeyMap] :- KeyMapConfig
  (->> default-keymap
       (merge-with (fn [a b] (or a b)) provided-keymap)
       (map-vals normalise-binding)
       (map-invert)))

(s/defn enhance-syntax [provided-syntax :- Syntax] :- InternalSyntax
  (->> provided-syntax
       (merge-with (fn [a b] (or a b)) default-syntax)
       (normalise-palette)))

(defn- clean-up-palette [syntax-palette]
  (assoc syntax-palette h/-select :default
                        h/-back   :default))

(defn- selection-palette [syntax-palette]
  (let [select-colour (syntax-palette h/-select)]
    (-> (syntax-palette h/-text)
        (constantly)
        (map-vals syntax-palette)
        (assoc h/-back   select-colour)
        (assoc h/-select select-colour))))

(s/defn convert [config :- Config] :- InternalConfig
  (let [syntax   (-> config (:syntax) (enhance-syntax))
        select   (selection-palette syntax)
        clean-up (clean-up-palette syntax)]
    {:os      (config :os default-os)
     :keymap  (-> config (:keymap) (enhance-keymap))
     :syntax  {:standard syntax
               :clean-up clean-up
               :selection select}
     :terminal (config :terminal default-terminal-config)}))

(s/defn read-config [path :- String]
  (t/do-tasks
    [config (gulp-or-else path default-config)
     _      (validate! config)
     result (convert config)]
    result))