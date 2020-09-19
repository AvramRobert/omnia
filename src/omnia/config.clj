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

(def Palette
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

(def TerminalPalette default)

(def InternalPalette
  (merge Palette {h/-string* Colour}))

(def Font
  (s/cond-pre default s/Str))

(def FontSize
  (s/cond-pre default s/Num))

(def Config
  {:os                                OS
   :keymap                            KeyMap
   :palette                           Palette
   (s/optional-key :terminal-palette) TerminalPalette
   (s/optional-key :font)             Font
   (s/optional-key :font-size)        FontSize})

(def InternalConfig
  {:os               OS
   :palette          InternalPalette
   ;:terminal-palette TerminalPalette
   :keymap           InternalKeyMap
   ;:font             Font
   ;:font-size        FontSize
   })

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

(s/def default-palette :- Palette
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

(s/def default-config :- Config
  {:os      default-os
   :keymap  default-keymap
   :palette default-palette})

(defn- validate [keymap]
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

(defn- normalise [config]
  {:key   (:key config)
   :ctrl  (:ctrl config false)
   :alt   (:alt config false)
   :shift (:shift config false)})

(defn- keep-left [a b]
  (if a a b))

(defn- prune-keymap [provided-keymap]
  (->> default-keymap
       (merge-with keep-left provided-keymap)
       (map-vals normalise)
       (map-invert)))

(defn- prune-palette [provided-palette]
  (-> keep-left
      (merge-with default-palette provided-palette)
      (assoc h/-string* (get provided-palette h/-string :green))))

(s/defn convert [config :- Config] :- InternalConfig
  {:os           (get :os config default-os)
   :keymap       (-> config (:keymap) (prune-keymap))
   :palette      (-> config (:palette) (prune-palette))})

(s/defn read-config [path :- String]
  (t/do-tasks
    [config (gulp-or-else path default-config)
     _      (validate (:keymap config))
     result (convert config)]
    result))