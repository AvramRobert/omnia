(ns omnia.config.core
  (:require [omnia.util.collection :refer [map-vals merge-from-both]]
            [omnia.util.misc :refer [gulp-or-else]]
            [clojure.string :refer [join]]
            [schema.core :as s]
            [omnia.config.defaults :as d]
            [omnia.config.components.core :as c]
            [omnia.config.components.text :as t]))

(def UserConfig
  {:os                        c/OS
   :keymap                    c/UserKeyMap
   :syntax                    c/UserHighlighting
   (s/optional-key :terminal) c/UserTerminal})

(def Config
  {:os       c/OS
   :keymap   c/KeyMap
   :syntax   c/Syntax
   :terminal c/Terminal})

(s/defn check-duplicates! [keymap :- c/UserKeyMap]
  (letfn [(report! [errs]
            (if (empty? errs)
              keymap
              (throw (Exception. (str "Duplicate bindings in keymap:" (join "\n" errs))))))]
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
  (s/validate c/UserKeyMap (:keymap config))
  (s/validate c/UserHighlighting (:syntax config))
  (check-duplicates! (:keymap config)))

(s/defn make-rgb :- t/RGBColour
  [colour :- t/Colour]
  (let [default-colour (get d/default-colours t/default)]
    (if (keyword? colour)
      (get d/default-colours colour default-colour)
      colour)))

(s/defn clean-up-highlighting :- c/Highlighting
  [standard :- c/Highlighting]
  (let [default-colour (get d/default-colours t/default)]
    (assoc standard t/selections default-colour
                    t/backgrounds default-colour)))

(s/defn selection-highlighting :- c/Highlighting
  [standard :- c/Highlighting]
  (let [select-colour (standard t/selections)]
    (-> (standard t/texts)
        (constantly)
        (map-vals standard)
        (assoc t/backgrounds select-colour
               t/selections select-colour))))

(s/defn fix-highlighting :- c/Highlighting
  [provided-highlighting :- c/UserHighlighting]
  (->> (merge-from-both provided-highlighting d/default-user-highlighting)
       (merge {t/selections  t/blue
               t/backgrounds t/default
               t/foregrounds t/default})
       (map-vals make-rgb)))

(s/defn create-syntax :- c/Syntax
  [highlighting :- c/Highlighting]
  (let [selection (selection-highlighting highlighting)
        clean-up  (clean-up-highlighting highlighting)]
    {:standard  highlighting
     :selection selection
     :clean-up  clean-up}))

(s/defn fix-key-binding :- c/KeyBinding
  [binding :- c/UserKeyBinding]
  {:key   (:key binding)
   :ctrl  (:ctrl binding false)
   :alt   (:alt binding false)
   :shift (:shift binding false)})

(s/defn fix-keymap :- c/KeyMap
  [provided-keymap :- c/UserKeyMap]
  (->> (merge-from-both provided-keymap d/default-user-keymap)
       (map-vals fix-key-binding)))

(s/defn fix-terminal :- c/Terminal
  [provided-terminal :- c/UserTerminal]
  (merge-from-both provided-terminal d/default-user-terminal))

(s/defn convert :- Config
  [config :- UserConfig]
  {:os       (:os config d/default-os)                      ;; I should statically write a field with the os upon release
   :keymap   (-> config (:keymap) (fix-keymap))
   :syntax   (-> config (:syntax) (fix-highlighting) (create-syntax))
   :terminal (-> config (:terminal) (fix-terminal))})

(s/def default-user-config :- UserConfig
  {:os       d/default-os
   :keymap   d/default-user-keymap
   :syntax   d/default-user-highlighting
   :terminal d/default-user-terminal})

(s/def default-config :- Config
  (convert default-user-config))

(s/defn read-config :- UserConfig
  [path :- String]
  (let [config (gulp-or-else path default-user-config)
        _      (validate! config)]
    (convert config)))