(ns omnia.config.core
  (:require [omnia.util.collection :refer [map-vals merge-from-both]]
            [omnia.util.misc :refer [gulp-or-else]]
            [clojure.string :refer [join]]
            [clojure.set :refer [map-invert]]
            [schema.core :as s]
            [omnia.config.defaults :as d]
            [omnia.config.components.core :as c]
            [omnia.config.components.text :as t]))

(def UserConfig
  {:os                        c/OS
   :keymap                    c/KeyMapConfig
   :syntax                    c/SyntaxConfig
   (s/optional-key :terminal) c/TerminalConfig})

(def Config
  {:os       c/OS
   :keymap   c/KeyMap
   :syntax   c/Syntax
   :terminal c/Terminal})

(s/def default-config :- UserConfig
  {:os       d/default-os
   :keymap   d/default-keymap
   :syntax   d/default-syntax
   :terminal d/default-terminal})

(s/defn check-duplicates! [keymap :- c/KeyMapConfig]
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
  (s/validate c/KeyMapConfig (:keymap config))
  (s/validate c/SyntaxConfig (:syntax config))
  (check-duplicates! (:keymap config)))

(defn- clean-up-syntax [syntax]
  (assoc syntax t/selections t/default
                t/backgrounds t/default))

(defn- selection-syntax [syntax]
  (let [select-colour (syntax t/selections)]
    (-> (syntax t/texts)
        (constantly)
        (map-vals syntax)
        (assoc t/backgrounds select-colour
               t/selections select-colour))))

(s/defn fix-syntax :- c/Syntax
  [provided-syntax :- c/SyntaxConfig]
  (let [standard  (merge-from-both provided-syntax d/default-terminal)
        selection (selection-syntax standard)
        clean-up  (clean-up-syntax standard)]
    {:standard  standard
     :selection selection
     :clean-up  clean-up}))

(s/defn fix-key-binding :- c/KeyBinding
  [binding :- c/KeyBindingConfig]
  {:key   (:key binding)
   :ctrl  (:ctrl binding false)
   :alt   (:alt binding false)
   :shift (:shift binding false)})

(s/defn fix-keymap :- c/KeyMap
  [provided-keymap :- c/KeyMapConfig]
  (->> (merge-from-both provided-keymap d/default-keymap)
       (map-vals fix-key-binding)
       (map-invert)))

(s/defn fix-terminal :- c/Terminal
  [provided-terminal :- c/TerminalConfig]
  (merge-from-both provided-terminal d/default-terminal))

(s/defn convert :- Config
  [config :- UserConfig]
  {:os       (:os config d/default-os)                      ;; I should statically write a field with the os upon release
   :keymap   (-> config (:keymap) (fix-keymap))
   :syntax   (-> config (:syntax) (fix-syntax))
   :terminal (-> config (:terminal) (fix-terminal))})

(s/defn read-config :- UserConfig
  [path :- String]
  (let [config (gulp-or-else path default-config)
        _      (validate! config)]
    (convert config)))