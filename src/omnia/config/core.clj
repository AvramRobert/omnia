(ns omnia.config.core
  (:require [schema.core :as s]
            [omnia.util.collection :refer [map-vals merge-from-both]]
            [omnia.util.misc :refer [slurp-or-else]]
            [clojure.string :refer [join]]
            [omnia.config.defaults :as d]
            [omnia.schema.config :as c]
            [omnia.schema.syntax :as t]
            [omnia.schema.syntax :as st]
            [omnia.schema.eval-history :as eh]))

(s/defn check-duplicates! :- nil
  [keymap :- c/UserKeyMap]
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
  (let [select-colour (get standard t/selections)]
    (-> (get standard t/texts)
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
  [app-path          :- s/Str
   provided-terminal :- c/UserTerminal]
  (let [default-font-path (str app-path "/" d/default-font-file-name)
        default-font-size (get d/default-user-terminal st/font-size)
        default-palette   (get d/default-user-terminal st/palette)]
    {st/font-path (get provided-terminal st/font-path default-font-path)
     st/font-size (get provided-terminal st/font-size default-font-size)
     st/palette   (get provided-terminal st/palette default-palette)}))

(s/defn fix-persistence :- c/Persistence
  [app-path :- s/Str
   provided-persistence :- c/UserPersistence]
  (let [default-history-file-path (str app-path "/" d/default-history-file-name)
        default-history-size      (get d/default-user-persistence eh/history-size)]
    {eh/history-file-path (get provided-persistence eh/history-file-path default-history-file-path)
     eh/history-size      (get provided-persistence eh/history-size default-history-size)}))

(defn validate! [config]
  (s/validate c/UserKeyMap (:keymap config))
  (s/validate c/UserHighlighting (:syntax config))
  (s/validate c/UserTerminal (:terminal config))
  (s/validate c/UserPersistence (:persistence config))
  (check-duplicates! (:keymap config)))

(s/defn convert :- c/Config
  [app-path :- s/Str
   config   :- c/UserConfig]
  {:keymap      (->> config (:keymap) (fix-keymap))
   :syntax      (->> config (:syntax) (fix-highlighting) (create-syntax))
   :terminal    (->> config (:terminal) (fix-terminal app-path))
   :persistence (->> config (:persistence) (fix-persistence app-path))})

(s/defn read-user-config! :- c/UserConfig
  [app-path :- String]
  (let [config-path (str app-path "/" d/default-config-file-name)
        config      (slurp-or-else config-path d/default-user-config)
        _           (validate! config)]
    config))

(s/defn read-config! :- c/Config
  [app-path :- String]
  (->> app-path
       (read-user-config!)
       (convert app-path)))
