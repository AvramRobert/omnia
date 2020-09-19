(ns omnia.config
  (:require [omnia.more :refer [map-vals gulp-or-else]]
            [clojure.string :refer [join]]
            [clojure.set :refer [map-invert]]
            [halfling.task :as t]
            [omnia.highlight :as h]
            [schema.core :as c]))

(def ^:const macOS :macOS)
(def ^:const linux :linux)
(def ^:const os :os)
(def ^:const keymap :keymap)
(def ^:const colourscheme :colourscheme)



(def ^:private linux-keymap
  {
   ;; --- EDITOR KEYMAP ---
   :docs              {:key \i :alt true}
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
   :match       {:key \p :ctrl true}
   :suggest     {:key :tab}
   :scroll-up   {:key :page-up}
   :scroll-down {:key :page-down}
   :prev-eval   {:key :up :alt true}
   :next-eval   {:key :down :alt true}
   :format      {:key \l :ctrl true :alt true}
   :clear       {:key \r :ctrl true}
   :eval        {:key \e :alt true}
   :exit        {:key \d :ctrl true}})

(def ^:private macOS-keymap
  {
   ; --- EDITOR KEYMAP ---
   :docs              {:key \i :alt true}
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
   ; --- HUD KEYMAP ---
   :match       {:key \p :ctrl true}
   :suggest     {:key :tab}
   :scroll-up   {:key :page-up}
   :scroll-down {:key :page-down}
   :prev-eval   {:key :up :alt true}
   :next-eval   {:key :down :alt true}
   :format      {:key \l :ctrl true :alt true}
   :clear       {:key \r :ctrl true}
   :eval        {:key \e :alt true}
   :exit        {:key \d :ctrl true}})

(def ^:private standard-cs
  {h/-list     :white
   h/-vector   :white
   h/-map      :white
   h/-number   :blue
   h/-char     :green
   h/-string   :green
   h/-keyword  :cyan
   h/-comment  :magenta
   h/-word     :yellow
   h/-function :yellow
   h/-text     :white
   h/-select   :blue})

(def ^:private control-cs
  {h/-space :default
   h/-back  :default
   h/-break :default})

(defn keymap-for [os]
  (if (= os macOS) macOS-keymap linux-keymap))

(defn scheme-from [cs]
  (-> cs (merge control-cs) (assoc h/-string* (cs h/-string :green))))

(defn config-for [os-key]
  {keymap  (keymap-for os-key)
   colourscheme standard-cs})

(def default-keymap (keymap-for linux))
(def default-cs     (scheme-from standard-cs))
(def default-config (config-for linux))

(defn- failed [msg cause]
  (throw (Exception. (str msg "\n" cause))))

(defn validate [keymap]
  (letfn [(report! [errs]
            (if (empty? errs)
              keymap
              (failed "Duplicate bindings in keymap:" (join "\n" errs))))]
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

(defn normalise [config]
  {:key   (:key config)
   :ctrl  (:ctrl config false)
   :alt   (:alt config false)
   :shift (:shift config false)})

(defn keep-left [a b]
  (if a a b))

(defn- prune-keymap [provided-keymap]
  (->> default-keymap
       (merge-with keep-left provided-keymap)
       (map-vals normalise)
       (map-invert)))

(defn- prune-palette [provided-palette]
  (->> default-cs
       (merge-with keep-left provided-palette)))

(defn convert [config]
  {:os           :linux
   :keymap       (-> config (:keymap) (prune-keymap))
   :colourscheme (-> config (:colourscheme) (prune-palette))})

(defn read-config [path]
  (t/do-tasks
    [config (gulp-or-else path default-config)
     _      (validate (:keymap config))
     result (convert config)]
    result))