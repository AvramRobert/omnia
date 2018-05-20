(ns omnia.config
  (:require [omnia.more :refer [map-vals gulp-or-else]]
            [clojure.string :refer [join]]
            [clojure.set :refer [map-invert]]
            [halfling.task :refer [task]]
            [omnia.highlight :as h]))

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

(defn validate [config]
  (letfn [(report! [errs]
            (if (empty? errs)
              config
              (failed "Duplicate bindings in keymap:" (join "\n" errs))))]
    (->> (get config keymap)
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
  (map-vals
    #(merge {:key   :none
             :ctrl  false
             :shift false
             :alt   false} %) config))

(defn patch [config]
  (letfn [(patched [patchee patcher]
            (reduce
              (fn [m [k v]]
                (if (contains? m k) m (assoc m k v))) patchee patcher))
          (fix-keymap [kmap] (->> (:os config) (keymap-for) (patched kmap)))
          (fix-scheme [cs]   (->> default-cs (patched cs) (scheme-from)))]
    (-> config
        (update os #(or % linux))
        (update colourscheme fix-scheme)
        (update keymap fix-keymap))))

(defn read-config [path]
  (task
    (-> (gulp-or-else path default-config)
        (patch)
        (validate)
        (update keymap (comp map-invert normalise)))))