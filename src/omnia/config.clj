(ns omnia.config
  (:require [omnia.more :refer [map-vals gulp-or-else]]
            [clojure.string :refer [join]]
            [clojure.set :refer [map-invert]]
            [halfling.task :refer [task]]
            [omnia.highlight :as h]
            [omnia.render :as r]))

(def ^:const highlighting :syntax-highlighting)
(def ^:const scrolling :scrolling)
(def ^:const suggestions :suggestions)
(def ^:const keymap :keymap)
(def ^:const colourscheme :colourscheme)

(def editor-keymap
  {:docs {:key \i :alt true}
   :signature {:key \p :alt true}
   :expand {:key \w :ctrl true}
   :paste {:key \v :alt true}
   :copy {:key \c :alt true}
   :cut {:key \x :alt true}
   :select-all {:key \a :ctrl true}
   :up {:key :up}
   :down {:key :down}
   :left {:key :left}
   :right {:key :right}
   :jump-left {:key :left :ctrl true}
   :jump-right {:key :right :ctrl true}
   :select-up {:key :up :shift true}
   :select-down {:key :down :shift true}
   :select-left {:key :left :shift true}
   :select-right {:key :right :shift true}
   :jump-select-left {:key :left :shift true :ctrl true}
   :jump-select-right {:key :right :shift true :ctrl true}
   :backspace {:key :backspace}
   :delete {:key :delete}
   :newline {:key :enter}})

(def hud-keymap
  {:match       {:key \p :ctrl true}
   :suggest     {:key :tab}
   :scroll-up   {:key :page-up}
   :scroll-down {:key :page-down}
   :prev-eval   {:key :up :alt true}
   :next-eval   {:key :down :alt true}
   :format      {:key \l :ctrl true :alt true}
   :clear       {:key \r :ctrl true}
   :eval        {:key \e :alt true}
   :exit        {:key \d :ctrl true}})

(def syntax-cs
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
   h/-select   :blue
   h/-back     :default})

(def control-cs
  {h/-break :default
   h/-space :default})

(defn ext-cs [cs]
  {h/-string* (cs h/-string :green)})

(defn- make-cs [cs]
  (merge cs (ext-cs cs)))

(def default-keymap (merge editor-keymap hud-keymap))
(def default-cs (make-cs (merge syntax-cs control-cs)))

(def default-config
  {highlighting true
   scrolling    true
   suggestions  true
   keymap       default-keymap
   colourscheme default-cs})

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
                     (clojure.string/join "," (map first actions))
                     (-> actions first second))))

         (report!))))

(defn- normalise [config]
  (map-vals
    #(merge {:key   :none
             :ctrl  false
             :shift false
             :alt   false} %) config))

(defn- patch-with [patcher patchee]
  (reduce
    (fn [c [k v]]
      (if (contains? c k) c (assoc c k v)))
    patchee patcher))

;; Let missing unspecified keys be turned off by default
(defn patch [config]
  (-> config
      (update colourscheme #(->> % (patch-with default-cs) (make-cs)))
      (update keymap (partial patch-with default-keymap))
      (update highlighting some?)
      (update suggestions some?)
      (update scrolling some?)))

(defn read-config [path]
  (task
    (-> (gulp-or-else path default-config)
        (patch)
        (validate))))

(defn with-features [config]
  (cond-> config
          (not (get config highlighting)) (update colourscheme r/select-cs)
          (not (get config scrolling)) (update keymap #(dissoc % :scroll-up :scroll-down))
          (not (get config suggestions)) (update keymap #(dissoc % :suggest))
          :always (update keymap (comp map-invert normalise))))