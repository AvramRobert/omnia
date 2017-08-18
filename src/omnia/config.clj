(ns omnia.config
  (require [omnia.more :refer [map-vals gulp-or-else]]
           [clojure.string :refer [join]]
           [omnia.highlight :refer [default-cs default-selection-cs]]
           [clojure.set :refer [map-invert]]
           [halfling.task :refer [task]]))

(def ^:const highlighting :syntax-highlighting)
(def ^:const scrolling :scrolling)
(def ^:const suggestions :suggestions)
(def ^:const keymap :keymap)
(def ^:const colourscheme :colourscheme)

(def editor-keymap
  {:expand            {:key \w :ctrl true}
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
   :enter             {:key :enter}})

(def hud-keymap
  {:match       {:key \p :ctrl true}
   :suggest     {:key :tab}
   :scroll-up   {:key :page-up}
   :scroll-down {:key :page-down}
   :prev-eval   {:key :up :alt true}
   :next-eval   {:key :down :alt true}
   :reformat    {:key \l :ctrl true :alt true}
   :clear       {:key \r :ctrl true}
   :eval        {:key \e :alt true}
   :exit        {:key \d :ctrl true}})

(def default-keymap (merge editor-keymap hud-keymap))

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
      (update colourscheme (partial patch-with default-cs))
      (update keymap (partial patch-with default-keymap))
      (update highlighting some?)
      (update suggestions some?)
      (update scrolling some?)))

(defn read-config [path]
  (task
    (-> path
        (gulp-or-else default-config)
        (patch)
        (validate))))

(defn with-features [config]
  (cond-> config
          (not (get config highlighting)) (assoc colourscheme default-selection-cs)
          (not (get config scrolling)) (update keymap #(dissoc % :scroll-up :scroll-down))
          (not (get config suggestions)) (update keymap #(dissoc % :suggest))
          :always (update keymap (comp map-invert normalise))))