(ns omnia.config
  (require [omnia.input :as i]
           [omnia.more :refer [map-vals]]
           [omnia.highlight :refer [default-colourscheme no-colourscheme]]
           [clojure.core.match :as m]
           [clojure.set :refer [map-invert]]))

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
  {:hightlight  {:key \p :ctrl true}
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
  {:syntax-highlighting true
   :scrolling           true
   :suggestions         true
   :keymap              default-keymap
   :colourscheme        default-colourscheme})

(defn normalise [config]
  (map-vals
    #(merge {:key   :none
             :ctrl  false
             :shift false
             :alt   false} %) config))

(defn with-features [config]
  (cond-> config
          (not (:syntax-highlighting config)) (assoc :colourscheme no-colourscheme)
          (not (:scrolling config)) (update :keymap #(dissoc % :scroll-up :scroll-down))
          (not (:suggestions config)) (update :keymap #(dissoc % :suggest))
          :always (update :keymap (comp map-invert normalise))))

(defn match-stroke [config stroke]
  (m/match [(-> config (:keymap) (get stroke))]
           [nil :guard (constantly (i/char-key? stroke))] (i/->Event :char (:key stroke))
           [nil] (i/->Event :none (:key stroke))
           [action] (i/->Event action (:key stroke))))
