(ns omnia.config
  (require [omnia.input :refer [->Event]]
           [omnia.more :refer [map-vals]]
           [clojure.string :refer [join]]
           [halfling.result :refer [success failure]]
           [omnia.highlight :refer [default-colourscheme no-colourscheme]]
           [clojure.core.match :as m]
           [clojure.set :refer [map-invert]]
           [clojure.java.io :as io]
           [clojure.edn :as edn]
           [halfling.task :as tsk]))

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
  {:highlight   {:key \p :ctrl true}
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
   colourscheme default-colourscheme})

(defn failed [msg cause]
  (throw (new Exception (str msg "\n" cause))))

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

(defn normalise [config]
  (map-vals
    #(merge {:key   :none
             :ctrl  false
             :shift false
             :alt   false} %) config))

(defn with-features [config]
  (cond-> config
          (not (get config highlighting)) (assoc colourscheme no-colourscheme)
          (not (get config scrolling)) (update keymap #(dissoc % :scroll-up :scroll-down))
          (not (get config suggestions)) (update keymap #(dissoc % :suggest))
          (get config highlighting) (update colourscheme #(or % default-colourscheme))
          :always (update keymap (comp map-invert normalise))))

(defn match-stroke [config stroke]
  (letfn [(char-key? [stroke] (char? (:key stroke)))]
    (m/match [(-> (get config keymap) (get stroke))]
             [nil :guard (constantly (char-key? stroke))] (->Event :char (:key stroke))
             [nil] (->Event :none (:key stroke))
             [action] (->Event action (:key stroke)))))


(defn read-config [path]
  (tsk/task
    (if (-> path (io/file) (.exists))
      (-> path (slurp) (edn/read-string) (validate))
      default-config)))