(ns omnia.config
  (require [omnia.input :as i]
           [clojure.core.match :as m]))

(defn map-keys [m f]
  (reduce (fn [nm [k v]]
            (assoc nm (f k) v)) {} m))

(def editor-keymap
  {{:key \w :ctrl true}                 :expand
   {:key \a :ctrl true}                 :select-all
   {:key \v :alt true}                  :paste
   {:key \c :alt true}                  :copy
   {:key \x :alt true}                  :cut
   {:key :up}                           :up
   {:key :down}                         :down
   {:key :left}                         :left
   {:key :right}                        :right
   {:key :left :ctrl true}              :jump-left
   {:key :right :ctrl true}             :jump-right
   {:key :up :shift true}               :select-up
   {:key :down :shift true}             :select-down
   {:key :left :shift true}             :select-left
   {:key :right :shift true}            :select-right
   {:key :left :shift true :ctrl true}  :jump-select-left
   {:key :right :shift true :ctrl true} :jump-select-right
   {:key :backspace}                    :backspace
   {:key :delete}                       :delete
   {:key :enter}                        :enter})

(def hud-keymap
  {{:key \p :ctrl true}           :hightlight
   {:key :tab}                    :suggest
   {:key :page-up}                :scroll-up
   {:key :page-down}              :scroll-down
   {:key :up :alt true}           :prev-eval
   {:key :down :alt true}         :next-eval
   {:key \l :ctrl true :alt true} :reformat
   {:key \r :ctrl true}           :clear
   {:key \e :alt true}            :eval
   {:key \d :ctrl true}           :exit})

(def default-keymap (merge editor-keymap hud-keymap))

(defn normalise [config]
  (map-keys config
            #(merge {:key   :none
                     :ctrl  false
                     :shift false
                     :alt   false} %)))

(defn match-stroke [config stroke]
  (m/match [(-> config (normalise) (get stroke))]
           [nil :guard (constantly (i/char-key? stroke))] (i/->Event :char (:key stroke))
           [nil] (i/->Event :none (:key stroke))
           [action] (i/->Event action (:key stroke))))
