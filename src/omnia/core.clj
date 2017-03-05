(ns omnia.core
  (:gen-class)
  (require [lanterna.screen :as s]
           [omnia.processing :as t]))
(comment
  "Laterna is the one")

(defn print! [screen seeker]
  (dorun
    (map-indexed
      (fn [idx line]
        (s/put-string screen 0 idx (apply str line))) (:lines seeker))))

(defn move! [screen seeker]
  (let [[x y] (:cursor seeker)]
    (s/move-cursor screen x y)))

(defn reads [screen seeker]
  (doto screen
    (s/clear)
    (print! seeker)
    (move! seeker)
    (s/redraw))
  (if-let [k (s/get-key screen)]
    (case k
      :escape (s/stop screen)
      (recur screen (t/inputs seeker k)))
    (recur screen seeker)))

(defn -main [& args]
  (let [terminal (s/get-screen :text)
        _ (s/start terminal)]
    (reads terminal t/empty-seeker)))
