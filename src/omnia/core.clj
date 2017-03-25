(ns omnia.core
  (:gen-class)
  (use [omnia.syntax])
  (require [lanterna.screen :as s]
           [omnia.text :as p]
           [clojure.core.match :as m]
           [lanterna.constants :as const]))

(defn print! [screen seeker]
  (dorun
    (map-indexed
      (fn [idx line]
        (s/put-string screen (apply str line) 0 idx)) (:lines seeker))))

(defn print-colour! [screen seeker]
  (let [indexed (map-indexed vector (:lines seeker))]
    (doseq [[y line] indexed]
      (reduce (fn [state [x c]]
                (let [[next-state colour] (process state c)]
                  (s/put-character screen c x y {:fg colour})
                  next-state)) s0 (map-indexed vector line)))))

(defn move! [screen seeker]
  (let [[x y] (:cursor seeker)]
    (s/move-cursor screen x y)))

(defn bye [seeker]
  (reduce p/simple-insert (-> seeker p/end-x p/break) [\B \y \e \!]))

(defn sleep [screen ms]
  (Thread/sleep ms)
  screen)

(defn reads [screen seeker]
  (doto screen
    (s/clear)
    (print-colour! seeker)
    (move! seeker)
    (s/redraw))
  (let [stroke (s/get-keystroke-blocking screen)]
    (m/match [stroke]
             [{:key \d :ctrl true}] (doto screen
                                      (print! (bye seeker)) ;; move the cursor also at the end of the lines
                                      (s/redraw)
                                      (sleep 500)
                                      (s/stop))
             :else (recur screen (p/inputs seeker stroke)))))

(defn -main [& args]
  (let [screen (s/get-screen :text)
        _ (s/start screen)]
    (reads screen p/empty-seeker)))
