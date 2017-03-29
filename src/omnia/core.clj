(ns omnia.core
  (:gen-class)
  (use [omnia.highlighting])
  (require [lanterna.screen :as s]
           [omnia.input :as p]
           [omnia.repl :as r]
           [clojure.core.match :as m]
           [lanterna.constants :as const]
           [fipp.clojure :as f]
           [omnia.input :as i]))


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

(defn update-screen! [screen seeker]
  (doto screen
    ;(s/clear)
    (print-colour! seeker)
    (move! seeker)
    (s/redraw)))

(defn shutdown [screen seeker]
  (do
    (doto screen
      (print-colour! (bye seeker)) ;; move the cursor at the end of all lines and print
      (s/redraw)
      (sleep 500)
      (s/stop))
    (System/exit 1)))

;; ctrl,shift, alt + enter still don't work in :text
;; ctrl, alt + backspace don't work in :text either
;; ctrl up and down also don't work in :text
(defn reads [screen repl seeker]
  (update-screen! screen seeker)
  (let [stroke (s/get-keystroke-blocking screen)]
    (m/match [stroke]
             [{:key \d :ctrl true}] (shutdown screen seeker)

             [{:key :up :alt true}] (let [x (r/travel-back repl)]
                                      (recur screen x (r/now x)))

             [{:key :down :alt true}] (let [x (r/travel-forward repl)]
                                        (recur screen x (r/now x)))

             [{:key \e :alt true}] (let [evaled (r/evaluate repl seeker)]
                                     (recur screen evaled (r/result evaled)))
             :else (recur screen repl (p/inputs seeker stroke)))))

(defn start-terminal                                        ;; I don't really like this
  ([ttype]
   (start-terminal ttype nil nil :identity))
  ([ttype port]
   (start-terminal ttype "localhost" port))
  ([ttype host port]
   (start-terminal ttype host port :nrepl))
  ([ttype host port repl-type]
   (let [screen (s/get-screen ttype)
         repl (r/repl host port repl-type)
         _ (s/start screen)]
     (reads screen repl (:result repl)))))

(defn -main [& args]
  (start-terminal :text))
