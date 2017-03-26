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
    (s/clear)
    (print-colour! seeker)
    (move! seeker)
    (s/redraw)))

;; ctrl,shift, alt + enter doesn't work still
;; ctrl, alt + backspace doesn't work either
(defn reads
  ([screen seeker]
   (reads screen identity seeker))
  ([screen repl-eval seeker]
   (update-screen! screen seeker)
   (let [stroke (s/get-keystroke-blocking screen)]
     (m/match [stroke]
              [{:key \d :ctrl true}] (doto screen
                                       (print-colour! (bye seeker)) ;; move the cursor also at the end of the lines
                                       (s/redraw)
                                       (sleep 500)
                                       (s/stop))
              [{:key \e :alt true}] (recur screen repl-eval (repl-eval seeker))
              :else (recur screen repl-eval (p/inputs seeker stroke))))))

(defn start-terminal
  ([ttype]
   (let [screen (s/get-screen ttype)
         _ (s/start screen)]
     (reads screen i/empty-seeker)))
  ([ttype port]
   (start-terminal ttype "localhost" port))
  ([ttype host port]
   (let [screen (s/get-screen ttype)
         evaluator (r/evaluator host port)
         _ (s/start screen)]
     (reads screen evaluator i/empty-seeker))))

(defn -main [& args]
  (start-terminal :text))
