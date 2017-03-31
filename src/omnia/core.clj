(ns omnia.core
  (:gen-class)
  (use [omnia.highlighting])
  (require [lanterna.screen :as s]
           [omnia.input :as p]
           [omnia.repl :as r]
           [clojure.core.match :as m]
           [lanterna.constants :as const]
           [fipp.clojure :as f]
           [omnia.input :as i]
           [lanterna.terminal :as t]
           [omnia.hud :as hud]))

(defn move!
  ([screen x y]
   (t/move-cursor screen x y))
  ([screen seeker]
   (let [[x y] (:cursor seeker)]
     (t/move-cursor screen x y))))

(defn print-colour! [screen seeker]
  (let [indexed (map-indexed vector (:lines seeker))]
    (doseq [[y line] indexed]
      (reduce (fn [state [x c]]
                (let [[next-state colour] (process state c)]
                  (doto screen
                    (t/set-fg-color colour)
                    (move! x y)
                    (t/put-character c))
                  next-state)) s0 (map-indexed vector line)))))

(defn bye [seeker]
  (reduce p/simple-insert (-> seeker p/end-x p/break) [\B \y \e \!]))

(defn sleep [screen ms]
  (Thread/sleep ms)
  screen)

(defn update-screen! [screen seeker]
  (doto screen
    ;(s/clear)
    (t/clear)
    (print-colour! seeker)
    (move! seeker)
    ;(s/redraw)
    ))

(defn shutdown [screen seeker]
  (do
    (doto screen
      ;(print-colour! (bye seeker)) ;; move the cursor at the end of all lines and print
      ;(t/redraw)
      ;(sleep 500)
      (t/stop))
    (System/exit 1)))

;; ctrl,shift, alt + enter still don't work in :text
;; ctrl, alt + backspace don't work in :text either
;; ctrl up and down also don't work in :text
(defn reads [screen repl seeker]
  (update-screen! screen seeker)
  (let [stroke (t/get-keystroke-blocking screen)]
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
  ([kind]
   (start-terminal kind nil nil :identity))
  ([kind port]
   (start-terminal kind "localhost" port))
  ([kind host port]
   (start-terminal kind host port :nrepl))
  ([kind host port repl-type]
   (let [screen (t/get-terminal kind)                       ;(s/get-screen kind)
         repl (r/repl host port repl-type)
         _ (t/start screen)]
     (reads screen repl (:result repl)))))

(defn -main [& args]
  #_(start-terminal :text 35575)

  (let [terminal (t/get-terminal :text)
        _ (t/start terminal)]
    (hud/read-eval-print terminal 35575)))
