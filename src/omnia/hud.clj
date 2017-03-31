(ns omnia.hud
  (:gen-class)
  (use omnia.highlighting)
  (require [lanterna.terminal :as t]
           [omnia.repl :as r]
           [omnia.input :as i]
           [clojure.core.match :as m]))

(comment
  "Good start, but the hood needs to also easily be manipulated directly"
  "For example adding pre-existing text to the history or starting characters to input lines
  and so on.
  This can all most probably be done through [x y] offsets but I have to test it out.
  I believe that a Hud record might also ease some of these details")

(defn print!
  ([terminal seeker] (print! terminal seeker 0))
  ([terminal seeker offset]
   (doseq [[gy line] (->> seeker :lines (map-indexed vector))
           :let [y (+ gy offset)]]
     (->> line
          (map-indexed vector)
          (reduce (fn [state [x c]]
                    (let [[next-state colour] (process state c)]
                      (doto terminal
                        (t/set-fg-color colour)
                        (t/move-cursor x y)
                        (t/put-character c))
                      next-state)) s0)))))

(defn reprint! [terminal history]
  (t/clear terminal)
  (reduce
    (fn [acc-off [seeker off]]
      (doto terminal
        (print! seeker acc-off)
        (t/move-cursor 0 (dec (+ acc-off off))))
      (+ acc-off off)) 0 history))

(defn update! [offset terminal seeker]
  (let [[x y] (:cursor seeker)]
    (print! terminal seeker offset)
    (t/move-cursor terminal x (+ y offset))))

(defn shutdown [terminal]
  (t/stop terminal)
  (System/exit 1))

(defn read-eval-print [terminal port]
  (loop [history []
         repl (r/repl "localhost" port)
         seeker i/empty-seeker]
    (-> terminal
        (reprint! history)
        (update! terminal seeker))
    (let [stroke (t/get-keystroke-blocking terminal)]
      (m/match [stroke]
               [{:key \d :ctrl true}] (shutdown terminal)

               [{:key :up :alt true}] (let [x (r/travel-back repl)]
                                        (recur history x (r/now x)))

               [{:key :down :alt true}] (let [x (r/travel-forward repl)]
                                          (recur history x (r/now x)))

               [{:key \e :alt true}] (let [evaled (r/evaluate repl seeker)]
                                       (recur (conj history
                                                    [seeker (i/height seeker)]
                                                    [(r/result evaled) (i/height (r/result evaled))])
                                              evaled
                                              i/empty-seeker))
               :else (recur history repl (i/inputs seeker stroke))))))