(ns omnia.hud
  (:gen-class)
  (use omnia.highlighting)
  (require [lanterna.terminal :as t]
           [omnia.repl :as r]
           [omnia.input :as i]
           [clojure.core.match :as m]))

(comment
  "The terminal size limitation is a good thing.
  It will also allow me to limit how many lines will be printed at one time.
  Whilst the Hud seeker itself will contain all lines, i will only print
  the subset indicated by some `view` parameter. => printing overhead constant")

(defrecord Hud [history offsets])

(defn lines [& line]
  (vec (apply concat line)))

(def ^:const empty-line [[]])
(def ^:const greeting (i/str->line "Welcome to the Omnia REPL! (ALPHA)"))
(def ^:const caret (i/str->line "Ω=>"))
(def ^:const goodbye (lines
                       empty-line
                       (i/str->line "Bye.. for now.")
                       (i/str->line "For even the very wise cannot see all ends.")))

(defn print!
  ([terminal seeker] (print! terminal seeker 0 0))
  ([terminal seeker ox oy]
   (doseq [[-y line] (->> seeker :lines (map-indexed vector))
           :let [y (+ -y oy)]]
     (->> line
          (map-indexed vector)
          (reduce (fn [state [-x c]]
                    (let [[next-state colour] (process state c)
                          x (+ -x ox)]
                      (doto terminal
                        (t/set-fg-color colour)
                        (t/move-cursor x y)
                        (t/put-character c))
                      next-state)) s0)))))

(defn update! [terminal seeker hud]
  (let [[x y] (:cursor seeker)
        [_ oy] (:cursor hud)]
    (print! terminal seeker 0 oy)
    (t/move-cursor terminal x (+ y oy))))

(def init-hud
  "A `hud` is just a seeker of seekers, where the cursor
  is denotes the offset input position."
  (let [prelude (lines greeting
                       empty-line
                       caret)]
    (i/->Seeker prelude [0 (count prelude)])))

(defn preserve [hud & seekers]
  (let [data (->> seekers
                  (map :lines)
                  (apply lines))]
    (-> hud
        (update :lines #(into % data))
        (i/move (fn [[x y]] [x (+ y (count data))])))))

(defn render [terminal seeker hud]
  (doto terminal
    (t/clear)
    (print! hud)
    (update! seeker hud)))

(defn read-eval-print [terminal repl]
  (loop [hud init-hud
         nrepl repl
         seeker i/empty-seeker]
    (render terminal seeker hud)
    (let [stroke (t/get-keystroke-blocking terminal)]
      (m/match [stroke]
               [{:key \d :ctrl true}] (do
                                        (render terminal (i/seeker goodbye) (preserve hud seeker))
                                        (Thread/sleep 1200))

               [{:key :up :alt true}] (let [x (r/travel-back nrepl)]
                                        (recur hud x (r/now x)))

               [{:key :down :alt true}] (let [x (r/travel-forward nrepl)]
                                          (recur hud x (r/now x)))

               [{:key \e :alt true}] (let [evaled (r/evaluate nrepl seeker)]
                                       (recur (preserve hud
                                                        seeker
                                                        (r/result evaled)
                                                        (i/seeker caret))
                                              evaled
                                              i/empty-seeker))
               :else (recur hud nrepl (i/inputs seeker stroke))))))

