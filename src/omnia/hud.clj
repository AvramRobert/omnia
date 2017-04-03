(ns omnia.hud
  (:gen-class)
  (use omnia.highlighting
       omnia.more)
  (require [lanterna.terminal :as t]
           [omnia.repl :as r]
           [omnia.input :as i]
           [clojure.core.match :as m]))

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

(defn init-hud [fov]
  (let [prelude (lines greeting
                       empty-line
                       caret)
        lor (count prelude)                                 ;; amount of lines from the history that i've included
        cursor [0 lor]]
    (-> i/empty-seeker
        (assoc :lines prelude)
        (assoc :cursor cursor)
        (assoc :lor lor)
        (assoc :fov fov))))

(defn scroll [hud]
  (assoc hud :scrolling true))

(defn noscroll [hud]
  (assoc hud :scrolling false))

(defn upwards [hud]
  (update hud :lor #(bound-inc % (inc (i/height hud)))))    ;; include 0

(defn downwards [hud seeker]
  (update hud :lor #(bound-dec % (- (:fov hud)
                                    (i/height seeker)))))                       ;; exclude 0 ;; try (- fov (h/height seeker))

(defn nowards [hud seeker]
  (let [fov (:fov hud)
        x (->> seeker :lines (take-last fov) count)
        h (->> hud :lines (vtake-right (- fov x)) count)]
    (assoc hud :lor h)))

(defn scroll-projection [hud seeker]
  (let [{fov :fov
         lor :lor} hud
        amount (- (i/height hud) lor)
        phud (update hud :lines #(->> % (drop+ amount) (take fov) (vec)))
        pseeker (update seeker :lines #(-> (- fov (i/height %)) (take %) (vec)))] ;; (i/height phud) instead of :lor because :lor may be negative
    [phud pseeker]))

(defn input-projection [hud seeker]
  (let [fov (:fov hud)
        pseeker (update seeker :lines #(vtake-right fov %)) ;; this is 0, because of no input. That's why the cursor always get's displayed there
        phud (-> hud
                 (update :lines #(vtake-right (- fov (i/height pseeker)) %))
                 (i/end-y))]
    [phud pseeker]))

(defn project [hud seeker]
  (if (:scrolling hud)
    (scroll-projection hud seeker)
    (input-projection hud seeker)))

(defn preserve [hud & seekers]
  (let [data (->> seekers
                  (map :lines)
                  (apply lines))]
    (-> hud
        (update :lines #(into % data))
        (i/move (fn [[x y]] [x (+ y (count data))])))))


(defn render [terminal seeker hud]
  (let [[phud pseeker] (project hud seeker)
        [ox oy] (:cursor phud)
        [x y] (:cursor pseeker)]
    (doto terminal
      (t/clear)
      (print! phud)
      (print! pseeker ox oy)
      (t/move-cursor (+ x ox) (+ y oy)))))

(defn read-eval-print [terminal repl] -
  (loop [hud (init-hud (-> terminal (.getTerminalSize) (.getRows)))
         nrepl repl
         seeker i/empty-seeker]
    (render terminal seeker hud)
    (let [stroke (t/get-keystroke-blocking terminal)]
      (m/match [stroke]
               [{:key :page-up}] (recur (-> hud (scroll) (upwards))
                                        nrepl
                                        seeker)
               [{:key :page-down}] (recur (-> hud (scroll) (downwards seeker))
                                          nrepl
                                          seeker)
               [{:key \d :ctrl true}] (do
                                        (render terminal (i/seeker goodbye) (-> hud
                                                                                (preserve seeker)
                                                                                (noscroll)
                                                                                (nowards seeker)))
                                        (Thread/sleep 1200))

               [{:key :up :alt true}] (let [x (r/travel-back nrepl)]
                                        (recur hud x (r/now x)))

               [{:key :down :alt true}] (let [x (r/travel-forward nrepl)]
                                          (recur hud x (r/now x)))

               [{:key \e :alt true}] (let [evaled (r/evaluate nrepl seeker)]
                                       (recur (-> hud
                                                  (preserve seeker (r/result evaled) (i/seeker caret))
                                                  (noscroll)
                                                  (nowards seeker))
                                              evaled
                                              i/empty-seeker))
               :else (recur (-> hud (noscroll) (nowards seeker))
                            nrepl
                            (i/inputs seeker stroke))))))

