(ns omnia.hud
  (:gen-class)
  (use omnia.highlighting
       omnia.more)
  (require [lanterna.terminal :as t]
           [omnia.repl :as r]
           [omnia.input :as i]
           [clojure.core.match :as m]))

(comment
  "This is now working properly.
  DO IT BETTER!
  Advice:
  1. Make seekers store their height. This would optimise some things. // done
  2. Make the projections and view clipping more explicit. Currently it does too much naive offsetting. // done
  3. Perhaps make primitives and combinators.
  4. Make more modular.
  5. Configurise input from pattern-match.")

(def ^:const empty-line [[]])
(def ^:const greeting (i/str->lines "Welcome to the Omnia REPL! (ALPHA)"))
(def ^:const caret (i/str->lines "Ω=>"))
(def ^:const goodbye (i/join-lines
                       (i/str->lines "Bye.. for now.")
                       (i/str->lines "For even the very wise cannot see all ends.")))

(defn hud [fov & prelude]
  (let [seeker (->> prelude (apply i/join-lines) (i/seeker))]
    (-> seeker
        (i/end-y)
        (assoc :lor (i/height seeker))
        (assoc :fov fov))))

(defn init-hud [fov]
  (hud fov greeting empty-line caret))

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

(defn scroll [hud]
  (assoc hud :scrolling true))

(defn noscroll [hud]
  (assoc hud :scrolling false))


(defn preserve [hud & seekers]
  (let [data (->> seekers
                  (map :lines)
                  (apply i/join-lines))]
    (-> hud
        (i/rebase #(into % data))
        (i/end-y))))

(defn join [this-seeker that-seeker]
  (let [[x y] (:cursor that-seeker)]
    (-> this-seeker
        (i/rebase #(concat % (:lines that-seeker)))
        (i/move (fn [[_ oy]] [x (+ y oy)])))))

(defn nowards [hud]
  (let [fov (:fov hud)
        h (i/height hud)
        lor (if (> h fov) fov h)]
    (assoc hud :lor lor)))

(defn upwards [hud limit]
  (update hud :lor #(bound-inc % (inc limit))))

(defn downwards [hud limit]
  (let [fov (:fov hud)
        min (if (> limit fov) fov limit)]
    (update hud :lor #(bound-dec % min))))

(defn project [hud]
  (let [{lor :lor
         fov :fov
         scroll? :scrolling} hud]
    (if scroll?
      (i/rebase hud #(->> % (take-right lor) (take fov)))
      (i/rebase hud #(take-right fov %)))))

(defn y [hud]
  "proper y position: fov - (h/hud - c/y)"
  (let [{fov   :fov
         [_ y] :cursor} hud]
    (if (> y fov) (- fov (- (i/height hud) y)) y)))

(defn render [terminal hud]
  (let [{fov :fov
         [x _] :cursor} hud]
    (doto terminal
      (t/clear)
      (print! (project hud))
      (t/move-cursor x (y hud)))))

(defn read-eval-print [terminal repl]
  (let [fov (-> terminal (.getTerminalSize) (.getRows))]
    (loop [hud (init-hud fov)
           nrepl repl
           seeker i/empty-seeker]
      (let [joined (join hud seeker)]
        (render terminal joined)
        (let [stroke (t/get-keystroke-blocking terminal)]
          (m/match [stroke]
                   [{:key :page-up}] (recur (-> hud (scroll) (upwards (i/height joined)))
                                            nrepl
                                            seeker)
                   [{:key :page-down}] (recur (-> hud (scroll) (downwards (i/height joined)))
                                              nrepl
                                              seeker)
                   [{:key \d :ctrl true}] (do
                                            (render terminal (-> joined (preserve (i/seeker goodbye)) (noscroll) (nowards)))
                                            (Thread/sleep 1200))

                   [{:key :up :alt true}] (let [x (r/travel-back nrepl)]
                                            (recur hud x (r/now x)))

                   [{:key :down :alt true}] (let [x (r/travel-forward nrepl)]
                                              (recur hud x (r/now x)))

                   [{:key \e :alt true}] (let [evaled (r/evaluate nrepl seeker)]
                                           (recur (-> hud
                                                      (preserve seeker (r/result evaled) (i/seeker caret))
                                                      (noscroll)
                                                      (nowards))
                                                  evaled
                                                  i/empty-seeker))
                   :else (recur (-> hud (noscroll) (nowards))
                                nrepl
                                (i/inputs seeker stroke))))))))

