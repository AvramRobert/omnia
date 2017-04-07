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
  2. Make the projections and view clipping more explicit. Currently it does too much naive offsetting.
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

(defn upwards [hud]
  (update hud :lor #(bound-inc % (inc (i/height hud)))))    ;; include 0

(defn downwards [hud seeker]
  (update hud :lor #(bound-dec % (- (:fov hud) (i/height seeker)))))

(defn nowards [hud seeker]
  (let [fov (:fov hud)]
    (assoc hud :lor (->> hud
                         (:lines)
                         (take-right (- fov (i/height seeker)))
                         (count)))))

(defn scroll-projection [hud seeker]
  (let [{fov :fov
         lor :lor} hud
        phud (i/rebase hud #(->> % (take-right lor) (take fov)))
        pseeker (i/rebase seeker #(take fov %))]
    [phud pseeker]))

(defn input-projection [hud seeker]
  (let [fov (:fov hud)
        pseeker (i/rebase seeker #(take-right fov %))       ;; Cursor is always displayed over the caret because after evaluating, this is always empty
        phud (-> hud
                 (i/rebase #(take-right (- fov (i/height pseeker)) %))
                 (i/end-y))]
    [phud pseeker]))

(defn project [hud seeker]
  (if (:scrolling hud)
    (scroll-projection hud seeker)
    (input-projection hud seeker)))

(defn preserve [hud & seekers]
  (let [data (->> seekers
                  (map :lines)
                  (apply i/join-lines))]
    (-> hud
        (i/rebase #(into % data))
        (i/end-y))))

(defn render [terminal seeker hud]
  (let [[phud pseeker] (project hud seeker)
        [ox oy] (:cursor phud)
        [x y] (:cursor pseeker)]
    (doto terminal
      (t/clear)
      (print! phud)
      (print! pseeker ox oy)
      (t/move-cursor (+ x ox) (+ y oy)))))

(defn join [this-seeker that-seeker]
  (let [[x y] (:cursor that-seeker)]
    (-> this-seeker
        (i/rebase #(concat % (:lines that-seeker)))
        (i/move (fn [[_ oy]] [x (+ y oy)])))))

(comment
  "A mod would not work because when i go down, the page doesn't jump to the top and start again,
  but rather it rolls and continues downwards.")

(comment
  "nowards should compute the proper value for this function to work.
  The implementation of nowards should be very simple, but it keeps avoiding me.
  Essentially, lor describes how much of the whole hud i've seen so far.
  When resetting, it should be the `latest` amount. In most cases, this is the fov itself.
  When scrolling, this will then be offset depending on how far i go upward or downward.
  When typing however, the reset should kick in")
(defn scroll-proj [hud]
  (let [{lor :lor
         fov :fov} hud]
    (i/rebase hud #(->> % (take-right lor) (take fov)))))

(defn project2 [hud]
  (if (:scrolling hud)
    (scroll-proj hud)
    (i/rebase hud #(take-right (:fov hud) %))))

(defn y [hud]
  "proper y position: fov - (h/hud - c/y)"
  (let [{fov   :fov
         [_ y] :cursor} hud]
    (if (> y fov) (- fov (- (i/height hud) y)) y)))

(defn render2 [terminal hud]
  (let [{fov :fov
         [x _] :cursor} hud]
    (doto terminal
      (t/clear)
      (print! (project2 hud))
      (t/move-cursor x (y hud)))))

(defn read-eval-print [terminal repl]
  (let [fov (-> terminal (.getTerminalSize) (.getRows))]
    (loop [hud (init-hud fov)
           nrepl repl
           seeker i/empty-seeker]
      (render2 terminal (join hud seeker))
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
                              (i/inputs seeker stroke)))))))

