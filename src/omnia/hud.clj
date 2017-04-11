(ns omnia.hud
  (:gen-class)
  (use omnia.highlighting
       omnia.more)
  (require [lanterna.terminal :as t]
           [omnia.repl :as r]
           [omnia.input :as i]
           [clojure.core.match :as m]))

(comment
  ;; FIXME
  " 1. Configurise input from pattern-match.
    2. Add `jump-to` as a function that jumps to a line.
    3. Add copy-paste functionality.
    4. Add highlighting functionality.
    5. Add separate command input.
    6. Add i-search and reverse i-search as command.
    7. Add matching parens highlighting.")

(defrecord EvalCtx [terminal complete-hud persisted-hud repl seeker])

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
        (assoc :scroll? false)
        (assoc :lor (i/height seeker))
        (assoc :fov fov)
        (assoc :ov 0))))

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


(defn preserve [hud & seekers]
  (let [data (->> seekers
                  (map :lines)
                  (apply i/join-lines))]
    (-> hud
        (i/rebase #(into % data))
        (i/end-y))))

(defn nowards [height fov _ ov]
  (if (> height fov) (+ fov ov) height))

(defn upwards [height _ lor _]
  (bound-inc lor (inc height)))

(defn downwards [height fov lor ov]
  (bound-dec lor (nowards height fov lor ov)))

(defn scroll [hud f]
  (let [{lor :lor
         fov :fov
         ov  :ov
         h   :height} hud]
    (-> hud
        (assoc :scroll? true)
        (assoc :lor (f @h fov lor ov)))))

(defn noscroll [hud]
  (-> hud
      (scroll nowards)
      (assoc :scroll? false)))

(defn project [hud]
  (let [{lor     :lor
         fov     :fov
         ov      :ov
         scroll? :scroll?} hud]
    (if scroll?
      (i/rebase hud #(->> % (take-right lor) (take fov)))
      (i/rebase hud #(->> % (drop-last ov) (take-right fov))))))


(defn render [hud terminal]
  "y = fov - (h/hud - c/y) iff h/hud > fov"
  (let [{[x cy] :cursor
         fov    :fov
         ov     :ov
         h      :height} hud
        y (if (> @h fov) (- cy (- @h fov ov)) cy)]
    (doto terminal
      (t/clear)
      (print! (project hud))
      (t/move-cursor x y))))

(defn render-ctx [ctx]
  (render (:complete-hud ctx)
          (:terminal ctx)))

(comment
  " Limit logic:
    ov = how many lines were offset in the upward direction
    ov = 0 means that the latest fov lines of the seeker are displayed
    fov + ov = how much the whole page was moved upwards

    h - ov => line where the current view of the fov ends

    h - ov - fov = line form which the current view of the fov starts
  ")

(defn upper-limit? [ctx]
  (let [{{fov :fov
          ov  :ov} :complete-hud
         seeker    :seeker} ctx
        h (i/height seeker)
        [_ y] (:cursor seeker)]
    (neg? (+ (- (+ fov ov) h) (bound-dec y 0)))))

(defn lower-limit? [ctx]
  (let [{{fov :fov
          ov  :ov} :complete-hud
         seeker    :seeker} ctx
        h (i/height seeker)
        [_ y] (:cursor seeker)]
    (>= (bound-inc y h) (- h ov))))

(defn implicit-up [ctx]
  (if (upper-limit? ctx)
    (-> ctx
        (update-in [:complete-hud :ov] inc)
        (update-in [:persisted-hud :ov] inc))
    ctx))

(defn implicit-down [ctx]
  (if (lower-limit? ctx)
    (-> ctx
        (update-in [:complete-hud :ov] dec)
        (update-in [:persisted-hud :ov] dec))
    ctx))

(defn scroll-up [ctx]
  (update ctx :complete-hud #(scroll % upwards)))

(defn scroll-down [ctx]
  (update ctx :complete-hud #(scroll % downwards)))

(defn scroll-stop [ctx]
  (-> ctx
      (update :complete-hud noscroll)
      (update :persisted-hud noscroll)))

(defn roll-repl [ctx f]
  (let [then-repl (-> ctx :repl f)
        then-seeker (r/then then-repl)]
    (assoc ctx
      :complete-hud (-> ctx (:persisted-hud) (i/join then-seeker))
      :repl then-repl
      :seeker then-seeker)))

(defn roll-back [ctx]
  (roll-repl ctx r/travel-back))

(defn roll-forward [ctx]
  (roll-repl ctx r/travel-forward))

(defn evaluate [ctx]
  (let [evaluation (r/evaluate (:repl ctx) (:seeker ctx))
        persisted (-> ctx
                      (:persisted-hud)
                      (preserve (:seeker ctx)
                                (r/result evaluation)
                                (i/seeker caret)))]
    (assoc ctx
      :persisted-hud persisted
      :complete-hud persisted
      :repl evaluation
      :seeker i/empty-seeker)))

(defn capture [ctx stroke]
  (let [seeker (-> ctx (:seeker) (i/inputs stroke))]
    (assoc ctx
      :complete-hud (-> ctx (:persisted-hud) (i/join seeker))
      :seeker seeker)))

(defn exit [ctx]
  (-> ctx
      (update :complete-hud #(preserve % (i/seeker goodbye)))
      (render-ctx))
  (Thread/sleep 1200))

(defn resize [ctx]
  (let [{{fov :fov} :persisted-hud
         terminal  :terminal} ctx
        tsize (-> terminal (.getTerminalSize) (.getRows))]
    (if (not= tsize fov)
      (-> ctx
          (assoc-in [:persisted-hud :fov] tsize)
          (assoc-in [:complete-hud :fov] tsize))
      ctx)))

(defn read-eval-print [terminal repl]
  (let [start-hud (init-hud 0)
        eval-ctx (EvalCtx. terminal start-hud start-hud repl i/empty-seeker)]
    (loop [ctx (resize eval-ctx)]
      (render-ctx ctx)
      (let [stroke (t/get-keystroke-blocking terminal)]
        (m/match [stroke]
                 [{:key :page-up}] (-> ctx (resize) (scroll-up) (recur))
                 [{:key :page-down}] (-> ctx (resize) (scroll-down) (recur))
                 [{:key :up :alt true}] (-> ctx (resize) (roll-back) (scroll-stop) (recur))
                 [{:key :down :alt true}] (-> ctx (resize) (roll-forward) (scroll-stop) (recur))
                 [{:key \e :alt true}] (-> ctx (resize) (evaluate) (scroll-stop) (recur))
                 [{:key \d :ctrl true}] (-> ctx (resize) (scroll-stop) (exit))
                 [{:key :up}] (-> ctx (resize) (implicit-up) (capture stroke) (scroll-stop) (recur))
                 [{:key :down}] (-> ctx (resize) (implicit-down) (capture stroke) (scroll-stop) (recur))
                 [{:key \r :ctrl true}] (-> ctx (resize) (scroll-stop) (recur))
                 :else (-> ctx (resize) (capture stroke) (scroll-stop) (recur)))))))
