(ns omnia.hud
  (:gen-class)
  (use omnia.highlighting
       omnia.more)
  (require [lanterna.terminal :as t]
           [omnia.repl :as r]
           [omnia.input :as i]
           [clojure.core.match :as m]
           [omnia.formatting :as f]))

(comment
  " 1. Configurise input from pattern-match. // will be added together with the seeker input configurisation
    2. Add `jump-to` as a function that jumps to a line. // done
    3. Add highlighting functionality.
    4. Add separate command input.
    5. Add i-search and reverse i-search as command.
    6. Add matching parens highlighting.")

(defrecord EvalCtx [terminal complete-hud persisted-hud repl seeker])

(def ^:const empty-line [[]])
(def ^:const greeting (i/str->lines "Welcome to the Omnia REPL! (ALPHA)"))
(def ^:const caret (i/str->lines "Ω=>"))
(def ^:const goodbye (i/join-lines
                       (i/str->lines "Bye.. for now.")
                       (i/str->lines "For even the very wise cannot see all ends.")))

(defn hud [fov & prelude]
  "lor = line of reference
     indicates the amount of lines that have been viewed so far
     when going through the history. (seeker + hud)
   fov = field of view
     indicates the amount of lines that can be viewed at one time
     in the terminal screen
   ov  = overview
     indicates the amount of lines that have been viewed so far
     when going back just through the input seeker itself. (seeker)
     Conceptually it is the same as the `lor`, but only on input seeker level.
   scroll? = scrolling flag
     indicates if there should be scrolled currently"
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
  "y calculation is based on the limit logic"
  (let [{[x cy] :cursor
         fov    :fov
         ov     :ov
         h      :height} hud
        y (if (> @h fov) (- cy (- @h fov ov)) cy)]
    (doto terminal
      (t/clear)
      (print! (project hud))
      (t/move-cursor x y))))

(defn jump [hud line]
  (assoc hud :lor (-> hud (i/height) (- line))))

(defn render-ctx [ctx]
  (render (:complete-hud ctx)
          (:terminal ctx)))

(comment
  " Limit logic:
    ov = how many lines were offset in the upward direction
    ov = 0 means that the latest fov lines of the seeker are displayed
    fov + ov = how much we've gone upward and currently see/have seen

    h - ov => line where the current view of the fov ends

    (*) h - ov - fov = line form which the current view of the fov starts,
    given that h > fov")

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
      (assoc-in [:persisted-hud :ov] 0)
      (assoc-in [:complete-hud :ov] 0)
      (render-ctx))
  (Thread/sleep 1200))

(defn resize [ctx]
  (let [{{fov :fov} :persisted-hud
         terminal   :terminal} ctx
        tsize (-> terminal (.getTerminalSize) (.getRows))]
    (if (not= tsize fov)
      (-> ctx
          (assoc-in [:persisted-hud :fov] tsize)
          (assoc-in [:complete-hud :fov] tsize))
      ctx)))

(defn reformat [ctx]
  (let [formatted (-> ctx :seeker (f/format-seeker))]
    (assoc ctx
      :complete-hud (-> ctx (:persisted-hud) (i/join formatted)))))

(defn navigate [ctx]
  (let [{{fov :fov
          ov  :ov} :complete-hud
         seeker    :seeker} ctx
        h (i/height seeker)
        [_ y] (:cursor seeker)
        upper-limit (neg? (+ (- (+ fov ov) h) (bound-dec y 0)))
        lower-limit (and (> h 0) (>= (bound-inc y h) (- h ov)))
        f (cond
            upper-limit inc
            lower-limit dec
            :else identity)]
    (-> ctx
        (update-in [:complete-hud :ov] f)
        (update-in [:persisted-hud :ov] f))))

(defn read-eval-print [terminal repl]
  (let [start-hud (init-hud 0)
        eval-ctx (EvalCtx. terminal start-hud start-hud repl i/empty-seeker)]
    (loop [ctx (resize eval-ctx)]
      (render-ctx ctx)
      (let [stroke (t/get-keystroke-blocking terminal)]
        (m/match [stroke]
                 [{:key :page-up}] (-> ctx (resize) (scroll-up) (recur))
                 [{:key :page-down}] (-> ctx (resize) (scroll-down) (recur))
                 [{:key :up :alt true}] (-> ctx (resize) (roll-back) (reformat) (scroll-stop) (recur))
                 [{:key :down :alt true}] (-> ctx (resize) (roll-forward) (reformat) (scroll-stop) (recur))
                 [{:key \e :alt true}] (-> ctx (resize) (evaluate) (scroll-stop) (recur))
                 [{:key \d :ctrl true}] (-> ctx (resize) (scroll-stop) (exit))
                 [{:key \r :ctrl true}] (-> ctx (resize) (scroll-stop) (recur))
                 :else (-> ctx (resize) (capture stroke) (reformat) (navigate) (scroll-stop) (recur)))))))
