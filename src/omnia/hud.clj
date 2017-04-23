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
    3. fipp-pretty printed collection outputs // done
    4. Add highlighting functionality.
    5. Add separate command input.
    6. Add i-search and reverse i-search as command.
    7. Add matching parens highlighting.")

(comment
  " Limit logic:
    ov = how many lines were offset in the upward direction
    ov = 0 means that the latest fov lines of the seeker are displayed
    fov + ov = how much we've gone upward and currently see/have seen

    h - ov => line where the current view of the fov ends

    (*) h - ov - fov = line form which the current view of the fov starts,
    given that h > fov")

(defrecord Context [terminal render previous-hud complete-hud persisted-hud repl seeker])

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

(defn print-row! [terminal line y]
  (reduce-idx
    (fn [x state c]
      (let [[next-state colour] (process state c)]
        (doto terminal
          (t/set-fg-color colour)
          (t/put-character c x y))
        next-state)) s0 line))

(defn print! [terminal seeker]
  (reduce-idx
    (fn [y _ line] (print-row! terminal line y))
    nil (:lines seeker)))

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

(defn jump [hud line]
  (assoc hud :lor (-> hud (i/height) (- line))))

(defn project-cursor [hud]
  (let [{[x cy] :cursor
         fov    :fov
         ov     :ov
         h      :height} hud
        y (if (> @h fov) (- cy (- @h fov ov)) cy)]
    [x y]))

(defn render [ctx f]
  (let [{terminal :terminal
         complete :complete-hud
         previous :previous-hud} ctx
        [x y] (project-cursor complete)]
    (doto terminal
      (f (project complete) (project previous))
      (t/move-cursor x y))))

(defn total! [terminal current former]
  (doto terminal (t/clear) (print! current)))

(defn nothing! [terminal current former]
  (if (not= (:ov current) (:ov former))
    (total! terminal current former)
    ()))

(defn pad [current-line former-line]
  (let [hc (count current-line)
        hf (count former-line)
        largest (max hc hf)]
    (->> (repeat \space)
         (take (- largest hc))
         (concat current-line)
         (vec))))

(defn diff! [terminal current former]
  (if (not= (:ov current) (:ov former))
    (total! terminal current former)
    (->> (:lines former)
         (zip-all (:lines current))
         (map-indexed (fn [idx paired] (conj paired idx)))
         (drop-while (fn [[current-line former-line _]] (= current-line former-line)))
         (map (fn [[current-line former-line y]] [(pad current-line former-line) y]))
         (foreach (fn [[line y]] (print-row! terminal line y))))))

(defn render-context [ctx]
  (case (:render ctx)
    :diff (render ctx diff!)
    :nothing (render ctx nothing!)
    (render ctx total!)))

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
                      (:complete-hud)
                      (preserve (r/result evaluation)
                                (i/seeker caret))
                      (i/move-x (fn [_] 0)))]
    (assoc ctx
      :previous-hud (:complete-hud ctx)
      :persisted-hud persisted
      :complete-hud persisted
      :repl evaluation
      :seeker i/empty-seeker)))

(defn capture [ctx stroke]
  (let [seeker (-> ctx (:seeker) (i/inputs stroke))]
    (assoc ctx
      :previous-hud (:complete-hud ctx)
      :complete-hud (-> ctx (:persisted-hud) (i/join seeker))
      :seeker seeker)))

(defn exit [ctx]
  (-> ctx
      (update :complete-hud #(preserve % (i/seeker goodbye)))
      (assoc-in [:persisted-hud :ov] 0)
      (assoc-in [:complete-hud :ov] 0)
      (render-context))
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
  (let [formatted (-> ctx :seeker (f/lisp-format))]
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

(defn re-render [ctx]
  (assoc ctx :render :total))

(defn diff-render [ctx]
  (assoc ctx :render :diff))

(defn no-render [ctx]
  (assoc ctx :render :nothing))

(defn movement? [stroke]
  (contains? #{:up :down :left :right} (:key stroke)))

(defn clear [ctx]
  (let [fov (-> ctx :terminal (.getTerminalSize) (.getRows))
        start-hud (init-hud fov)]
    (assoc ctx
      :complete-hud (i/join start-hud (:seeker ctx))
      :persisted-hud start-hud
      :previous-hud start-hud)))

(defn read-eval-print [terminal repl]
  (let [start-hud (init-hud 0)
        eval-ctx (Context. terminal :total start-hud start-hud start-hud repl i/empty-seeker)]
    (loop [ctx (resize eval-ctx)]
      (render-context ctx)
      (let [stroke (t/get-keystroke-blocking terminal)]
        (m/match [stroke]
                 [{:key :page-up}] (-> ctx (resize) (scroll-up) (re-render) (recur))
                 [{:key :page-down}] (-> ctx (resize) (scroll-down) (re-render) (recur))
                 [{:key :up :alt true}] (-> ctx (resize) (roll-back) (reformat) (scroll-stop) (re-render) (recur))
                 [{:key :down :alt true}] (-> ctx (resize) (roll-forward) (reformat) (scroll-stop) (re-render) (recur))
                 [{:key \r :ctrl true}] (-> ctx (clear) (re-render) (recur))
                 [{:key \e :alt true}] (-> ctx (resize) (evaluate) (scroll-stop) (re-render) (recur))
                 [{:key \d :ctrl true}] (-> ctx (resize) (scroll-stop) (re-render) (exit))
                 [_ :guard movement?] (-> ctx (resize) (capture stroke) (reformat) (navigate) (scroll-stop) (no-render) (recur))
                 :else (-> ctx (resize) (capture stroke) (reformat) (navigate) (scroll-stop) (diff-render) (recur)))))))
