1(ns omnia.hud
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
    5. Add i-search and reverse i-search as command.
    6. Add matching parens highlighting.
    7. Add separate command input.")

(comment
  " Limit logic:
    ov = how many lines were offset in the upward direction
    ov = 0 means that the latest fov lines of the seeker are displayed
    fov + ov = how much we've gone upward and currently see/have seen

    h - ov => line where the current view of the fov ends

    (*) h - ov - fov = line form which the current view of the fov starts,
    given that h > fov")


;; FIXME: Better selection
(comment
  "Instead of mirroring the input seeker in the complete-hud,
  when refactoring, also renormalise the selection.
  This, in theory, should be doable, as the y-coordinate of lines doesn't
  get changed after formatting.
  The only thing that changes is the x-coordinate and the difference in change
  can be calculated:
  The difference is countable based on the number of spaces the original
  and formatted version contain.
        S_form - S_orig = D, where D -> indentation for that line

  This means that for some selection starting at xs and ending at xe,
  these should be offset by their respective D's for their line.

  Additionally, joining two seekers should also normalise selections.
  In this case, it is not the the x's that get offset, but rather the y's.
  The right-hand seeker's y-selection coordinate gets offset by the
  size of the left-hand seeker's height. ")

(defrecord Context [terminal render previous-hud complete-hud persisted-hud repl seeker])

(def ^:const empty-line [])
(def ^:const empty-text [empty-line])
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
        (assoc :selected :already)
        (assoc :scroll? false)
        (assoc :lor (i/height seeker))
        (assoc :fov fov)
        (assoc :ov 0))))

(defn init-hud [fov]
  (hud fov greeting empty-text caret))

(defn global-y [hud local-y]
  (let [{fov :fov
         ov  :ov
         h   :height} hud]
    (if (> @h fov)
      (- local-y (- @h fov ov))
      local-y)))

(defn screen-size [ctx]
  (if-let [terminal (:terminal ctx)]
    (-> terminal (.getTerminalSize) (.getRows))
    (-> ctx :persisted-hud :fov)))

(defn highlight! [ctx]
  (let [{terminal :terminal
         complete :complete-hud} ctx
        {[xs ys] :start
         [xe ye] :end} (:selection complete)]
    (loop [x xs
           y ys]
      (cond
        (-> complete :selection empty?) ()
        (and (= y ye) (= x xe)) ()
        (i/sym-at complete [x y]) (do
                                    (doto terminal
                                      (t/set-bg-color :blue)
                                      (t/put-character (i/sym-at complete [x y]) x (global-y complete y)))
                                    (recur (inc x) y))
        :else (recur 0 (inc y))))
    (t/set-bg-color terminal :default)))

(defn print-row! [y terminal line]
  (reduce-idx
    (fn [x state c]
      (let [[next-state colour] (process state c)]
        (doto terminal
          (t/set-fg-color colour)
          (t/put-character c x y))
        next-state)) s0 line))

(defn print! [terminal seeker]
  (reduce-idx
    (fn [y _ line] (print-row! y terminal line))
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

#_(defn jump [hud line]
    (assoc hud :lor (-> hud (i/height) (- line))))

(defn project-cursor [hud]
  "gy = cy - (h - fov - ov)
   cy = gy + (h - fov - ov)"
  (let [{[x cy] :cursor
         fov    :fov
         ov     :ov
         h      :height} hud
        y (global-y hud cy)]
    [x y]))

(defn total! [ctx]
  (let [{terminal :terminal
         complete :complete-hud} ctx]
    (doto terminal
      (t/clear)
      (print! (project complete)))))

(defn when-unscrolled [ctx f]
  (let [{terminal :terminal
         complete :complete-hud
         previous :previous-hud} ctx
        current (project complete)
        former (project previous)]
    (if (not= (:ov current) (:ov former))
      (total! ctx)
      (f terminal current former))))

(defn pad-erase [current-line former-line]
  (let [hc (count current-line)
        hf (count former-line)
        largest (max hc hf)]
    (->> (repeat \space)
         (take (- largest hc))
         (concat current-line)
         (vec))))

(defn diff! [ctx]
  (when-unscrolled ctx
    (fn [terminal current former]
      (->> (:lines former)
           (zip-all (:lines current))
           (map-indexed (fn [idx paired] (conj paired idx)))
           (drop-while (fn [[current-line former-line _]] (= current-line former-line)))
           (map (fn [[current-line former-line y]] [(pad-erase current-line former-line) y]))
           (foreach (fn [[line y]] (print-row! y terminal line)))))))

(defn input! [ctx]
  (let [{persisted :persisted-hud
         seeker    :seeker} ctx
        padding (->> empty-line (repeat) (take (i/height seeker)) (vec) (i/seeker))]
    (-> ctx
        (assoc :previous-hud (i/join persisted padding))
        (diff!))))

(defn minimal! [ctx]
  (when-unscrolled ctx
    (fn [_ current former]
      (if (-> current :selected (= :now))
        (input! ctx)
        ()))))

(defn nothing! [ctx]
  (when-unscrolled ctx (fn [_ _ _] ())))

(defn render-context [ctx]
  (let [{terminal :terminal
         complete :complete-hud} ctx
        [x y] (project-cursor complete)]
    (case (:render ctx)
      :diff (doto ctx (diff!) (highlight!))
      :input (doto ctx (input!) (highlight!))
      :minimal (doto ctx (minimal!) (highlight!))
      :nothing (doto ctx (nothing!) (highlight!))
      (doto ctx (total!) (highlight!)))
    (t/move-cursor terminal x y)))

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
      :complete-hud (-> ctx
                        (:persisted-hud)
                        (i/join seeker)
                        (assoc :selection (get-in ctx [:complete-hud :selection]))
                        (assoc :selected (get-in ctx [:complete-hud :selected])))
      :seeker seeker)))

(defn exit [ctx]
  (-> ctx
      (update :complete-hud #(preserve % (i/seeker goodbye)))
      (assoc-in [:persisted-hud :ov] 0)
      (assoc-in [:complete-hud :ov] 0)
      (render-context))
  (Thread/sleep 1200))

(defn resize [ctx]
  (let [fov (get-in ctx [:persisted-hud :fov])
        ssize (screen-size ctx)]
    (if (not= ssize fov)
      (-> ctx
          (assoc-in [:persisted-hud :fov] ssize)
          (assoc-in [:complete-hud :fov] ssize))
      ctx)))

(defn reformat [ctx]
  (let [formatted (-> ctx :seeker (f/lisp-format))]
    (assoc ctx
      :complete-hud (-> ctx
                        (:persisted-hud)
                        (i/join formatted)
                        (assoc :selection (get-in ctx [:complete-hud :selection]))
                        (assoc :selected (get-in ctx [:complete-hud :selected]))))))

(defn navigate [ctx]
  (let [{{fov :fov
          ov  :ov
          hc   :height} :complete-hud
         previous      :previous-hud
         seeker        :seeker} ctx
        h (i/height seeker)
        [_ y] (:cursor seeker)
        upper-limit? (neg? (+ (- (+ fov ov) h) (bound-dec y 0)))
        lower-limit? (and (> h 0) (>= (bound-inc y h) (- h ov)))
        decreased? (< @hc (i/height previous))
        f (cond
            upper-limit? inc
            lower-limit? dec
            decreased? #(-- % (i/height previous) @hc)
            :else identity)]
    (-> ctx
        (update-in [:complete-hud :ov] f)
        (update-in [:persisted-hud :ov] f))))

(defn re-render [ctx]
  (assoc ctx :render :total))

(defn diff-render [ctx]
  (assoc ctx :render :diff))

(defn input-render [ctx]
  (assoc ctx :render :input))

(defn min-render [ctx]
  (assoc ctx :render :minimal))

(defn no-render [ctx]
  (assoc ctx :render :nothing))

(defn clear [ctx]
  (let [fov (screen-size ctx)
        start-hud (init-hud fov)]
    (assoc ctx
      :complete-hud (i/join start-hud (:seeker ctx))
      :persisted-hud start-hud
      :previous-hud start-hud)))

(defn select [ctx]
  (-> ctx
      (assoc-in [:complete-hud :selected] :selecting)
      (update :complete-hud i/select)
      (update :seeker i/select)))

(defn deselect [ctx]
  (let [selectivity {:selecting :now
                     :now       :already
                     :already   :already}]
    (-> ctx
        (update-in [:complete-hud :selected] selectivity)
        (update :complete-hud i/deselect)
        (update :seeker i/deselect))))

(defn movement? [stroke]
  (contains? #{:up :down :left :right} (:key stroke)))

(defn selection? [stroke]
  (and (movement? stroke)
       (:shift stroke)))

(defn manipulation? [stroke]
  (and (contains? #{\v \c \x} (:key stroke))
       (:alt stroke)))

(comment
  "The easier solution to all of this would be to actually
  format the seeker directly, without side-effects
  and then use that x directly with the projected y of the complete hud.
  This would allow me to avoid keeping track of both complete and seeker selection."

  "The current way binds input logic to rendering logic in such a way, that I have to catch inputs things on the rendering level.
  This I don't like. Rendering should reflect input, not dictate how input should look like.

  For example: select-all would require me to write essentially the same function both for input and for hud")

(defn handle [ctx stroke]
  (m/match [stroke]
           [{:key :page-up}] (-> ctx (resize) (scroll-up) (re-render))
           [{:key :page-down}] (-> ctx (resize) (scroll-down) (re-render))
           [{:key :up :alt true}] (-> ctx (resize) (roll-back) (reformat) (scroll-stop) (re-render))
           [{:key :down :alt true}] (-> ctx (resize) (roll-forward) (reformat) (scroll-stop) (re-render))
           [{:key \r :ctrl true}] (-> ctx (clear) (re-render))
           [{:key \e :alt true}] (-> ctx (resize) (evaluate) (scroll-stop) (re-render))
           [{:key \d :ctrl true}] (-> ctx (resize) (scroll-stop) (re-render) (exit))
           [_ :guard selection?] (-> ctx (resize) (select) (capture stroke) (reformat) (select) (navigate) (scroll-stop) (no-render))
           [_ :guard movement?] (-> ctx (resize) (capture stroke) (reformat) (deselect) (navigate) (scroll-stop) (min-render))
           [_ :guard manipulation?] (-> ctx (resize) (capture stroke) (reformat) (deselect) (navigate) (scroll-stop) (re-render))
           :else (-> ctx (resize) (capture stroke) (reformat) (deselect) (navigate) (scroll-stop) (diff-render))))

(defn read-eval-print [terminal repl]
  (let [start-hud (init-hud 0)
        eval-ctx (Context. terminal :total start-hud start-hud start-hud repl i/empty-seeker)]
    (loop [ctx (resize eval-ctx)]
      (when ctx
        (render-context ctx)
        (-> ctx
            (handle (t/get-keystroke-blocking terminal))
            (recur))))))
