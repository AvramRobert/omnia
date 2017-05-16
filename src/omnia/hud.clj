(ns omnia.hud
  (:gen-class)
  (use omnia.highlight
       omnia.more)
  (require [lanterna.terminal :as t]
           [halfling.result :refer [attempt]]
           [omnia.rendering :refer [render-context]]
           [omnia.repl :as r]
           [omnia.input :as i]
           [clojure.core.match :as m]
           [omnia.formatting :as f]))

(comment
  " 1. Configurise input from pattern-match. // will be added together with the seeker input configurisation
    2. Add `jump-to` as a function that jumps to a line. // done
    3. fipp-pretty printed collection outputs // done
    4. Add highlighting functionality. // done
    5. Fix input re-rendering.
    6. Add i-search and reverse i-search as command.
    7. Add matching parens highlighting.
    8. Add a failure handling system to properly display errors and close the application gracefully.
    9. Add separate command input.")

(defrecord Context [terminal render previous-hud complete-hud persisted-hud repl seeker])

(def ^:const empty-line [i/empty-vec])
(def ^:const delimiter (i/str->lines "------"))
(def ^:const greeting (i/str->lines "Welcome to the Omnia REPL! (ALPHA)"))
(def ^:const caret (i/str->lines "Ω=>"))
(def ^:const goodbye (i/str->lines "Bye..for now\nFor even the very wise cannot see all ends"))
(def ^:const error (i/str->lines "I have not the heart to tell you, but something went wrong internally.."))

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


;; === Utils ===

(defn init-hud [fov]
  (hud fov greeting empty-line caret))

(defn screen-size [ctx]
  (if-let [terminal (:terminal ctx)]
    (-> terminal (.getTerminalSize) (.getRows))
    (-> ctx :persisted-hud :fov)))

(defn preserve [hud & seekers]
  (let [data (->> seekers
                  (map :lines)
                  (apply i/join-lines))]
    (-> hud
        (i/rebase #(into % data))
        (i/end-y))))

;; === Screen scrolling ===

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

(defn scroll-up [ctx]
  (update ctx :complete-hud #(scroll % upwards)))

(defn scroll-down [ctx]
  (update ctx :complete-hud #(scroll % downwards)))

(defn scroll-stop [ctx]
  (-> ctx
      (update :complete-hud noscroll)
      (update :persisted-hud noscroll)))

#_(defn jump [hud line]
    (assoc hud :lor (-> hud (i/height) (- line))))

;; === REPL ===

(defn roll [ctx f]
  (let [then-repl (-> ctx :repl f)
        then-seeker (r/then then-repl)]
    (assoc ctx
      :complete-hud (-> ctx (:persisted-hud) (i/join then-seeker))
      :repl then-repl
      :seeker then-seeker)))

(defn roll-back [ctx]
  (roll ctx r/travel-back))

(defn roll-forward [ctx]
  (roll ctx r/travel-forward))

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

(defn suggest [ctx]
  (let [suggestion (r/complete (:repl ctx) (:seeker ctx))]
    (-> ctx
        (update :complete-hud #(preserve % (i/seeker delimiter) suggestion))
        (update :complete-hud #(-> % i/end-y (i/move-y dec) i/end-x)))))

;; === Input ===
(defn capture [ctx stroke]
  (let [seeker (-> ctx (:seeker) (i/inputs stroke))]
    (assoc ctx
      :previous-hud (:complete-hud ctx)
      :complete-hud (-> ctx
                        (:persisted-hud)
                        (i/join seeker))
      :seeker seeker)))

(defn reformat [ctx]
  (let [formatted (-> ctx :seeker (f/lisp-format))
        joined (-> ctx (:persisted-hud) (i/join formatted))]
    (assoc ctx :complete-hud joined)))

;; === Rendering ===

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

;; === Control ===

(defn resize [ctx]
  (let [fov (get-in ctx [:persisted-hud :fov])
        ssize (screen-size ctx)]
    (if (not= ssize fov)
      (-> ctx
          (assoc-in [:persisted-hud :fov] ssize)
          (assoc-in [:complete-hud :fov] ssize))
      ctx)))

(defn navigate [ctx]
  (let [{{fov   :fov
          ov    :ov
          h     :height
          [_ y] :cursor} :complete-hud
         {ph :height}    :previous-hud} ctx
        upper-y (- @h fov ov)
        lower-y (- @h ov)
        nov (cond
              (< @h @ph) (-- ov @ph @h)
              (< y upper-y) (-- @h fov y)
              (> (inc y) lower-y) (-- @h (inc y))
              :else ov)]
    (-> ctx
        (assoc-in [:persisted-hud :ov] nov)
        (assoc-in [:complete-hud :ov] nov))))

(defn clear [ctx]
  (let [fov (screen-size ctx)
        start-hud (init-hud fov)]
    (assoc ctx
      :complete-hud (i/join start-hud (:seeker ctx))
      :persisted-hud start-hud
      :previous-hud start-hud)))

(defn refresh [ctx]
  (assoc ctx :complete-hud (i/join (:persisted-hud ctx) (:seeker ctx))))

(defn exit [ctx]
  (-> ctx
      (update :complete-hud #(preserve % (i/seeker goodbye)))
      (assoc-in [:persisted-hud :ov] 0)
      (assoc-in [:complete-hud :ov] 0)
      (render-context))
  (Thread/sleep 1200))

(defn deselect [ctx]
  (-> ctx
      (update :complete-hud i/deselect)
      (update :persisted-hud i/deselect)
      (update :seeker i/deselect)))

(defn failure [{:keys [message trace]} ctx]
  (let [element (first trace)
        msg (-> "Exception: %s, at %s.%s (%s:%s)"
                (format message
                        (.getClassName element)
                        (.getMethodName element)
                        (.getFileName element)
                        (.getLineNumber element))
                (i/str->lines))
        text (-> (i/join-lines error
                               msg
                               empty-line
                               caret)
                 (i/seeker))]
    (-> ctx
        (update :persisted-hud #(preserve % text))
        (update :complete-hud #(preserve % text))
        (assoc :seeker i/empty-seeker)
        (re-render))))

;; === Events ===

(defn movement? [stroke]
  (contains? #{:up :down :left :right} (:key stroke)))

(defn manipulation? [stroke]
  (and (contains? #{\v \c \x} (:key stroke))
       (:alt stroke)))

(defn handle [ctx stroke]
  (m/match [stroke]
           [{:key :tab}] (-> ctx (resize) (refresh) (reformat) (suggest) (scroll-stop) (deselect) (re-render))
           [{:key :page-up}] (-> ctx (resize) (scroll-up) (deselect) (re-render))
           [{:key :page-down}] (-> ctx (resize) (scroll-down) (deselect) (re-render))
           [{:key :up :alt true}] (-> ctx (resize) (roll-back) (reformat) (scroll-stop) (re-render))
           [{:key :down :alt true}] (-> ctx (resize) (roll-forward) (reformat) (scroll-stop) (re-render))
           [{:key \r :ctrl true}] (-> ctx (clear) (deselect) (re-render))
           [{:key \e :alt true}] (-> ctx (resize) (evaluate) (scroll-stop) (re-render))
           [{:key \d :ctrl true}] (-> ctx (resize) (scroll-stop) (deselect) (re-render) (exit))
           [_ :guard manipulation?] (-> ctx (resize) (capture stroke) (navigate) (reformat) (scroll-stop) (re-render))
           [_ :guard movement?] (-> ctx (resize) (capture stroke) (navigate) (reformat)  (scroll-stop) (min-render))
           :else (-> ctx (resize) (capture stroke) (navigate) (reformat) (scroll-stop) (diff-render))))

(defn read-eval-print [terminal repl]
  (let [start-hud (init-hud 0)
        eval-ctx (Context. terminal :total start-hud start-hud start-hud repl i/empty-seeker)]
    (loop [ctx (resize eval-ctx)]
      (when ctx
        (let [result (attempt
                       (render-context ctx)
                       (handle ctx (t/get-keystroke-blocking terminal)))]
          (m/match [result]
                   [{:status :success :val nctx}] (recur nctx)
                   [{:status :failure :val errs}] (recur (failure errs ctx))))))))
