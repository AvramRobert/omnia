(ns omnia.hud
  (use omnia.more)
  (require [lanterna.terminal :as t]
           [halfling.result :refer [attempt]]
           [omnia.rendering :refer [render-context]]
           [omnia.repl :as r]
           [omnia.input :as i]
           [clojure.core.match :as m]
           [omnia.formatting :as f]))

(defrecord Context [terminal
                    render
                    previous-hud
                    persisted-hud
                    complete-hud
                    repl
                    seeker
                    raw-seeker
                    suggestion])

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


(defn context [terminal hud repl]
  (Context. terminal :total hud hud hud repl i/empty-seeker i/empty-seeker [i/empty-vec 0]))

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

(defn auto-complete [seeker sgst]
  (if (empty? sgst)
    seeker
    (-> seeker
        (i/expand-word)
        (i/delete)
        (i/slicer #(concat sgst %))
        (i/move-x #(+ % (count sgst))))))

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
  (let [then-repl   (-> ctx :repl f)
        then-seeker (r/then then-repl)]
    (assoc ctx
      :complete-hud (-> ctx (:persisted-hud) (i/join then-seeker))
      :repl then-repl
      :seeker then-seeker
      :raw-seeker then-seeker)))

(defn roll-back [ctx]
  (roll ctx r/travel-back))

(defn roll-forward [ctx]
  (roll ctx r/travel-forward))

(defn evaluate [ctx]
  (let [evaluation (r/evaluate (:repl ctx) (:raw-seeker ctx))
        persisted  (-> ctx
                       (:complete-hud)
                       (preserve (r/result evaluation)
                                 (i/seeker caret))
                       (i/move-x (fn [_] 0)))]
    (assoc ctx
      :previous-hud (:complete-hud ctx)
      :persisted-hud persisted
      :complete-hud persisted
      :repl evaluation
      :seeker i/empty-seeker
      :raw-seeker i/empty-seeker)))

(defn suggest [ctx]
  (let [suggestion (r/suggest (:repl ctx) (:seeker ctx))]
    (-> ctx
        (assoc :previous-hud (:complete-hud ctx))
        (update :complete-hud #(-> % (preserve (i/seeker delimiter) suggestion) (i/end))))))

(defn paginate [ctx suggestions]
  (let [sgst-idx (-> ctx :suggestion second inc)
        fov      (get-in ctx [:persisted-hud :fov])
        h-seeker (-> ctx :seeker i/height)
        space    (- fov h-seeker 2)]
    (i/rebase suggestions
              #(cond->> %
                        (> sgst-idx space) (drop (- sgst-idx space))
                        :always (take space)))))

(defn complete [ctx]
  (let [{persisted    :persisted-hud
         seeker       :seeker
         repl         :repl
         [_ sgst-idx] :suggestion} ctx
        suggestions (-> (r/suggest repl seeker)
                        (i/reset-y sgst-idx)
                        (i/end-x))
        suggestion  (i/line suggestions)
        seeker      (auto-complete seeker suggestion)
        nidx        (-> sgst-idx (inc) (mod* (i/height suggestions)))]
    (-> ctx
        (assoc :previous-hud (:complete-hud ctx))
        (assoc :suggestion [suggestion nidx])
        (assoc :complete-hud (-> persisted
                                 (i/join seeker)
                                 (preserve (i/seeker delimiter))
                                 (i/join (paginate ctx suggestions)))))))

(defn uncomplete [ctx]
  (let [[sgst _] (:suggestion ctx)]
    (-> ctx
        (update :seeker #(auto-complete % sgst))
        (update :raw-seeker #(auto-complete % sgst))
        (assoc :suggestion [i/empty-vec 0]))))

;; === Input ===
(defn capture [ctx stroke]
  (let [seeker     (-> ctx (:seeker) (i/inputs stroke))
        raw-seeker (-> ctx (:raw-seeker) (i/inputs stroke))]
    (assoc ctx
      :previous-hud (:complete-hud ctx)
      :complete-hud (-> ctx
                        (:persisted-hud)
                        (i/join seeker))
      :seeker seeker
      :raw-seeker raw-seeker)))

(defn reformat [ctx]
  (let [formatted (-> ctx :raw-seeker (f/lisp-format))
        joined    (-> ctx (:persisted-hud) (i/join formatted))]
    (assoc ctx :complete-hud joined
               :seeker formatted)))

;; === Rendering ===

(defn re-render [ctx]
  (assoc ctx :render :total))

(defn diff-render [ctx]
  (assoc ctx :render :diff))

(defn input-render [ctx]
  (assoc ctx :render :input))

(defn no-render [ctx]
  (assoc ctx :render :nothing))

(defn opt-render [ctx]
  (let [{complete :previous-hud
         [sgst _] :suggestion} ctx]
    (if (i/selection? complete)
      (input-render ctx)
      (diff-render ctx))))

;; === Control ===

(defn resize [ctx]
  (let [fov   (get-in ctx [:persisted-hud :fov])
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
        nov     (cond
                  (< @h @ph) (-- ov @ph @h)
                  (< y upper-y) (-- @h fov y)
                  (> (inc y) lower-y) (-- @h (inc y))
                  :else ov)]
    (-> ctx
        (assoc-in [:persisted-hud :ov] nov)
        (assoc-in [:complete-hud :ov] nov))))

(defn clear [ctx]
  (let [fov       (screen-size ctx)
        start-hud (init-hud fov)]
    (assoc ctx
      :complete-hud (i/join start-hud (:seeker ctx))
      :persisted-hud start-hud
      :previous-hud start-hud)))

(defn rebase [ctx]
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
        msg     (-> "Exception: %s, at %s.%s (%s:%s)"
                    (format message
                            (.getClassName element)
                            (.getMethodName element)
                            (.getFileName element)
                            (.getLineNumber element))
                    (i/str->lines))
        text    (-> (i/join-lines error
                                  msg
                                  empty-line
                                  caret)
                    (i/seeker))]
    (-> ctx
        (update :persisted-hud #(preserve % (:seeker ctx) text))
        (update :complete-hud #(-> % (preserve text) (i/start-x)))
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
           [{:key :tab}] (-> ctx (resize) (rebase) (complete) (scroll-stop) (deselect) (re-render))
           [{:key :page-up}] (-> ctx (resize) (scroll-up) (deselect) (re-render))
           [{:key :page-down}] (-> ctx (resize) (scroll-down) (deselect) (re-render))
           [{:key :up :alt true}] (-> ctx (resize) (uncomplete) (roll-back) (scroll-stop) (re-render))
           [{:key :down :alt true}] (-> ctx (resize) (uncomplete) (roll-forward) (scroll-stop) (re-render))
           [{:key \l :ctrl true :alt true}] (-> ctx (resize) (reformat) (scroll-stop) (deselect) (diff-render))
           [{:key \r :ctrl true}] (-> ctx (clear) (uncomplete) (deselect)  (re-render))
           [{:key \e :alt true}] (-> ctx (resize) (uncomplete) (evaluate) (scroll-stop) (re-render))
           [{:key \d :ctrl true}] (-> ctx (resize) (uncomplete) (scroll-stop) (deselect) (re-render) (exit))
           [_ :guard manipulation?] (-> ctx (resize) (uncomplete) (capture stroke) (navigate) (scroll-stop) (re-render))
           [_ :guard movement?] (-> ctx (resize) (uncomplete) (capture stroke) (navigate) (scroll-stop) (opt-render))
           :else (-> ctx (resize) (uncomplete) (capture stroke) (navigate) (scroll-stop) (diff-render))))

(defn read-eval-print [terminal repl]
  (let [start-hud (init-hud 0)
        eval-ctx  (context terminal start-hud repl)]
    (loop [ctx (resize eval-ctx)]
      (when ctx
        (let [result (attempt
                       (render-context ctx)
                       (handle ctx (t/get-keystroke-blocking terminal)))]
          (m/match [result]
                   [{:status :success :val nctx}] (recur nctx)
                   [{:status :failure :val errs}] (recur (failure errs ctx))))))))
