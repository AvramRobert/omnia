(ns omnia.hud
  (use omnia.more)
  (require [lanterna.terminal :as t]
           [halfling.result :refer [attempt]]
           [omnia.rendering :refer [render-context]]
           [omnia.repl :as r]
           [omnia.input :as i]
           [clojure.core.match :as m]
           [omnia.formatting :as f]
           [omnia.highlight :refer [default-colourscheme]]))

(defrecord Context [terminal
                    render
                    previous-hud
                    persisted-hud
                    complete-hud
                    repl
                    seeker
                    raw-seeker
                    suggestion
                    highlights
                    garbage
                    colourscheme])

(def empty-set #{})
(def empty-line (i/seeker [i/empty-vec]))
(def delimiter (i/from-string "------"))
(def continuation (i/from-string "..."))
(def greeting (i/from-string "Welcome to the Omnia REPL! (ALPHA)"))
(def caret (i/from-string "Ω=>"))
(def goodbye (i/from-string "Bye..for now\nFor even the very wise cannot see all ends"))
(def error (i/from-string "I have not the heart to tell you, but something went wrong internally.."))

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
  (let [seeker (apply i/join-many prelude)]
    (-> seeker
        (i/end-y)
        (assoc :scroll? false)
        (assoc :lor (i/height seeker))
        (assoc :fov fov)
        (assoc :ov 0))))


(defn context [terminal hud repl colourscheme]
  (Context. terminal
            :total
            hud
            hud
            hud
            repl
            i/empty-seeker
            i/empty-seeker
            [i/empty-vec 0]
            empty-set
            empty-set
            colourscheme))

;; === Utils ===

(defn init-hud [fov]
  (hud fov greeting empty-line caret))

(defn screen-size [ctx]
  (if-let [terminal (:terminal ctx)]
    (-> terminal (.getTerminalSize) (.getRows))
    (-> ctx :persisted-hud :fov)))

(defn preserve [hud & seekers]
  (->> seekers (reduce i/join hud) (i/end-y)))

(defn adjoin [ths tht]
  (i/rebase ths #(concat % (:lines tht))))

(defn indent [hud n]
  (let [padding (repeat n \space)]
    (i/rebase hud (fn [lines] (map #(concat padding %) lines)))))

(defn auto-complete [seeker sgst]
  (if (empty? sgst)
    seeker
    (-> seeker
        (i/expand-word)
        (i/delete)
        (i/slicer #(concat sgst %))
        (i/move-x #(+ % (count sgst))))))

;; === Rendering ===

(defn re-render [ctx]
  (assoc ctx :render :total))

(defn diff-render [ctx]
  (assoc ctx :render :diff))

(defn no-render [ctx]
  (assoc ctx :render :nothing))

(defn highlight [ctx]
  (let [complete (:complete-hud ctx)]
    (update ctx :highlights
            #(if (i/selection? complete)
               (conj % (i/selection complete)) %))))

(defn gc [ctx]
  (assoc ctx :highlights empty-set
             :garbage (:highlights ctx)))

(defn parens-highlight [ctx]
  (letfn [(paint [& selections] (update ctx :highlights #(concat % selections)))
          (left [hud]
            (paint (-> hud i/select i/advance i/selection)
                   (-> hud i/advance i/expand-right i/regress i/select i/expand-right i/selection)))
          (right [hud]
            (paint (-> hud i/select i/advance i/selection)
                   (-> hud i/expand-left i/select i/advance i/selection)))]
    (m/match [(:complete-hud ctx)]
             [complete :guard #(contains? i/open-exps (i/center %))] (left complete)
             [complete :guard #(contains? i/closing-exps (i/center %))] (right complete)
             :else ctx)))

;; === Control ===

(defn resize [ctx]
  (let [fov   (get-in ctx [:persisted-hud :fov])
        ssize (screen-size ctx)]
    (if (not= ssize fov)
      (-> ctx
          (assoc-in [:persisted-hud :fov] ssize)
          (assoc-in [:complete-hud :fov] ssize))
      ctx)))

(defn calibrate [ctx]
  (let [{{fov   :fov
          ov    :ov
          h     :height
          [_ y] :cursor} :complete-hud
         {ph :height}    :previous-hud} ctx
        upper-y (- @h fov ov)
        lower-y (- @h ov)
        nov     (cond
                  (< y upper-y) (-- @h fov y)
                  (> (inc y) lower-y) (-- @h (inc y))
                  (= y (dec @h)) ov
                  (and (> @h fov) (< @h @ph)) (++ ov (- @h @ph))
                  (> @h fov) (++ ov (- @h @ph))
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

(defn rebase
  ([ctx] (rebase ctx (:seeker ctx)))
  ([ctx seeker]
   (->> seeker
        (i/join (:persisted-hud ctx))
        (assoc ctx :complete-hud))))

(defn exit [ctx]
  (-> ctx
      (update :complete-hud #(preserve % goodbye))
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
                    (i/from-string))]
    (-> ctx
        (update :persisted-hud #(preserve % (:seeker ctx) error msg empty-line caret))
        (update :complete-hud #(-> % (preserve error msg empty-line caret) (i/start-x)))
        (assoc :seeker i/empty-seeker)
        (re-render))))

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
                       (preserve (r/result evaluation) caret)
                       (i/start-x))]
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
        (update :complete-hud #(-> % (preserve delimiter suggestion) (i/end))))))

(defn- paginate [suggestions sgst-idx]
  (let [per-page     10
        nxt-sgst-idx (inc sgst-idx)
        hs           (i/height suggestions)
        dots         (cond
                       (<= hs per-page) i/empty-seeker
                       (< (inc sgst-idx) hs) continuation
                       :else i/empty-seeker)]
    (-> suggestions
        (i/rebase #(cond->> %
                            (> nxt-sgst-idx per-page) (drop (- nxt-sgst-idx per-page))
                            :always (take per-page)))
        (adjoin dots)
        (indent 1)
        (i/move-y #(if (>= % per-page) (dec per-page) %)))))

(defn suggestion-window [ctx suggestions]
  (let [{persisted    :persisted-hud
         seeker       :seeker
         [_ sgst-idx] :suggestion} ctx
        completed (auto-complete seeker (i/line suggestions))
        paginated (paginate suggestions sgst-idx)
        ph        (i/height paginated)
        top       (i/peer completed (fn [l [x & _]] (conj l x)))
        bottom    (i/peer completed (fn [_ [_ & r]] (drop (+ ph 2) r)))]
    (-> persisted
        (i/join top)
        (i/join delimiter)
        (i/end-y)
        (i/join paginated)
        (i/join delimiter)
        (i/end-x)
        (adjoin bottom)
        (assoc :ov (-- (i/height bottom) ph)))))

(defn complete [ctx]
  (let [{seeker       :seeker
         repl         :repl
         [_ sgst-idx] :suggestion} ctx
        suggestions (-> (r/suggest repl seeker)
                        (i/reset-y sgst-idx)
                        (i/end-x))
        suggestion  (i/line suggestions)
        nidx        (-> sgst-idx (inc) (mod* (i/height suggestions)))]
    (-> ctx
        (assoc :previous-hud (:complete-hud ctx))
        (assoc :suggestion [suggestion nidx])
        (assoc :complete-hud (suggestion-window ctx suggestions)))))

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

;; === Events ===

(defn movement? [stroke]
  (contains? #{:up :down :left :right} (:key stroke)))

(defn handle [ctx stroke]
  (m/match [stroke]
           [{:key \p :ctrl true :alt true}] (-> ctx (gc) (resize) (scroll-stop) (deselect) (parens-highlight) (re-render))
           [{:key :tab}] (-> ctx (gc) (resize) (rebase) (complete) (scroll-stop) (deselect) (highlight) (re-render))
           [{:key :page-up}] (-> ctx (gc) (resize) (scroll-up) (deselect) (highlight) (re-render))
           [{:key :page-down}] (-> ctx (gc) (resize) (scroll-down) (deselect) (highlight) (re-render))
           [{:key :up :alt true}] (-> ctx (gc) (resize) (uncomplete) (roll-back) (highlight) (scroll-stop) (re-render))
           [{:key :down :alt true}] (-> ctx (gc) (resize) (uncomplete) (roll-forward) (highlight) (scroll-stop) (re-render))
           [{:key \l :ctrl true :alt true}] (-> ctx (resize) (reformat) (highlight) (scroll-stop) (diff-render))
           [{:key \r :ctrl true}] (-> ctx (gc) (resize) (clear) (uncomplete) (deselect) (highlight) (re-render))
           [{:key \e :alt true}] (-> ctx (gc) (resize) (uncomplete) (evaluate) (highlight) (scroll-stop) (re-render))
           [{:key \d :ctrl true}] (-> ctx (gc) (resize) (uncomplete) (scroll-stop) (deselect) (highlight) (re-render) (exit))
           [_ :guard movement?] (-> ctx (gc) (resize) (uncomplete) (capture stroke) (calibrate) (highlight) (scroll-stop) (no-render))
           :else (-> ctx (gc) (resize) (uncomplete) (capture stroke) (calibrate) (highlight) (scroll-stop) (diff-render))))

(defn read-eval-print [terminal repl]
  (let [start-hud (init-hud 0)
        eval-ctx  (context terminal start-hud repl default-colourscheme)]
    (loop [ctx (resize eval-ctx)]
      (when ctx
        (let [result (attempt
                       (render-context ctx)
                       (handle ctx (t/get-keystroke-blocking terminal)))]
          (m/match [result]
                   [{:status :success :val nctx}] (recur nctx)
                   [{:status :failure :val errs}] (recur (failure errs ctx))))))))
