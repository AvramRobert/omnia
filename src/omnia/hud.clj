(ns omnia.hud
  (require [lanterna.terminal :as t]
           [halfling.result :as res]
           [halfling.task :as tsk]
           [omnia.rendering :refer [render]]
           [omnia.repl :as r]
           [omnia.input :as i]
           [omnia.config :refer [match-stroke with-features]]
           [clojure.core.match :as m]
           [omnia.formatting :as f]
           [omnia.highlight :refer [default-colourscheme]]
           [omnia.more :refer [-- ++ inc< dec< mod*]])
  (:import (halfling.result Result)))

(defrecord Context [terminal
                    repl
                    keymap
                    colourscheme
                    render
                    previous-hud
                    persisted-hud
                    complete-hud
                    seeker
                    suggestion
                    highlights
                    garbage])

(defmacro omnia-version []
  (System/getProperty "omnia.version"))

(def empty-set #{})
(def empty-line (i/seeker [i/empty-vec]))
(def no-suggestion [i/empty-vec 0])

(def clj-version (i/from-string (format "-- Clojure v%s --" (clojure-version))))
(def java-version (i/from-string (format "-- Java v%s --" (System/getProperty "java.version"))))
(def delimiter (i/from-string "------"))
(def continuation (i/from-string "..."))
(def greeting (i/from-string (format "Welcome to Omnia! (ALPHA v%s)" (omnia-version))))
(def caret (i/from-string "Ω =>"))
(def goodbye (i/from-string "Bye..for now\nFor even the very wise cannot see all ends"))

;; === Utils ===

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
        (i/join i/empty-seeker)
        (assoc :scroll? false)
        (assoc :lor fov)
        (assoc :fov fov)
        (assoc :ov 0))))

(defn init-hud [fov]
  (hud fov
       greeting
       clj-version
       java-version
       empty-line
       caret))

(defn get-screen-size [terminal alt]
  (or (some-> terminal (.getTerminalSize) (.getRows))
      alt))

(defn context [{:keys [terminal repl keymap colourscheme]}]
  (let [hud (init-hud (get-screen-size terminal 0))]
    (Context.
      terminal
      repl
      keymap
      colourscheme
      :total
      hud
      hud
      hud
      i/empty-seeker
      no-suggestion
      empty-set
      empty-set)))

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

(defn rebase
  ([ctx]
   (rebase ctx (:seeker ctx)))
  ([ctx seeker]
   (assoc ctx :complete-hud (i/join (:persisted-hud ctx) seeker))))

(defn preserve [ctx & seekers]
  (update ctx :complete-hud #(reduce i/join % seekers)))

(defn persist [ctx]
  (assoc ctx :persisted-hud (:complete-hud ctx)))

(defn remember [ctx]
  (assoc ctx :previous-hud (:complete-hud ctx)))

(defn seek [ctx seeker]
  (->> (get-in ctx [:seeker :clipboard])
       (or (:clipboard seeker))
       (assoc seeker :clipboard)
       (assoc ctx :seeker)))

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

(defn matching [ctx]
  (if-let [{[xs ys] :start
            [xe ye] :end} (-> (:complete-hud ctx) (i/find-pair))]
    (update ctx :highlights
            #(conj % {:start [xs ys] :end [(inc xs) ys]} {:start [xe ye] :end [(inc xe) ye]}))
    ctx))

;; === Control ===

(defn continue [ctx]
  [:continue ctx])

(defn terminate [ctx]
  [:terminate ctx])

(defn resize [ctx]
  (let [fov (get-in ctx [:persisted-hud :fov])
        ssize (-> ctx (:terminal) (get-screen-size fov))]
    (if (not= ssize fov)
      (-> ctx
          (assoc-in [:persisted-hud :fov] ssize)
          (assoc-in [:complete-hud :fov] ssize))
      ctx)))

(defn calibrate [ctx]
  (let [{{fov :fov
          ov :ov
          h :height
          [_ y] :cursor} :complete-hud
         {ph :height} :previous-hud} ctx
        upper-y (- h fov ov)                                ;; the top viewable y
        lower-y (dec (- h ov))                              ;; the lower viewable y
        over-upper? (< y upper-y)
        over-lower? (> y lower-y)
        at-lower? (= y lower-y)
        smaller? (< h ph)
        larger? (> h ph)
        unpaged? (and (<= h fov)
                      (<= ph fov))
        nov (cond
              unpaged? ov                                   ;; we've not exceeded the fov
              (and larger? at-lower?) ov                    ;; we've gotten bigger but we're still at the bottom
              (or larger? smaller?) (++ ov (- h ph))        ;; we've changed in size
              over-upper? (inc ov)                          ;; we've exceeded the upper bound
              over-lower? (dec ov)                          ;; we've exceed the lower bound
              :else ov)]
    (-> ctx
        (assoc-in [:persisted-hud :ov] nov)
        (assoc-in [:complete-hud :ov] nov))))

(defn clear [ctx]
  (let [fov (get-in ctx [:persisted-hud :fov])
        start-hud (-> ctx (:terminal) (get-screen-size fov) (init-hud))]
    (assoc ctx
      :complete-hud (i/join start-hud (:seeker ctx))
      :persisted-hud start-hud
      :previous-hud start-hud)))

(defn exit [ctx]
  (-> (preserve ctx goodbye)
      (assoc-in [:persisted-hud :ov] 0)
      (assoc-in [:complete-hud :ov] 0)))

(defn deselect [ctx]
  (-> ctx
      (update :complete-hud i/deselect)
      (update :persisted-hud i/deselect)
      (update :seeker i/deselect)))

;; === Screen scrolling ===

(defn nowards [{:keys [height fov ov]}]
  (if (> height fov) (+ fov ov) height))

(defn upwards [{:keys [height lor]}]
  (inc< lor height))

(defn downwards [{:keys [lor] :as hud}]
  (dec< lor (nowards hud)))

(defn scroll [hud f]
  (-> hud
      (assoc :scroll? true)
      (assoc :lor (f hud))))

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
    (-> (rebase ctx then-seeker)
        (seek then-seeker)
        (assoc :repl then-repl))))

(defn roll-back [ctx]
  (roll ctx r/travel-back))

(defn roll-forward [ctx]
  (roll ctx r/travel-forward))

(defn evaluate [ctx]
  (let [evaluation (r/evaluate (:repl ctx) (:seeker ctx))
        result     (r/result evaluation)]
    (-> (remember ctx)
        (preserve result caret i/empty-seeker)
        (persist)
        (seek i/empty-seeker)
        (assoc :repl evaluation))))

(defn- paginate [suggestions sgst-idx]
  (let [per-page 10
        nxt-sgst-idx (inc sgst-idx)
        hs (i/height suggestions)
        dots (cond
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
  (let [{seeker :seeker
         [_ sgst-idx] :suggestion} ctx
        completed (auto-complete seeker (i/line suggestions))
        paginated (paginate suggestions sgst-idx)
        ph (i/height paginated)
        top (i/peer completed (fn [l [x & _]] (conj l x)))
        bottom (i/peer completed (fn [_ [_ & r]] (drop (+ ph 2) r)))]
    (-> i/empty-seeker
        (i/join top)
        (i/join delimiter)
        (i/join paginated)
        (i/end-x)
        (adjoin delimiter)
        (adjoin bottom)
        (assoc :ov (-- (i/height bottom) ph)))))

(defn suggest [ctx]
  (let [{seeker :seeker
         repl :repl
         [_ sgst-idx] :suggestion} ctx
        suggestions (-> (r/suggest repl seeker)
                        (i/reset-y sgst-idx)
                        (i/end-x))
        suggestion (i/line suggestions)
        nidx (-> sgst-idx (inc) (mod* (i/height suggestions)))]
    (-> (remember ctx)
        (rebase (suggestion-window ctx suggestions))
        (assoc :suggestion [suggestion nidx]))))

(defn complete [ctx]
  (let [[sgst _] (:suggestion ctx)]
    (-> ctx
        (update :seeker #(auto-complete % sgst))
        (assoc :suggestion [i/empty-vec 0]))))

;; === Input ===

(defn capture [ctx event]
  (let [seeker (-> ctx (:seeker) (i/process event))]
    (-> (remember ctx)
        (rebase seeker)
        (seek seeker))))

(defn reformat [ctx]
  (let [formatted (-> ctx (:seeker) (f/lisp-format))]
    (-> (remember ctx)
        (rebase formatted)
        (seek formatted))))

;; === Events ===

(defn process [ctx event]
  (case (:action event)
    :match (-> ctx (gc) (resize) (scroll-stop) (deselect) (matching) (re-render) (continue))
    :suggest (-> ctx (gc) (resize) (rebase) (suggest) (scroll-stop) (deselect) (highlight) (re-render) (continue))
    :scroll-up (-> ctx (gc) (resize) (scroll-up) (deselect) (highlight) (re-render) (continue))
    :scroll-down (-> ctx (gc) (resize) (scroll-down) (deselect) (highlight) (re-render) (continue))
    :prev-eval (-> ctx (gc) (resize) (complete) (roll-back) (highlight) (scroll-stop) (re-render) (continue))
    :next-eval (-> ctx (gc) (resize) (complete) (roll-forward) (highlight) (scroll-stop) (re-render) (continue))
    :reformat (-> ctx (resize) (complete) (reformat) (highlight) (scroll-stop) (diff-render) (continue))
    :clear (-> ctx (gc) (resize) (clear) (complete) (deselect) (highlight) (re-render) (continue))
    :eval (-> ctx (gc) (resize) (complete) (evaluate) (highlight) (scroll-stop) (re-render) (continue))
    :exit (-> ctx (gc) (resize) (complete) (scroll-stop) (deselect) (highlight) (re-render) (exit) (terminate))
    (-> ctx (gc) (resize) (complete) (capture event) (calibrate) (highlight) (scroll-stop) (diff-render) (continue))))

(defn iread [ctx]
  (tsk/task (t/get-keystroke-blocking (:terminal ctx))))

(defn ieval [ctx stroke]
  (tsk/task (process ctx (match-stroke ctx stroke))))

(defn sleep [msecs]
  (tsk/task (Thread/sleep msecs)))

(defn read-eval-print [config]
  (loop [result (-> config (with-features) (context) (continue) (res/success))]
    (if (res/success? result)
      (let [[stage ctx] (res/get! result)]
        (case stage
          :continue (-> (tsk/task (render ctx))
                        (tsk/then (fn [_] (iread ctx)))
                        (tsk/then (fn [s] (ieval ctx s)))
                        (tsk/run)
                        (recur))
          :terminate (-> (tsk/task (render ctx))
                         (tsk/then (fn [_] (sleep 1200)))
                         (tsk/then (fn [_] ctx))
                         (tsk/run)
                         (tsk/from-result))))
      (tsk/from-result result))))