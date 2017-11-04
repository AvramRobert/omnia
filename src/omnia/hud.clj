(ns omnia.hud
  (require [halfling.task :as tsk]
           [omnia.render :refer [render top-y bottom-y project-hud project-cursor]]
           [omnia.repl :as r]
           [omnia.input :as i]
           [omnia.config :refer [with-features keymap]]
           [clojure.core.match :as m]
           [omnia.format :as f]
           [omnia.terminal :as t]
           [omnia.more :refer [-- ++ inc< dec< mod*]]))

(defrecord Context [terminal
                    repl
                    keymap
                    colourscheme
                    render
                    previous-hud
                    persisted-hud
                    complete-hud
                    seeker
                    suggestions
                    highlights
                    garbage])

(defmacro omnia-version []
  (System/getProperty "omnia.version"))

(def empty-set #{})
(def empty-line (i/seeker [i/empty-vec]))

(def clj-version (i/from-string (format "-- Clojure v%s --" (clojure-version))))
(def java-version (i/from-string (format "-- Java v%s --" (System/getProperty "java.version"))))
(def delimiter (i/from-string "------"))
(def continuation (i/from-string "..."))
(def greeting (i/from-string (format "Welcome to Omnia! (BETA v%s)" (omnia-version))))
(def caret (i/from-string "Ω =>"))
(def goodbye (i/from-string "Bye..for now\nFor even the very wise cannot see all ends"))
(defn nrepl-info [host port] (i/from-string (str "-- nREPL server started on nrepl://" host ":" port " --")))

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

(defn init-hud [{:keys [terminal repl]}]
  (let [fov       (t/size terminal)
        repl-info (nrepl-info (:host repl) (:port repl))]
    (hud fov
         greeting
         repl-info
         clj-version
         java-version
         empty-line
         caret)))

(defn context [{:keys [terminal repl keymap colourscheme] :as config}]
  (assert (not (nil? terminal)) "Please provide a proper terminal (look in omnia.terminal)")
  (assert (not (nil? repl)) "Please provide a proper repl (look in omnia.repl)")
  (assert (not (nil? keymap)) "Please provide a proper keymap (look in omnia.config)")
  (assert (not (nil? colourscheme)) "Please provide a proper colourscheme (look in omnia.config)")
  (let [hud (init-hud config)]
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
      i/empty-seeker
      empty-set
      empty-set)))

(defn adjoin [ths tht]
  (i/rebase ths #(concat % (:lines tht))))

(defn indent [hud n]
  (let [padding (repeat n \space)]
    (i/rebase hud (fn [lines] (mapv #(vec (concat padding %)) lines)))))

(defn correct-ov
  ([hud] (correct-ov hud hud))
  ([hud previous-hud]
   (let [{fov   :fov
          ov    :ov
          h     :height
          [_ y] :cursor} hud
         {pfov :fov
          ph   :height} previous-hud
         upper-y     (top-y hud)                            ;; the top viewable y
         lower-y     (bottom-y hud)                         ;; the lower viewable y
         over-upper? (< y upper-y)
         over-lower? (> y lower-y)
         at-lower?   (= y lower-y)
         smaller?    (< h ph)
         larger?     (> h ph)
         unpaged?    (and (<= h fov)
                          (<= ph fov))
         resized?    (and (not= pfov fov)
                          (not= 0 ov))]
     (cond
       resized? (++ ov (- pfov fov))                        ;; we've changed the terminal size
       unpaged? ov                                          ;; we've not exceeded the fov
       (and larger? at-lower?) ov                           ;; we've gotten bigger but we're still at the bottom
       (or larger? smaller?) (++ ov (- h ph))               ;; we've changed in size
       over-upper? (++ ov (- upper-y y))                    ;; we've exceeded the upper bound
       over-lower? (-- ov (- y lower-y))                    ;; we've exceeded the lower bound
       :else ov))))

(defn riffle [suggestions]
  (let [{[_ y]  :cursor
         height :height} suggestions
        y'      (mod* (inc y) height)
        correct #(assoc % :ov (correct-ov %))]
    (-> suggestions (i/reset-y y') (correct))))

(defn rebase
  ([ctx]
   (rebase ctx (:seeker ctx)))
  ([ctx seeker]
   (assoc ctx :complete-hud (i/join (:persisted-hud ctx) seeker))))

(defn preserve [ctx & seekers]
  (update ctx :complete-hud #(reduce i/join % seekers)))

(defn persist
  ([ctx] (persist ctx (:complete-hud ctx)))
  ([ctx hud] (assoc ctx :persisted-hud hud)))

(defn remember [ctx]
  (assoc ctx :previous-hud (:complete-hud ctx)))

(defn seek [ctx seeker]
  (->> (get-in ctx [:seeker :clipboard])
       (or (:clipboard seeker))
       (assoc seeker :clipboard)
       (assoc ctx :seeker)))

(defn re-suggest [ctx suggestions]
  (assoc ctx :suggestions suggestions))

(defn un-suggest [ctx]
  (assoc ctx :suggestions i/empty-seeker))

(defn auto-complete [ctx]
  (let [{seeker      :seeker
         suggestions :suggestions} ctx
        sgst (i/line suggestions)]
    (seek ctx
          (if (empty? (:lines suggestions))
            seeker
            (-> seeker
                (i/expand-word)
                (i/delete)
                (i/slicer #(concat sgst %))
                (i/move-x #(+ % (count sgst)))
                (i/deselect))))))

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

(defn match [ctx]
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

(defn calibrate [ctx]
  (let [nov (correct-ov (:complete-hud ctx)
                        (:previous-hud ctx))]
    (-> ctx
        (assoc-in [:persisted-hud :ov] nov)
        (assoc-in [:complete-hud :ov] nov))))

(defn resize [ctx]
  (let [{persisted :persisted-hud
         terminal  :terminal} ctx
        fov     (:fov persisted)
        new-fov (t/size terminal)]
    (if (not= new-fov fov)
      (->> (assoc persisted :fov new-fov :lor new-fov)
           (persist ctx)
           (rebase)
           (calibrate)
           (remember)
           (re-render))
      ctx)))

(defn clear [ctx]
  (let [fov       (get-in ctx [:persisted-hud :fov])
        start-hud (init-hud ctx)]
    (-> (remember ctx)
        (persist start-hud)
        (rebase (:seeker ctx))
        (un-suggest))))

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

;; === REPL ===

(defn roll [ctx f]
  (let [then-repl   (-> ctx :repl f)
        then-seeker (i/end (r/then then-repl))]
    (-> (remember ctx)
        (rebase then-seeker)
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

(defn- paginate [ctx]
  (letfn [(project [hud]
            (-> (i/move hud (constantly (project-cursor hud)))
                (project-hud)))]
    (as-> (:suggestions ctx) page
          (project page)
          (if (not= 0 (:ov page))
            (adjoin page continuation)
            page)
          (indent page 1))))

(defn suggestion-window [ctx]
  (let [completed (:seeker (auto-complete ctx))
        paginated (paginate ctx)
        ph        (i/height paginated)
        top       (i/peer completed (fn [l [x & _]] (conj l x)))
        bottom    (i/peer completed (fn [_ [_ & r]] (drop (+ ph 2) r)))]
    (rebase ctx
            (-> i/empty-seeker
                (i/join top)
                (i/join delimiter)
                (i/join paginated)
                (i/end-x)
                (adjoin delimiter)
                (adjoin bottom)
                (assoc :ov (-- (i/height bottom) ph))))))

(defn suggest [ctx]
  (let [{seeker      :seeker
         repl        :repl
         suggestions :suggestions} ctx
        correct     #(assoc % :ov (correct-ov %))
        suggestions (if (empty? (:lines suggestions))
                      (->> (r/suggest repl seeker)
                           (hud 10)
                           (i/start)
                           (i/end-x)
                           (correct))
                      (riffle suggestions))]
    (-> (remember ctx)
        (re-suggest suggestions)
        (suggestion-window))))

(defn complete [ctx]
  (-> (auto-complete ctx)
      (un-suggest)))

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
    :match (-> ctx (gc) (scroll-stop) (deselect) (match) (diff-render) (resize) (continue))
    :suggest (-> ctx (gc) (suggest) (scroll-stop) (deselect) (highlight) (diff-render) (resize) (continue))
    :scroll-up (-> ctx (gc) (scroll-up) (deselect) (highlight) (re-render) (resize) (continue))
    :scroll-down (-> ctx (gc) (scroll-down) (deselect) (highlight) (re-render) (resize) (continue))
    :prev-eval (-> ctx (gc) (complete) (roll-back) (highlight) (scroll-stop) (diff-render) (resize) (continue))
    :next-eval (-> ctx (gc) (complete) (roll-forward) (highlight) (scroll-stop) (diff-render) (resize) (continue))
    :format (-> ctx (complete) (reformat) (highlight) (scroll-stop) (diff-render) (resize) (continue))
    :clear (-> ctx (gc) (clear) (deselect) (highlight) (re-render) (resize) (continue))
    :eval (-> ctx (gc) (complete) (evaluate) (highlight) (scroll-stop) (diff-render) (resize) (continue))
    :exit (-> ctx (gc) (scroll-stop) (deselect) (highlight) (diff-render) (resize) (exit) (terminate))
    (-> ctx (gc) (complete) (capture event) (calibrate) (highlight) (scroll-stop) (diff-render) (resize) (continue))))

(defn match-stroke [{:keys [keymap]} stroke]
  (let [key    (:key stroke)
        action (get keymap stroke)]
    (m/match [action (char? key)]
             [nil true] (i/->Event :char key)
             [nil false] (i/->Event :none key)
             [_ _] (i/->Event action key))))

(defn iread [ctx]
  (tsk/task (t/keystroke! (:terminal ctx))))

(defn ieval [ctx stroke]
  (tsk/task (process ctx (match-stroke ctx stroke))))

(defn sleep [msecs]
  (tsk/task (Thread/sleep msecs)))

(defn read-eval-print [config]
  (loop [task (-> config (with-features) (context) (continue) (tsk/success))]
    (m/match [@task]
             [[:continue ctx]] (-> (tsk/task (render ctx))
                                   (tsk/then-do (iread ctx))
                                   (tsk/then #(ieval ctx %))
                                   (tsk/run)
                                   (recur))
             [[:terminate ctx]] (-> (tsk/task (render ctx))
                                    (tsk/then-do (sleep 1200))
                                    (tsk/then-do ctx)
                                    (tsk/run))
             :else task)))