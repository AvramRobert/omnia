(ns omnia.hud
  (:require [halfling.task :as tsk]
            [omnia.render :refer [render
                                  top-y
                                  bottom-y
                                  project-hud
                                  project-cursor
                                  select-cs
                                  clean-cs
                                  simple-scheme]]
            [omnia.repl :as r]
            [omnia.input :as i]
            [clojure.core.match :as m]
            [omnia.format :as f]
            [omnia.terminal :as t]
            [omnia.more :refer [-- ++ inc< dec< mod* omnia-version map-vals]]))

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
                    docs
                    highlights
                    garbage])

(def empty-line (i/seeker [i/empty-vec]))

(def clj-version (i/from-string (format "-- Clojure v%s --" (clojure-version))))
(def java-version (i/from-string (format "-- Java v%s --" (System/getProperty "java.version"))))
(def delimiter (i/from-string "------"))
(def continuation (i/from-string "..."))
(def greeting (i/from-string (format "Welcome to Omnia! (Ω) v%s" (omnia-version))))
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

(defn empty-hud [{:keys [terminal]}]
  (hud (t/size terminal)))

(defn context [{:keys [terminal repl keymap colourscheme] :as config}]
  (assert (not (nil? terminal)) "Please provide a proper terminal (look in omnia.terminal)")
  (assert (not (nil? repl)) "Please provide a proper repl (look in omnia.repl)")
  (assert (not (nil? keymap)) "Please provide a proper keymap (look in omnia.config)")
  (assert (not (nil? colourscheme)) "Please provide a proper colourscheme (look in omnia.config)")
  (let [ehud (empty-hud config)
        hud  (init-hud config)]
    (map->Context
      {:terminal      terminal
       :repl          repl
       :keymap        keymap
       :colourscheme  colourscheme
       :render        :diff
       :previous-hud  ehud
       :persisted-hud hud
       :complete-hud  hud
       :seeker        i/empty-seeker
       :suggestions   i/empty-seeker
       :docs          i/empty-seeker
       :highlights    i/empty-vec
       :garbage       i/empty-vec})))

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

(defn paginate [hud]
  (letfn [(project [hud]
            (->> (project-cursor hud)
                 (constantly)
                 (i/move hud)
                 (project-hud)))]
    (as-> hud page
          (project page)
          (if (not= 0 (:ov page))
            (adjoin page continuation)
            page)
          (indent page 1))))

(defn pop-up-riffle [ctx hud]
  (let [seeker    (:seeker ctx)
        paginated (paginate hud)
        ph        (:height hud)
        top       (-> seeker (i/peer (fn [l [x & _]] (conj l x))))
        bottom    (-> seeker (i/peer (fn [_ [_ & r]] (drop (+ ph 2) r))))]
    (rebase ctx (-> (i/join-many top delimiter paginated)
                    (i/end-x)
                    (adjoin delimiter)
                    (adjoin bottom)
                    (assoc :ov (-- (:height bottom) ph))))))

(defn pop-up-static [ctx hud]
  (let [seeker    (:seeker ctx)
        paginated (paginate hud)
        ph        (:height hud)
        top       (-> seeker (i/peer (fn [l [x & _]] (conj l x))))
        bottom    (-> seeker (i/peer (fn [_ [_ & r]] (drop (+ ph 2) r))))]
    (rebase ctx (-> top
                    (adjoin delimiter)
                    (adjoin paginated)
                    (adjoin delimiter)
                    (adjoin bottom)
                    (assoc :ov (-- (:height bottom) ph))))))

(defn window [seeker size]
  (letfn [(correct [hud]
            (assoc hud :ov (correct-ov hud)))]
    (->> seeker (hud size) (i/start) (i/end-x) (correct))))

(defn riffle [hud]
  (let [{[_ y]  :cursor
         height :height} hud
        y'      (mod* (inc y) height)
        correct #(assoc % :ov (correct-ov %))]
    (-> hud (i/reset-y y') (correct))))

(defn track-suggest [ctx suggestions]
  (assoc ctx :suggestions suggestions))

(defn track-docs [ctx docs]
  (assoc ctx :docs docs))

(defn un-suggest [ctx]
  (assoc ctx :suggestions i/empty-seeker))

(defn un-docs [ctx]
  (assoc ctx :docs i/empty-seeker))

(defn auto-complete [ctx]
  (let [{seeker      :seeker
         suggestions :suggestions} ctx
        sgst (i/line suggestions)]
    (seek ctx
          (if (empty? (:lines suggestions))
            seeker
            (-> seeker
                (i/expand)
                (i/delete)
                (i/slicer #(concat sgst %))
                (i/move-x #(+ % (count sgst))))))))

;; === Rendering ===

(defn re-render [ctx]
  (assoc ctx :render :total))

(defn diff-render [ctx]
  (assoc ctx :render :diff))

(defn clear-render [ctx]
  (assoc ctx :render :clear))

(defn no-render [ctx]
  (assoc ctx :render :nothing))

(defn highlight [ctx]
  (let [{complete :complete-hud
         cs       :colourscheme} ctx
        scheme (fn [region]
                 {:region region
                  :scheme (-> cs (select-cs) (simple-scheme))})]
    (if (i/selection? complete)
      (assoc-in ctx [:highlights :selection] (->> complete (i/selection) (scheme)))
      ctx)))

(defn gc [ctx]
  (let [colourscheme (-> ctx (:colourscheme) (clean-cs) (simple-scheme))]
    (assoc ctx :highlights i/empty-map
               :garbage (map-vals #(assoc % :scheme colourscheme) (:highlights ctx)))))

(defn match [ctx]
  (if-let [{[xs ys] :start
            [xe ye] :end} (-> (:complete-hud ctx) (i/find-pair))]
    (let [scheme (fn [region]
                   {:region   region
                    :scheme   {:cs (-> ctx (:colourscheme) (clean-cs))
                               :style :underline}})]
      (-> ctx
          (assoc-in [:highlights :open-paren] (scheme {:start [xs ys] :end [(inc xs) ys]}))
          (assoc-in [:highlights :closed-paren] (scheme {:start [xe ye] :end [(inc xe) ye]}))))
    ctx))

(defn auto-match [ctx]
  (let [complete-hud (:complete-hud ctx)]
    (cond
      (i/open-pairs (i/right complete-hud)) (match ctx)
      (i/closed-pairs (i/left complete-hud)) (match ctx)
      :else ctx)))

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
  (let [new-fov  (-> ctx (:terminal) (t/size))
        fov      (-> ctx (:persisted-hud) (:fov))]
    (if (not= new-fov fov)
      (-> (remember ctx)
          (assoc-in [:persisted-hud :lor] new-fov)
          (assoc-in [:persisted-hud :fov] new-fov)
          (assoc-in [:complete-hud :lor] new-fov)
          (assoc-in [:complete-hud :fov] new-fov)
          (calibrate)
          (re-render))
      ctx)))

(defn clear [ctx]
  (let [start-hud (init-hud ctx)]
    (-> (remember ctx)
        (persist start-hud)
        (rebase (:seeker ctx))
        (calibrate)
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
  (-> (remember ctx)
      (update :complete-hud #(scroll % upwards))))

(defn scroll-down [ctx]
  (-> (remember ctx)
      (update :complete-hud #(scroll % downwards))))

(defn scroll-stop [ctx]
  (-> ctx
      (update :complete-hud noscroll)
      (update :persisted-hud noscroll)))

;; === REPL ===

(defn roll [ctx f]
  (let [clipboard (get-in ctx [:seeker :clipboard])
        then-repl   (-> ctx :repl f)
        then-seeker (-> (r/then then-repl)
                        (i/end)
                        (assoc :clipboard clipboard))]
    (-> (remember ctx)
        (rebase then-seeker)
        (seek then-seeker)
        (assoc :repl then-repl))))

(defn roll-back [ctx]
  (roll ctx r/travel-back))

(defn roll-forward [ctx]
  (roll ctx r/travel-forward))

(defn evaluate [ctx]
  (let [evaluation (r/evaluate! (:repl ctx) (:seeker ctx))
        result     (r/result evaluation)]
    (-> (remember ctx)
        (preserve result caret i/empty-seeker)
        (persist)
        (seek i/empty-seeker)
        (assoc :repl evaluation))))

(defn suggest [ctx]
  (let [{seeker      :seeker
         repl        :repl
         suggestions :suggestions} ctx
        suggestions (if (empty? (:lines suggestions))
                      (-> (r/complete! repl seeker) (window 10))
                      (riffle suggestions))]
    (-> (remember ctx)
        (track-suggest suggestions)
        (auto-complete)
        (pop-up-riffle suggestions))))

(defn sign [ctx]
  (let [{repl   :repl
         seeker :seeker} ctx
        make-lines (fn [{:keys [ns name args]}]
                     (mapv #(i/from-string (str ns "/" name " " %)) args))
        info-lines (some->> (r/info! repl seeker)
                            (make-lines)
                            (apply i/join-many))]
    (-> (remember ctx)
        (pop-up-static (-> info-lines
                           (or i/empty-seeker)
                           (window 10))))))

(defn document [ctx]
  (let [{repl   :repl
         seeker :seeker
         docs   :docs} ctx
        empty-docs {:doc ""}
        doc-lines (if (empty? (:lines docs))
                    (-> (r/info! repl seeker)
                        (or empty-docs)
                        (:doc)
                        (i/from-string)
                        (window 15))
                    (riffle docs))]
    (-> (remember ctx)
        (track-docs doc-lines)
        (pop-up-riffle doc-lines))))

;; === Input ===

(defn capture [ctx event]
  (let [seeker (-> ctx (:seeker) (i/process event))]
    (-> (remember ctx)
        (rebase seeker)
        (seek seeker))))

(defn gobble [ctx event]
  (let [persistent (-> (:persisted-hud ctx)
                       (i/rebase drop-last)
                       (adjoin (:key event))
                       (adjoin i/empty-seeker)
                       (adjoin caret))]
    (-> (remember ctx)
        (persist persistent)
        (rebase))))

(defn reformat [ctx]
  (let [formatted (-> ctx (:seeker) (f/format-seeker))]
    (-> (remember ctx)
        (rebase formatted)
        (seek formatted))))


(defn predef [ctx event]
  (let [repl (:repl ctx)
        _    (r/evaluate! repl (:key event))
        _    (r/out-subscribe! repl)]
    ctx))

;; === Events ===

(defn process [ctx event]
  (case (:action event)
    :predef (-> ctx (predef event) (continue))
    :gobble (-> ctx (scroll-stop) (gobble event) (calibrate) (diff-render) (resize) (continue))
    :docs (-> ctx (gc) (un-suggest) (scroll-stop) (deselect) (document) (auto-match) (diff-render) (resize) (continue))
    :signature (-> ctx (gc) (un-suggest) (un-docs) (scroll-stop) (deselect) (sign) (auto-match) (diff-render) (resize) (continue))
    :match (-> ctx (gc) (scroll-stop) (deselect) (match) (diff-render) (resize) (continue))
    :suggest (-> ctx (gc) (un-docs) (scroll-stop) (suggest) (deselect) (auto-match) (diff-render) (resize) (continue))
    :scroll-up (-> ctx (gc) (scroll-up) (deselect) (highlight) (diff-render) (resize) (continue))
    :scroll-down (-> ctx (gc) (scroll-down) (deselect) (highlight) (diff-render) (resize) (continue))
    :prev-eval (-> ctx (gc) (un-suggest) (un-docs) (roll-back) (highlight) (scroll-stop) (auto-match) (diff-render) (resize) (continue))
    :next-eval (-> ctx (gc) (un-suggest) (un-docs) (roll-forward) (highlight) (scroll-stop) (auto-match) (diff-render) (resize) (continue))
    :format (-> ctx (gc) (un-suggest) (un-docs) (reformat) (highlight) (scroll-stop) (auto-match) (diff-render) (resize) (continue))
    :clear (-> ctx (gc) (clear) (deselect) (highlight) (auto-match) (clear-render) (resize) (continue))
    :eval (-> ctx (gc) (un-suggest) (un-docs) (un-docs) (evaluate) (highlight) (scroll-stop) (diff-render) (resize) (continue))
    :exit (-> ctx (gc) (scroll-stop) (deselect) (highlight) (diff-render) (resize) (exit) (terminate))
    (-> ctx (gc) (un-suggest) (un-docs) (capture event) (calibrate) (highlight) (scroll-stop) (auto-match) (diff-render) (resize) (continue))))

(defn match-stroke [{:keys [keymap]} stroke]
  (let [key    (:key stroke)
        action (get keymap stroke)]
    (m/match [action (char? key)]
             [nil true] (i/->Event :char key)
             [nil false] (i/->Event :none key)
             [_ _] (i/->Event action key))))

(defn sleep [msecs]
  (tsk/task (Thread/sleep msecs)))

(defn tell! [a event]
  (letfn [(proceed [[_ ctx]]
            (let [[status nctx] (process ctx event)
                  _ (render nctx)]
              [status nctx]))]
    (or (some-> a (agent-error) (throw))
        (send-off a proceed))))

(defn continuously! [a f]
  (tsk/task
    (loop [[status ctx] @a]
      (when (= :continue status)
        (some->> ctx (f) (tell! a))
        (recur @a)))))

(defn read-out! [a]
  (continuously! a #(some->> % (:repl) (r/read-out!) (apply i/join-many) (i/->Event :gobble))))

(defn read-in! [a]
  (continuously! a #(some->> % (:terminal) (t/poll-key!) (match-stroke %))))

(defn init! [a]
  (tsk/task
    (->> [(i/from-string "(require '[omnia.resolution :refer [retrieve retrieve-from]])")]
         (apply i/join-many)
         (i/->Event :predef)
         (tell! a))))

(defn read-eval-print [config]
  (let [oracle (->> config (context) (continue) (agent))]
    (-> (tsk/zip
          (init! oracle)
          (read-in! oracle)
          (read-out! oracle))
        (tsk/then-do (sleep 1200))
        (tsk/then-do (second @oracle))
        (tsk/run))))