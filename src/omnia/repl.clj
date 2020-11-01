(ns omnia.repl
  (:require [halfling.task :as tsk]
            [omnia.server :as r]
            [omnia.hud :as h]
            [omnia.input :as i]
            [omnia.format :as f]
            [omnia.terminal :as t]
            [omnia.event :as e]
            [schema.core :as s]
            [omnia.hud :refer [Hud]]
            [omnia.terminal :refer [Terminal]]
            [omnia.input :refer [Seeker]]
            [omnia.server :refer [REPLServer]]
            [omnia.render :refer [render!]]
            [omnia.more :refer [-- ++ inc< dec< mod* omnia-version map-vals Point Region]]
            [omnia.config :refer [InternalConfig InternalSyntax]]))

(def Render
  (s/enum :diff :total :clear :nothing))

(def Highlight
  {:region Region
   :scheme InternalSyntax
   :styles [s/Keyword]})

(def Highlights
  {(s/optional-key :selection)    Highlight
   (s/optional-key :open-paren)   Highlight
   (s/optional-key :closed-paren) Highlight})

(def Context
  {:terminal      Terminal
   :repl          REPLServer
   :config        InternalConfig
   :render        Render
   :previous-hud  Hud
   :persisted-hud Hud
   :complete-hud  Hud
   :seeker        Seeker
   :suggestions   Hud
   :docs          s/Any
   :highlights    Highlights
   :garbage       Highlights})

(def delimiter (i/from-string "------"))
(def caret (i/from-string "Ω =>"))
(def goodbye (i/from-string "Bye..for now\nFor even the very wise cannot see all ends"))

(s/defn context [config   :- InternalConfig
                 terminal :- Terminal
                 repl     :- REPLServer] :- Context
  (let [seeker        i/empty-line
        previous-hud  (h/hud (t/size terminal))
        persisted-hud (h/init-hud terminal repl)
        complete-hud  (h/enrich-with persisted-hud [seeker])]
    {:config        config
     :terminal      terminal
     :repl          repl
     :render        :diff
     :previous-hud  previous-hud
     :persisted-hud persisted-hud
     :complete-hud  complete-hud
     :seeker        seeker
     :suggestions   h/empty-hud
     :docs          i/empty-seeker
     :highlights    i/empty-map
     :garbage       i/empty-map}))


(s/defn preview-hud [ctx :- Context] :- Hud
  (:complete-hud ctx))

(s/defn persisted-hud [ctx :- Context] :- Hud
  (:persisted-hud ctx))

(s/defn previous-hud [ctx :- Context] :- Hud
  (:previous-hud ctx))

(s/defn input-area [ctx :- Context] :- Hud
  (:seeker ctx))

(s/defn server [ctx :- Context] :- REPLServer
  (:repl ctx))

(s/defn refresh [ctx :- Context] :- Context
  (assoc ctx
    :previous-hud (:complete-hud ctx)
    :complete-hud (-> ctx (:persisted-hud) (h/enrich-with [(:seeker ctx)]))))

(s/defn with-hud [ctx :- Context, hud :- Hud] :- Context
  (-> ctx (assoc :persisted-hud hud) (refresh)))

(s/defn with-text [ctx :- Context, input :- Seeker] :- Context
  (let [clipboard (or (:clipboard input)
                      (-> ctx (input-area) (:clipboard)))
        new-text  (assoc input :clipboard clipboard)]
    (assoc ctx :seeker new-text)))

(s/defn with-server [ctx :- Context, repl :- REPLServer] :- Context
  (assoc ctx :repl repl))

(s/defn rebase
  ([ctx :- Context] :- Context
   (rebase ctx (:seeker ctx)))
  ([ctx    :- Context
    seeker :- Seeker] :- Context
   (let [persisted (:persisted-hud ctx)
         rebased   (update persisted :seeker #(i/conjoin % seeker))]
     (assoc ctx :complete-hud rebased))))

(s/defn preserve [ctx & seekers] :- Context
  (update-in ctx [:complete-hud :seeker] #(reduce i/conjoin % seekers)))

(s/defn persist
  ([ctx :- Context] :- Context
   (persist ctx (:complete-hud ctx)))
  ([ctx :- Context
    hud :- Hud] :- Context
   (assoc ctx :persisted-hud hud)))

(s/defn remember [ctx :- Context] :- Context
  (assoc ctx :previous-hud (:complete-hud ctx)))

(s/defn seek [ctx    :- Context
              seeker :- Seeker] :- Context
  (->> (get-in ctx [:seeker :clipboard])
       (or (:clipboard seeker))
       (assoc seeker :clipboard)
       (assoc ctx :seeker)))

(s/defn pop-up-riffle [ctx :- Context
                       hud :- Hud] :- Context
  (let [seeker    (:seeker ctx)
        paginated (h/paginate hud)
        ph        (-> hud :seeker :height)
        top       (-> seeker (i/peer (fn [l [x & _]] (conj l x))))
        bottom    (-> seeker (i/peer (fn [_ [_ & r]] (drop (+ ph 2) r))))]
    (rebase ctx (-> (i/conjoin top delimiter paginated)
                    (i/end-x)
                    (i/adjoin delimiter bottom)))))

(s/defn pop-up-static [ctx :- Context
                       hud :- Hud] :- Context
  (let [seeker    (:seeker ctx)
        paginated (h/paginate hud)
        ph        (-> hud :seeker :height)
        top       (-> seeker (i/peer (fn [l [x & _]] (conj l x))))
        bottom    (-> seeker (i/peer (fn [_ [_ & r]] (drop (+ ph 2) r))))]
    (rebase ctx (i/adjoin top delimiter paginated delimiter bottom))))

(defn track-suggest [ctx suggestions]
  (assoc ctx :suggestions suggestions))

(defn track-docs [ctx docs]
  (assoc ctx :docs docs))

(defn un-suggest [ctx]
  (assoc ctx :suggestions h/empty-hud))

(defn un-docs [ctx]
  (assoc ctx :docs i/empty-seeker))

(s/defn auto-complete [ctx :- Context] :- Context
  (let [{seeker      :seeker
         suggestions :suggestions} ctx
        sgst (-> suggestions (:seeker) (i/line))]
    (seek ctx
          (if (h/hollow? suggestions)
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

(s/defn highlight [ctx :- Context] :- Context
  (let [seeker    (-> ctx :complete-hud :seeker)
        scheme    (-> ctx :config :syntax :selection)]
    (if (i/selection? seeker)
      (assoc-in ctx [:highlights :selection]
                {:region (i/selection seeker)
                 :scheme  scheme
                 :styles []})
      ctx)))

;; Question is: Should this thing actually provide a scheme or should I just pick it from the context?
(s/defn gc [ctx :- Context] :- Context
  (let [scheme (-> ctx (:config) (:syntax) (:clean-up))]
    (assoc ctx :highlights i/empty-map
               :garbage (->> (:highlights ctx)
                             (map-vals (fn [selection]
                                         {:region (:region selection)
                                          :scheme scheme
                                          :styles []}))))))

(s/defn match [ctx :- Context] :- Context
  (if-let [{[xs ys] :start
            [xe ye] :end} (-> ctx (:complete-hud) (:seeker) (i/find-pair))]
    (let [scheme (fn [region]
                   {:region region
                    :scheme (-> ctx (:config) (:syntax) (:clean-up))
                    :styles [:underline]})]
      (-> ctx
          (assoc-in [:highlights :open-paren] (scheme {:start [xs ys] :end [(inc xs) ys]}))
          (assoc-in [:highlights :closed-paren] (scheme {:start [xe ye] :end [(inc xe) ye]}))))
    ctx))

(s/defn auto-match [ctx :- Context] :- Context
  (let [seeker (-> ctx :complete-hud :seeker)]
    (cond
      (i/open-pairs (i/right seeker)) (match ctx)
      (i/closed-pairs (i/left seeker)) (match ctx)
      :else ctx)))

;; === Control ===

(defrecord Cont [status ctx])

(defn continue [ctx]
  (Cont. :continue ctx)
  #_[:continue ctx])

(defn terminate [ctx]
  (Cont. :terminate ctx)
  #_[:terminate ctx])

(s/defn calibrate [ctx :- Context] :- Context
  (let [nov (h/correct-ov (:complete-hud ctx)
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

(defn clear [{:keys [terminal repl] :as ctx}]
  (let [start-hud (h/init-hud terminal repl)]
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
      (update-in [:complete-hud :seeker] i/deselect)
      (update-in [:persisted-hud :seeker] i/deselect)
      (update :seeker i/deselect)))

(defn scroll-up [ctx]
  (-> (remember ctx)
      (update :complete-hud h/scroll-up)))

(defn scroll-down [ctx]
  (-> (remember ctx)
      (update :complete-hud h/scroll-down)))

(defn scroll-stop [ctx]
  (-> ctx
      (update :complete-hud h/scroll-stop)
      (update :persisted-hud h/scroll-stop)))

;; === REPL ===

(defn roll [ctx f]
  (let [clipboard (get-in ctx [:seeker :clipboard])
        then-server   (-> ctx (server) f)
        then-seeker (-> (r/then then-server)
                        (i/end)
                        (assoc :clipboard clipboard))]
    (-> ctx (with-server then-server) (with-text then-seeker) (refresh))))

(defn roll-back [ctx]
  (roll ctx r/travel-back))

(defn roll-forward [ctx]
  (roll ctx r/travel-forward))

(s/defn evaluate [ctx :- Context] :- Context
  (let [text    (input-area ctx)
        server' (-> ctx (server) (r/evaluate! text))
        result  (r/last-eval server')
        new-hud (-> ctx
                    (persisted-hud)
                    (h/enrich-with [text result caret]))]
    (-> ctx
        (with-server server')
        (with-text i/empty-line)
        (with-hud new-hud))))

(s/defn suggest [ctx :- Context] :- Context
  (let [{seeker      :seeker
         repl        :repl
         suggestions :suggestions} ctx
        suggestions (if (h/hollow? suggestions)
                      (-> (r/complete! repl seeker) (h/window 10))
                      (h/riffle suggestions))]
    (-> (remember ctx)
        (track-suggest suggestions)
        (auto-complete)
        (pop-up-riffle suggestions))))

(s/defn sign [ctx :- Context] :- Context
  (let [{repl   :repl
         seeker :seeker} ctx
        make-lines (fn [{:keys [ns name args]}]
                     (mapv #(i/from-string (str ns "/" name " " %)) args))
        info-lines (some->> (r/info! repl seeker)
                            (make-lines)
                            (i/conjoined))]
    (-> (remember ctx)
        (pop-up-static (-> info-lines
                           (or i/empty-seeker)
                           (h/window 10))))))

(s/defn document [ctx :- Context] :- Context
  (let [{repl   :repl
         seeker :seeker
         docs   :docs} ctx
        empty-docs {:doc ""}
        doc-lines (if (empty? (:lines docs))
                    (-> (r/info! repl seeker)
                        (or empty-docs)
                        (:doc)
                        (i/from-string)
                        (h/window 15))
                    (h/riffle docs))]
    (-> (remember ctx)
        (track-docs doc-lines)
        (pop-up-riffle doc-lines))))

;; === Input ===

(defn capture [ctx event]
  (let [new-input (-> ctx (input-area) (i/process event))]
    (-> ctx (with-text new-input) (refresh))))

(defn reformat [ctx]
  (let [formatted (-> ctx (input-area) (f/format-seeker))]
    (-> ctx (with-text formatted) (refresh))))

(defn inject [ctx event]
  (let [repl (:repl ctx)
        _    (->> event (:value) (i/from-string) (r/evaluate! repl))
        _    (r/read-out! repl)]
    ctx))

;; === Events ===

(defn process [ctx event]
  (case (:action event)
    :inject (-> ctx (inject event) (continue))
    :docs (-> ctx (gc) (un-suggest) (scroll-stop) (deselect) (document) (auto-match) (diff-render) (resize) (continue))
    :signature (-> ctx (gc) (un-suggest) (un-docs) (scroll-stop) (deselect) (sign) (auto-match) (diff-render) (resize) (continue))
    :match (-> ctx (gc) (scroll-stop) (deselect) (match) (diff-render) (resize) (continue))
    :suggest (-> ctx (gc) (un-docs) (scroll-stop) (suggest) (deselect) (auto-match) (diff-render) (resize) (continue))
    :scroll-up (-> ctx (gc) (scroll-up) (deselect) (highlight) (diff-render) (resize) (continue))
    :scroll-down (-> ctx (gc) (scroll-down) (deselect) (highlight) (diff-render) (resize) (continue))
    :prev-eval (-> ctx (gc) (un-suggest) (un-docs) (roll-back) (highlight) (scroll-stop) (auto-match) (diff-render) (resize) (continue))
    :next-eval (-> ctx (gc) (un-suggest) (un-docs) (roll-forward) (highlight) (scroll-stop) (auto-match) (diff-render) (resize) (continue))
    :indent (-> ctx (gc) (un-suggest) (un-docs) (reformat) (highlight) (scroll-stop) (auto-match) (diff-render) (resize) (continue))
    :clear (-> ctx (gc) (clear) (deselect) (highlight) (auto-match) (clear-render) (resize) (continue))
    :evaluate (-> ctx (gc) (un-suggest) (un-docs) (un-docs) (evaluate) (highlight) (scroll-stop) (diff-render) (resize) (continue))
    :exit (-> ctx (gc) (scroll-stop) (deselect) (highlight) (diff-render) (resize) (exit) (terminate))
    :ignore (continue ctx)
    (-> ctx (gc) (un-suggest) (un-docs) (capture event) (calibrate) (highlight) (scroll-stop) (auto-match) (diff-render) (resize) (continue))))

(defn match-stroke [ctx stroke]                             ;; I think this should be done in the terminal
  (let [key    (:key stroke)
        action (-> ctx (:config) (:keymap) (get stroke))
        ;control?   (and (char? key)
        ;                (Character/isISOControl ^Character char)) ;; Swing gives me control characters
        unknown?   (and (nil? action)
                        (not (char? key)))
        character? (and (nil? action)
                        (char? key))]
    (cond
      ;      control?   ()
      unknown?   (e/event e/ignore)
      character? (e/event e/character key)
      :else      (e/event action))))

(defn render-ret! [^Cont cont]
  (render! (.ctx cont))
  cont)

(defn prelude! [^Cont cont]
  (let [event (e/event e/inject "(require '[omnia.resolution :refer [retrieve retrieve-from]])")]
    (-> (.ctx cont)
        (process event)
        (tsk/task)
        (tsk/then render-ret!))))

(defn read! [terminal ^Cont cont]
  (tsk/task
    (loop [current-cont cont]
      (if (= :continue (.status current-cont))
        (->> (t/get-key! terminal)
             (match-stroke (.ctx current-cont))
             (process (.ctx current-cont))
             (render-ret!)
             (recur))
        current-cont))))

(defn read-eval-print [config terminal repl]
  (let [cont (continue (context config terminal repl))]
    (-> (prelude! cont)
        (tsk/then (partial read! terminal))
        (tsk/then #(do (Thread/sleep 1200) %))
        (tsk/then #(.ctx ^Cont %))
        (tsk/run))))