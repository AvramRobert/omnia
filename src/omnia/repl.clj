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
            [omnia.more :refer [=> omnia-version map-vals Region]]
            [omnia.config :refer [InternalConfig InternalSyntax]]))

(def Render
  (s/enum :diff :total :clear :nothing))

(def Highlight
  {:region Region
   :scheme InternalSyntax
   :styles [s/Keyword]})

(def HighlightType (s/enum :selection :open-paren :closed-paren))

(def Highlights {(s/optional-key HighlightType) Highlight})

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
   :documentation Hud
   :signatures    Hud
   :highlights    Highlights
   :garbage       Highlights})

(def delimiter (i/from-string "------"))
(def caret (i/from-string "Ω =>"))
(def goodbye (i/from-string "Bye..for now\nFor even the very wise cannot see all ends"))
(def greeting (i/from-string (format "Welcome to Omnia! (Ω) v%s" (omnia-version))))
(def clj-version (i/from-string (format "-- Clojure v%s --" (clojure-version))))
(def java-version (i/from-string (format "-- Java v%s --" (System/getProperty "java.version"))))

(defn nrepl-info [host port] (i/from-string (str "-- nREPL server started on nrepl://" host ":" port " --")))
(def prelude [(e/event e/inject "(require '[omnia.resolution :refer [retrieve retrieve-from]])")])

(s/defn init-hud :- Hud
  [terminal :- Terminal,
   repl     :- REPLServer]
  (let [fov       (t/size terminal)
        repl-info (nrepl-info (:host repl) (:port repl))]
    (-> (h/hud-of fov)
        (h/enrich-with [greeting
                        repl-info
                        clj-version
                        java-version
                        i/empty-line
                        caret]))))

(s/defn context :- Context
  [config   :- InternalConfig
   terminal :- Terminal
   repl     :- REPLServer]
  (let [seeker        i/empty-line
        previous-hud  (h/hud-of (t/size terminal))
        persisted-hud (init-hud terminal repl)
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
     :documentation h/empty-hud
     :signatures    h/empty-hud
     :highlights    i/empty-map
     :garbage       i/empty-map}))

(s/defn configuration :- InternalConfig
  [ctx :- Context]
  (:config ctx))

(s/defn preview-hud :- Hud
  [ctx :- Context]
  (:complete-hud ctx))

(s/defn persisted-hud :- Hud
  [ctx :- Context]
  (:persisted-hud ctx))

(s/defn previous-hud :- Hud
  [ctx :- Context]
  (:previous-hud ctx))

(s/defn input-area :- Hud
  [ctx :- Context]
  (:seeker ctx))

(s/defn server :- REPLServer
  [ctx :- Context]
  (:repl ctx))

(s/defn suggestions :- Hud
  [ctx :- Context]
  (:suggestions ctx))

(s/defn signatures :- Hud
  [ctx :- Context]
  (:signatures ctx))

(s/defn documentation :- Hud
  [ctx :- Context]
  (:documentation ctx))

(s/defn terminal :- Terminal
  [ctx :- Context]
  (:terminal ctx))

(s/defn highlights :- Highlights
  [ctx :- Context]
  (:highlights ctx))

(s/defn refresh :- Context
  [ctx :- Context]
  (assoc ctx
    :previous-hud (:complete-hud ctx)
    :complete-hud (-> ctx (:persisted-hud) (h/enrich-with [(:seeker ctx)]))))

(s/defn with-preview :- Context
  [ctx :- Context, hud :- Hud]
  (assoc ctx
    :previous-hud (:complete-hud ctx)
    :complete-hud hud))

(s/defn with-persisted :- Context
  [ctx :- Context, hud :- Hud]
  (assoc ctx :persisted-hud hud))

(s/defn with-hud :- Context
  [ctx :- Context, hud :- Hud]
  (-> ctx (with-persisted hud) (refresh)))

(s/defn with-unrefreshed-preview :- Context
  [ctx :- Context, hud :- Hud]
  (assoc ctx :complete-hud hud))

(s/defn reset-highlights :- Context
  [ctx :- Context]
  (assoc ctx :highlights i/empty-map))

(s/defn with-garbage :- Context
  [ctx :- Context, highlights :- Highlights]
  (assoc ctx :garbage highlights))

(s/defn with-selection :- Context
  [ctx :- Context, highlight :- Highlight]
  (assoc-in ctx [:highlights :selection] highlight))

(s/defn with-parens :- Context
  [ctx :- Context, open :- Highlight, closed :- Highlight]
  (-> ctx
      (assoc-in [:highlights :open-paren] open)
      (assoc-in [:highlights :closed-paren] closed)))

(s/defn make-selection :- Highlight
  [ctx :- Context, region :- Region]
  (let [scheme (-> ctx (configuration) (:syntax) (:selection))]
    {:region region
     :scheme scheme
     :styles []}))

(s/defn make-paren :- Highlight
  [ctx :- Context, region :- Region]
  (let [scheme (-> ctx (configuration) (:syntax) (:clean-up))]
    {:region region
     :scheme scheme
     :styles [:underline]}))

(s/defn make-garbage :- Highlight
  [ctx :- Context, region :- Region]
  (let [scheme (-> ctx (configuration) (:syntax) (:clean-up))]
    {:region region
     :scheme scheme
     :styles []}))

(s/defn with-text [ctx :- Context, input :- Seeker] :- Context
  (let [clipboard (or (:clipboard input)
                      (-> ctx (input-area) (:clipboard)))
        new-text  (assoc input :clipboard clipboard)]
    (assoc ctx :seeker new-text)))

(s/defn with-server :- Context
  [ctx :- Context, repl :- REPLServer]
  (assoc ctx :repl repl))

(s/defn with-terminal :- Context
  [ctx :- Context, terminal :- Terminal]
  (assoc ctx :terminal terminal))

(s/defn with-suggestions :- Context
  [ctx :- Context, suggestions :- Hud]
  (assoc ctx :suggestions suggestions))

(s/defn with-documentation :- Context
  [ctx :- Context, documentation :- Hud]
  (assoc ctx :documentation documentation))

(s/defn with-signatures :- Context
  [ctx :- Context, signatures :- Hud]
  (assoc ctx :signatures signatures))

(s/defn reset-suggestions :- Context
  [ctx :- Context]
  (with-suggestions ctx h/empty-hud))

(s/defn reset-documentation :- Context
  [ctx :- Context]
  (with-documentation ctx h/empty-hud))

(s/defn reset-signatures :- Context
  [ctx :- Context]
  (with-signatures ctx h/empty-hud))

(s/defn re-render :- Context
  [ctx :- Context]
  (assoc ctx :render :total))

(s/defn diff-render :- Context
  [ctx :- Context]
  (assoc ctx :render :diff))

(s/defn clear-render :- Context
  [ctx :- Context]
  (assoc ctx :render :clear))

(s/defn highlight :- Context
  [ctx :- Context]
  (let [text (-> ctx (preview-hud) (h/text))]
    (if (i/selected? text)
      (with-selection ctx (make-selection ctx (i/selection text)))
      ctx)))

(s/defn gc :- Context
  [ctx :- Context]
  (let [garbage (->> ctx
                     (highlights)
                     (map-vals #(->> % (:region) (make-garbage ctx))))]
    (if (empty? garbage)
      ctx
      (-> ctx
          (reset-highlights)
          (with-garbage garbage)))))

(s/defn match :- Context
  [ctx :- Context]
  (if-let [pair (-> ctx (preview-hud) (h/text) (i/find-pair))]
    (let [[xs ys] (:start pair)
          [xe ye] (:end pair)
          open   (make-paren ctx {:start [xs ys]
                                  :end   [(inc xs) ys]})
          closed (make-paren ctx {:start [xe ye]
                                  :end   [(inc xe) ye]})]
      (with-parens ctx open closed))
    ctx))

(s/defn match-parens :- Context
  [ctx :- Context]
  (let [text (-> ctx (preview-hud) (h/text))]
    (cond
      (i/open-pairs (i/right text)) (match ctx)
      (i/closed-pairs (i/left text)) (match ctx)
      :else ctx)))

;; === Control ===

(s/def Step
  {:status (s/enum :continue :terminate)
   :ctx    Context})

(s/defn continue :- Step
  [ctx :- Context]
  {:status :continue
   :ctx    ctx})

(s/defn terminate :- Step
  [ctx :- Context]
  {:status :terminate
   :ctx    ctx})

(s/defn calibrate :- Context
  [ctx :- Context]
  (let [nov       (h/correct-between (preview-hud ctx)
                                     (previous-hud ctx))
        persisted (-> ctx
                      (persisted-hud)
                      (h/reset-overview nov))
        preview   (-> ctx
                      (preview-hud)
                      (h/reset-overview nov))]
    (-> ctx
        (with-persisted persisted)
        (with-unrefreshed-preview preview))))

(s/defn resize :- Context
  [ctx :- Context]
  (let [current-fov (-> ctx (persisted-hud) (h/field-of-view))
        new-fov     (-> ctx (terminal) (t/size))]
    (if (not= new-fov current-fov)
      (-> ctx
          (with-persisted (-> ctx (persisted-hud) (h/resize new-fov)))
          (refresh)
          (calibrate)
          (re-render))
      ctx)))

(s/defn clear :- Context
  [ctx :- Context]
  (let [terminal    (terminal ctx)
        repl-server (server ctx)
        new-hud     (init-hud terminal repl-server)]
    (with-hud ctx new-hud)))

(s/defn exit :- Context
  [ctx :- Context]
  (let [preview (-> ctx
                    (preview-hud)
                    (h/enrich-with [goodbye])
                    (h/reset-overview))]
    (with-preview ctx preview)))

(s/defn deselect :- Context
  [ctx :- Context]
  (let [preview   (-> ctx (preview-hud) (h/deselect))
        persisted (-> ctx (persisted-hud) (h/deselect))
        input     (-> ctx (input-area) (i/deselect))]
    (-> ctx
        (with-unrefreshed-preview preview)
        (with-persisted persisted)
        (with-text input))))

(s/defn scroll-up :- Context
  [ctx :- Context]
  (let [preview (-> ctx (preview-hud) (h/scroll-up))]
    (with-preview ctx preview)))

(s/defn scroll-down :- Context
  [ctx :- Context]
  (let [preview (-> ctx (preview-hud) (h/scroll-down))]
    (with-preview ctx preview)))

(s/defn scroll-stop :- Context
  [ctx :- Context]
  (let [preview   (-> ctx (preview-hud) (h/scroll-stop))
        persisted (-> ctx (persisted-hud) (h/scroll-stop))]
    (-> ctx
        (with-unrefreshed-preview preview)
        (with-persisted persisted))))

;; === REPL ===

(s/defn roll :- Context
  [ctx :- Context, f :- (=> REPLServer REPLServer)]
  (let [clipboard   (-> ctx (input-area) (:clipboard))
        then-server (-> ctx (server) f)
        then-seeker (-> (r/then then-server)
                        (i/end)
                        (assoc :clipboard clipboard))]
    (-> ctx (with-server then-server) (with-text then-seeker) (refresh))))

(s/defn roll-back :- Context
  [ctx :- Context]
  (roll ctx r/travel-back))

(s/defn roll-forward :- Context
  [ctx :- Context]
  (roll ctx r/travel-forward))

(s/defn evaluate :- Context
  [ctx :- Context]
  (let [current-input (input-area ctx)
        server'       (-> ctx (server) (r/evaluate! current-input))
        result        (r/last-eval server')
        new-hud       (-> ctx
                          (persisted-hud)
                          (h/enrich-with [current-input result caret]))]
    (-> ctx
        (with-server server')
        (with-text i/empty-line)
        (with-hud new-hud))))

(s/defn suggestion-window :- Hud
  [ctx :- Context]
  (let [text  (input-area ctx)
        repl  (server ctx)
        suggs (suggestions ctx)]
    (if (h/hollow? suggs)
      (-> (r/complete! repl text) (h/riffle-window 10))
      (h/riffle suggs))))

(s/defn suggest :- Context
  [ctx :- Context]
  (let [suggestions (suggestion-window ctx)
        suggestion  (h/current-line suggestions)
        text        (-> ctx
                        (input-area)
                        (i/auto-complete suggestion))
        preview     (-> ctx
                        (persisted-hud)
                        (h/enrich-with [text])
                        (h/pop-up suggestions))]
    (-> ctx
        (with-suggestions suggestions)
        (with-text text)
        (with-preview preview))))

(s/defn signature-window :- Hud
  [ctx :- Context]
  (let [text (input-area ctx)
        repl (server ctx)
        sigs (signatures ctx)]
    (if (h/hollow? sigs)
      (-> (r/signature! repl text) (h/riffle-window 10))
      (h/riffle sigs))))

(s/defn signature :- Context
  [ctx :- Context]
  (let [signatures (signature-window ctx)
        text       (input-area ctx)
        preview    (-> ctx
                      (persisted-hud)
                      (h/enrich-with [text])
                      (h/pop-up signatures))]
    (-> ctx
        (with-signatures signatures)
        (with-preview preview))))

(s/defn documentation-window :- Hud
  [ctx :- Context]
  (let [text (input-area ctx)
        repl (server ctx)
        docs (documentation ctx)]
    (if (h/hollow? docs)
      (-> (r/docs! repl text) (h/riffle-window 15))
      (h/riffle docs))))

(s/defn document :- Context
  [ctx :- Context]
  (let [documentation (documentation-window ctx)
        text          (input-area ctx)
        preview       (-> ctx
                          (persisted-hud)
                          (h/enrich-with [text])
                          (h/pop-up documentation))]
    (-> ctx
        (with-documentation documentation)
        (with-preview preview))))

;; === Input ===

(s/defn capture :- Context
  [ctx :- Context
   event :- e/Event]
  (let [new-input (-> ctx (input-area) (i/process event))]
    (-> ctx (with-text new-input) (refresh))))

(s/defn reformat :- Context
  [ctx :- Context]
  (let [formatted (-> ctx (input-area) (f/format-seeker))]
    (-> ctx (with-text formatted) (refresh))))

(s/defn inject :- Context
  [ctx   :- Context
   event :- e/Event]
  (let [repl (server ctx)
        _    (->> event (:value) (i/from-string) (r/evaluate! repl))
        _    (r/read-out! repl)]
    ctx))

;; === Events ===

(s/defn process :- Step
  [ctx :- Context
   event :- e/Event]
  (case (:action event)
    :inject (-> ctx (inject event) (diff-render) (continue))
    :docs (-> ctx (gc) (scroll-stop) (reset-suggestions) (reset-signatures) (deselect) (document) (match-parens) (diff-render) (resize) (continue))
    :signature (-> ctx (gc) (scroll-stop) (reset-suggestions) (reset-documentation) (deselect) (signature) (match-parens) (diff-render) (resize) (continue))
    :match (-> ctx (gc) (scroll-stop) (deselect) (match) (diff-render) (resize) (continue))
    :suggest (-> ctx (gc) (scroll-stop) (reset-documentation) (reset-signatures) (deselect) (suggest) (match-parens) (diff-render) (resize) (continue))
    :scroll-up (-> ctx (gc) (scroll-up) (deselect) (highlight) (diff-render) (resize) (continue))
    :scroll-down (-> ctx (gc) (scroll-down) (deselect) (highlight) (diff-render) (resize) (continue))
    :prev-eval (-> ctx (gc) (scroll-stop) (reset-suggestions) (reset-documentation) (reset-signatures) (roll-back) (highlight) (match-parens) (diff-render) (resize) (continue))
    :next-eval (-> ctx (gc) (scroll-stop) (reset-suggestions) (reset-documentation) (reset-signatures) (roll-forward) (highlight) (match-parens) (diff-render) (resize) (continue))
    :indent (-> ctx (gc) (scroll-stop) (reset-suggestions) (reset-documentation) (reset-signatures) (reformat) (highlight) (match-parens) (diff-render) (resize) (continue))
    :clear (-> ctx (gc) (scroll-stop) (reset-suggestions) (reset-documentation) (reset-signatures) (deselect) (clear) (highlight) (match-parens) (clear-render) (resize) (continue))
    :evaluate (-> ctx (gc) (scroll-stop) (reset-suggestions) (reset-documentation) (reset-signatures) (evaluate) (highlight)  (diff-render) (resize) (continue))
    :exit (-> ctx (gc) (scroll-stop) (deselect) (highlight) (diff-render) (resize) (exit) (terminate))
    :refresh (-> ctx (gc) (scroll-stop) (deselect) (re-render) (resize) (continue))
    :ignore (continue ctx)
    (-> ctx (gc) (scroll-stop) (reset-suggestions) (reset-documentation) (reset-signatures) (capture event) (calibrate) (highlight) (match-parens) (diff-render) (resize) (continue))))

(s/defn consume :- Context
  [ctx    :- Context,
   events :- [e/Event]]
  (let [step   (process ctx (first events))
        status (:status step)
        ctx'   (:ctx step)
        _      (render! ctx')]
    (case status
      :continue (recur ctx' (rest events))
      :terminate ctx')))

(s/defn events-from :- [e/Event]
  [terminal :- Terminal]
  (iterate (fn [_] (t/get-event! terminal)) e/ignore))

(defn read-eval-print [config terminal repl]
  (let [events          (concat prelude (events-from terminal))
        initial-context (context config terminal repl)]
    (-> (tsk/task (consume initial-context events))
        (tsk/then #(do (Thread/sleep 1200) %))
        (tsk/run))))