(ns omnia.repl.context
  (:require [schema.core :as s]
            [omnia.repl.nrepl :as r]
            [omnia.repl.hud :as h]
            [omnia.text.core :as i]
            [omnia.text.format :as f]
            [omnia.view.terminal :as t]
            [omnia.config.components.event :as e]
            [omnia.config.components.event :refer [Event]]
            [omnia.repl.hud :refer [Hud]]
            [omnia.view.terminal :refer [Terminal]]
            [omnia.text.core :refer [Seeker]]
            [omnia.repl.nrepl :refer [REPLClient]]
            [omnia.util.schema :refer [=> Region]]
            [omnia.util.collection :refer [map-vals]]
            [omnia.util.misc :refer [omnia-version]]
            [omnia.config.components.text :refer [Style]]
            [omnia.config.components.core :refer [UserHighlighting]]
            [omnia.config.core :refer [Config]]
            [omnia.util.debug :as d]))

(def Render
  (s/enum :diff :total :clear :nothing))

(def Highlight
  {:region Region
   :scheme UserHighlighting
   :styles [Style]})

(def HighlightType (s/enum :selection :open-paren :closed-paren))

(def Highlights {(s/optional-key :selection) Highlight
                 (s/optional-key :open-paren) Highlight
                 (s/optional-key :closed-paren) Highlight})

(def Context
  {:terminal      Terminal
   :repl          REPLClient
   :config        Config
   :render        Render
   :previous-hud  Hud
   :persisted-hud Hud
   :preview-hud   Hud
   :input-area    Seeker
   :suggestions   Hud
   :documentation Hud
   :signatures    Hud
   :highlights    Highlights
   :garbage       Highlights})

(def caret (i/from-string "Ω =>"))
(def goodbye (i/from-string "Bye..for now\nFor even the very wise cannot see all ends"))
(def greeting (i/from-string (format "Welcome to Omnia! (Ω) v%s" (omnia-version))))
(def clj-version (i/from-string (format "-- Clojure v%s --" (clojure-version))))
(def java-version (i/from-string (format "-- Java v%s --" (System/getProperty "java.version"))))
(defn nrepl-info [host port] (i/from-string (str "-- nREPL server started on nrepl://" host ":" port " --")))

(s/defn init-hud :- Hud
  [terminal :- Terminal,
   repl     :- REPLClient]
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
  [config   :- Config
   terminal :- Terminal
   repl     :- REPLClient]
  (let [input     i/empty-line
        previous  (h/hud-of (t/size terminal))
        persisted (init-hud terminal repl)
        preview   (h/enrich-with persisted [input])]
    {:config        config
     :terminal      terminal
     :repl          repl
     :render        :diff
     :previous-hud  previous
     :persisted-hud persisted
     :preview-hud  preview
     :input-area        input
     :suggestions   h/empty-hud
     :documentation h/empty-hud
     :signatures    h/empty-hud
     :highlights    {}
     :garbage       {}}))

(s/defn configuration :- Config
  [ctx :- Context]
  (:config ctx))

(s/defn preview-hud :- Hud
  [ctx :- Context]
  (:preview-hud ctx))

(s/defn persisted-hud :- Hud
  [ctx :- Context]
  (:persisted-hud ctx))

(s/defn previous-hud :- Hud
  [ctx :- Context]
  (:previous-hud ctx))

(s/defn input-area :- Seeker
  [ctx :- Context]
  (:input-area ctx))

(s/defn client :- REPLClient
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

(s/defn client :- REPLClient
  [ctx :- Context]
  (:repl ctx))

(s/defn highlights :- Highlights
  [ctx :- Context]
  (:highlights ctx))

(s/defn garbage :- Highlights
  [ctx :- Context]
  (:garbage ctx))

(s/defn rendering :- Render
  [ctx :- Context]
  (:render ctx))

(s/defn refresh :- Context
  [ctx :- Context]
  (assoc ctx
    :previous-hud (:preview-hud ctx)
    :preview-hud (-> ctx (:persisted-hud) (h/enrich-with [(:input-area ctx)]))))

(s/defn with-preview :- Context
  [ctx :- Context, hud :- Hud]
  (assoc ctx
    :previous-hud (:preview-hud ctx)
    :preview-hud hud))

(s/defn with-persisted :- Context
  [ctx :- Context, hud :- Hud]
  (assoc ctx :persisted-hud hud))

(s/defn with-hud :- Context
  [ctx :- Context, hud :- Hud]
  (let [size (-> ctx (terminal) (t/size))
        hud  (h/resize hud size)]
    (-> ctx (with-persisted hud) (refresh))))

(s/defn with-unrefreshed-preview :- Context
  [ctx :- Context, hud :- Hud]
  (assoc ctx :preview-hud hud))

(s/defn reset-highlights :- Context
  [ctx :- Context]
  (assoc ctx :highlights {}))

(s/defn with-garbage :- Context
  [ctx :- Context, highlights :- Highlights]
  (assoc ctx :garbage highlights))

(s/defn with-highlight :- Context
  [ctx :- Context,
   h-type :- HighlightType,
   highlight :- Highlight]
  (assoc-in ctx [:highlights h-type] highlight))

(s/defn with-selection :- Context
  [ctx :- Context, highlight :- Highlight]
  (with-highlight ctx :selection highlight))

(s/defn with-parens :- Context
  [ctx :- Context, open :- Highlight, closed :- Highlight]
  (-> ctx
      (with-highlight :open-paren open)
      (with-highlight :closed-paren closed)))

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

(s/defn with-input-area [ctx :- Context, input :- Seeker] :- Context
  (let [clipboard (or (:clipboard input)
                      (-> ctx (input-area) (:clipboard)))
        new-text  (assoc input :clipboard clipboard)]
    (assoc ctx :input-area new-text)))

(s/defn with-server :- Context
  [ctx :- Context, repl :- REPLClient]
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
    (-> ctx (with-garbage garbage) (reset-highlights))))

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
                      (h/reset-view nov))
        preview   (-> ctx
                      (preview-hud)
                      (h/reset-view nov))]
    (-> ctx
        (with-persisted persisted)
        (with-unrefreshed-preview preview))))

(s/defn resize :- Context
  [ctx :- Context]
  (if-let [new-size (-> ctx (terminal) (t/resize!))]
    (-> ctx
        (with-persisted (h/resize (persisted-hud ctx) new-size))
        (refresh)
        (calibrate)
        (re-render))
    ctx))

(s/defn clear :- Context
  [ctx :- Context]
  (let [terminal    (terminal ctx)
        repl-server (client ctx)
        new-hud     (init-hud terminal repl-server)]
    (with-hud ctx new-hud)))

(s/defn exit :- Context
  [ctx :- Context]
  (let [preview (-> ctx
                    (preview-hud)
                    (h/enrich-with [goodbye])
                    (h/reset-view))]
    (with-preview ctx preview)))

(s/defn deselect :- Context
  [ctx :- Context]
  (let [preview   (-> ctx (preview-hud) (h/deselect))
        persisted (-> ctx (persisted-hud) (h/deselect))
        input     (-> ctx (input-area) (i/deselect))]
    (-> ctx
        (with-unrefreshed-preview preview)
        (with-persisted persisted)
        (with-input-area input))))

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

(s/defn eval-at :- Context
  [ctx :- Context,
   f :- (=> REPLClient REPLClient)]
  (let [clipboard   (-> ctx (input-area) (:clipboard))
        then-server (-> ctx (client) f)
        then-seeker (-> (r/then then-server)
                        (i/end)
                        (assoc :clipboard clipboard))]
    (-> ctx (with-server then-server) (with-input-area then-seeker) (refresh))))

(s/defn prev-eval :- Context
  [ctx :- Context]
  (eval-at ctx r/travel-back))

(s/defn next-eval :- Context
  [ctx :- Context]
  (eval-at ctx r/travel-forward))

(s/defn evaluate :- Context
  [ctx :- Context]
  (let [current-input (input-area ctx)
        server'       (-> ctx (client) (r/evaluate! current-input))
        result        (r/result server')
        new-hud       (-> ctx
                          (persisted-hud)
                          (h/enrich-with [current-input result caret]))]
    (-> ctx
        (with-server server')
        (with-input-area i/empty-line)
        (with-hud new-hud))))

(s/defn suggestion-window :- Hud
  [ctx :- Context]
  (let [text  (input-area ctx)
        repl  (client ctx)
        suggs (suggestions ctx)]
    (if (h/hollow? suggs)
      (-> repl (r/complete! text) (r/result) (h/riffle-window 10))
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
        (with-input-area text)
        (with-preview preview)
        (deselect))))

(s/defn signature-window :- Hud
  [ctx :- Context]
  (let [text (input-area ctx)
        repl (client ctx)
        sigs (signatures ctx)]
    (if (h/hollow? sigs)
      (-> repl (r/signature! text) (r/result) (h/riffle-window 10))
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
        repl (client ctx)
        docs (documentation ctx)]
    (if (h/hollow? docs)
      (-> repl (r/docs! text) (r/result) (h/riffle-window 15))
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

(s/defn capture :- Context
  [ctx   :- Context
   event :- Event]
  (let [new-input (-> ctx (input-area) (i/process event))]
    (-> ctx (with-input-area new-input) (refresh))))

(s/defn reformat :- Context
  [ctx :- Context]
  (let [formatted (-> ctx (input-area) (f/format-seeker))]
    (-> ctx (with-input-area formatted) (refresh))))

(s/defn inject :- Context
  [ctx   :- Context
   event :- e/Event]
  (let [repl (client ctx)
        _    (->> event (:value) (i/from-string) (r/evaluate! repl))]
    ctx))

(s/defn process :- Step
  [ctx :- Context
   event :- Event]
  (condp = (:action event)
    e/inject      (-> ctx (inject event) (diff-render) (continue))
    e/docs        (-> ctx (gc) (scroll-stop) (reset-suggestions) (reset-signatures) (deselect) (document) (match-parens) (diff-render) (resize) (continue))
    e/signature   (-> ctx (gc) (scroll-stop) (reset-suggestions) (reset-documentation) (deselect) (signature) (match-parens) (diff-render) (resize) (continue))
    e/match       (-> ctx (gc) (scroll-stop) (deselect) (match) (diff-render) (resize) (continue))
    e/suggest     (-> ctx (gc) (scroll-stop) (reset-documentation) (reset-signatures) (suggest) (match-parens) (diff-render) (resize) (continue))
    e/scroll-up   (-> ctx (gc) (scroll-up) (deselect) (highlight) (diff-render) (resize) (continue))
    e/scroll-down (-> ctx (gc) (scroll-down) (deselect) (highlight) (diff-render) (resize) (continue))
    e/prev-eval   (-> ctx (gc) (scroll-stop) (reset-suggestions) (reset-documentation) (reset-signatures) (prev-eval) (highlight) (match-parens) (diff-render) (resize) (continue))
    e/next-eval   (-> ctx (gc) (scroll-stop) (reset-suggestions) (reset-documentation) (reset-signatures) (next-eval) (highlight) (match-parens) (diff-render) (resize) (continue))
    e/indent      (-> ctx (gc) (scroll-stop) (reset-suggestions) (reset-documentation) (reset-signatures) (reformat) (highlight) (match-parens) (diff-render) (resize) (continue))
    e/clear       (-> ctx (gc) (scroll-stop) (reset-suggestions) (reset-documentation) (reset-signatures) (deselect) (clear) (highlight) (match-parens) (clear-render) (resize) (continue))
    e/evaluate    (-> ctx (gc) (scroll-stop) (reset-suggestions) (reset-documentation) (reset-signatures) (evaluate) (highlight)  (diff-render) (resize) (continue))
    e/exit        (-> ctx (gc) (scroll-stop) (deselect) (highlight) (diff-render) (resize) (exit) (terminate))
    e/refresh     (-> ctx (gc) (scroll-stop) (deselect) (re-render) (resize) (continue))
    e/ignore      (continue ctx)
    (-> ctx (gc) (scroll-stop) (reset-suggestions) (reset-documentation) (reset-signatures) (capture event) (calibrate) (highlight) (match-parens) (diff-render) (resize) (continue))))