(ns omnia.repl.context
  (:require [schema.core :as s]
            [omnia.repl.nrepl :as r]
            [omnia.repl.hud :as h]
            [omnia.repl.text :as i]
            [omnia.repl.format :as f]
            [omnia.schema.event :as e]
            [omnia.schema.context :refer :all]
            [omnia.schema.config :refer [Config]]
            [omnia.schema.render :refer [Highlights HighlightInfo HighlightType RenderingStrategy]]
            [omnia.schema.hud :refer [Hud]]
            [omnia.schema.text :refer [Text]]
            [omnia.schema.nrepl :refer [NReplClient]]
            [omnia.schema.common :refer [=> Region]]
            [omnia.util.collection :refer [map-vals assoc-new]]
            [omnia.util.misc :refer [omnia-version]]))

(def caret (i/from-string "Ω =>"))
(def goodbye (i/from-string "Bye..for now\nFor even the very wise cannot see all ends"))
(def greeting (i/from-string (format "Welcome to Omnia! (Ω) v%s" (omnia-version))))
(def clj-version (i/from-string (format "-- Clojure v%s --" (clojure-version))))
(def java-version (i/from-string (format "-- Java v%s --" (System/getProperty "java.version"))))
(defn nrepl-info [host port] (i/from-string (str "-- nREPL server started on nrepl://" host ":" port " --")))

(s/defn header :- [Text]
  [host :- s/Int
   port :- s/Int]
  (let [repl-info (nrepl-info host port)]
    [greeting
     repl-info
     clj-version
     java-version
     i/empty-line
     caret]))

(s/defn init-hud :- Hud
  [size :- s/Int,
   repl :- NReplClient]
  (let [header (header (:host repl) (:port repl))]
    (-> (h/hud-of size)
        (h/enrich-with header))))

(s/defn context :- Context
  [view-size :- s/Int
   repl      :- NReplClient]
  (let [input     i/empty-line
        previous  (h/hud-of view-size)
        persisted (init-hud view-size repl)
        preview   (h/enrich-with persisted [input])]
    {:nrepl          repl
     :render        :diff
     :previous-hud  previous
     :persisted-hud persisted
     :preview-hud   preview
     :input-area    input
     :suggestions   h/empty-hud
     :documentation h/empty-hud
     :signatures    h/empty-hud
     :highlights    {}
     :garbage       {}}))

(s/defn preview-hud :- Hud
  [ctx :- Context]
  (:preview-hud ctx))

(s/defn persisted-hud :- Hud
  [ctx :- Context]
  (:persisted-hud ctx))

(s/defn previous-hud :- Hud
  [ctx :- Context]
  (:previous-hud ctx))

(s/defn input-area :- Text
  [ctx :- Context]
  (:input-area ctx))

(s/defn view-size :- s/Int
  [ctx :- Context]
  (-> ctx (persisted-hud) (h/field-of-view)))

(s/defn suggestions :- Hud
  [ctx :- Context]
  (:suggestions ctx))

(s/defn signatures :- Hud
  [ctx :- Context]
  (:signatures ctx))

(s/defn documentation :- Hud
  [ctx :- Context]
  (:documentation ctx))

(s/defn nrepl-client :- NReplClient
  [ctx :- Context]
  (:nrepl ctx))

(s/defn highlights :- Highlights
  [ctx :- Context]
  (:highlights ctx))

(s/defn garbage :- Highlights
  [ctx :- Context]
  (:garbage ctx))

(s/defn rendering :- RenderingStrategy
  [ctx :- Context]
  (:render ctx))

(s/defn refresh :- Context
  [ctx :- Context]
  (assoc ctx
    :previous-hud (preview-hud ctx)
    :preview-hud (-> ctx (persisted-hud) (h/enrich-with [(:input-area ctx)]))))

(s/defn with-previous :- Context
  [ctx :- Context, hud :- Hud]
  (assoc ctx :previous-hud hud))

(s/defn with-preview :- Context
  [ctx :- Context, hud :- Hud]
  (assoc ctx
    :previous-hud (:preview-hud ctx)
    :preview-hud hud))

(s/defn with-persisted :- Context
  [ctx :- Context, hud :- Hud]
  (assoc ctx :persisted-hud hud))

(s/defn with-unrefreshed-preview :- Context
  [ctx :- Context, hud :- Hud]
  (assoc ctx :preview-hud hud))

(s/defn with-hud :- Context
  [ctx :- Context, hud :- Hud]
  (-> ctx (with-persisted hud) (refresh)))

(s/defn reset-highlights :- Context
  [ctx :- Context]
  (assoc-new ctx :highlights {}))

(s/defn reset-garbage :- Context
  [ctx :- Context]
  (assoc-new ctx :garbage {}))

(s/defn with-garbage :- Context
  [ctx :- Context, highlights :- Highlights]
  (assoc ctx :garbage highlights))

(s/defn with-highlight :- Context
  [ctx :- Context,
   h-type :- HighlightType,
   highlight :- HighlightInfo]
  (assoc-in ctx [:highlights h-type] highlight))

(s/defn with-selection :- Context
  [ctx :- Context, highlight :- HighlightInfo]
  (with-highlight ctx :selection highlight))

(s/defn with-parens :- Context
  [ctx :- Context, open :- HighlightInfo, closed :- HighlightInfo]
  (-> ctx
      (with-highlight :open-paren open)
      (with-highlight :closed-paren closed)))

(s/defn make-manual :- HighlightInfo
  [config :- Config, region :- Region]
  {:region region
   :scheme (-> config (:syntax) (:clean-up))
   :styles []})

(s/defn make-selection :- HighlightInfo
  [config :- Config, region :- Region]
  {:region region
   :scheme (-> config (:syntax) (:selection))
   :styles []})

(s/defn make-paren :- HighlightInfo
  [config :- Config, region :- Region]
  {:region region
   :scheme (-> config (:syntax) (:clean-up))
   :styles [:underline]})

(s/defn make-garbage :- HighlightInfo
  [config :- Config, region :- Region]
  {:region region
   :scheme (-> config (:syntax) (:clean-up))
   :styles []})

(s/defn with-input-area [ctx :- Context, input :- Text] :- Context
  (let [clipboard (or (:clipboard input)
                      (-> ctx (input-area) (:clipboard)))
        new-text  (assoc input :clipboard clipboard)]
    (assoc ctx :input-area new-text)))

(s/defn with-client :- Context
  [ctx :- Context, repl :- NReplClient]
  (assoc ctx :nrepl repl))

(s/defn with-suggestions :- Context
  [ctx :- Context, suggestions :- Hud]
  (assoc-new ctx :suggestions suggestions))

(s/defn with-documentation :- Context
  [ctx :- Context, documentation :- Hud]
  (assoc-new ctx :documentation documentation))

(s/defn with-signatures :- Context
  [ctx :- Context, signatures :- Hud]
  (assoc-new ctx :signatures signatures))

(s/defn with-render :- Context
  [ctx :- Context, render :- RenderingStrategy]
  (assoc-new ctx :render render))

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
  (with-render ctx :total))

(s/defn diff-render :- Context
  [ctx :- Context]
  (with-render ctx :diff))

(s/defn clear-render :- Context
  [ctx :- Context]
  (with-render ctx :clear))

(s/defn highlight :- Context
  [ctx :- Context
   config :- Config]
  (let [text (-> ctx (preview-hud) (h/text))]
    (if (i/selecting? text)
      (with-selection ctx (make-selection config (:selection text)))
      ctx)))

(s/defn gc :- Context
  [ctx :- Context
   config :- Config]
  (let [garbage (->> ctx
                     (highlights)
                     (map-vals #(make-garbage config (:region %))))]
    (-> ctx (with-garbage garbage) (reset-highlights))))

(s/defn match :- Context
  [ctx :- Context
   config :- Config]
  (if-let [pair (-> ctx (preview-hud) (h/text) (i/find-pair))]
    (let [open   (make-paren config (:left pair))
          closed (make-paren config (:right pair))]
      (with-parens ctx open closed))
    ctx))

(s/defn match-parens :- Context
  [ctx :- Context
   config :- Config]
  (let [text (-> ctx (preview-hud) (h/text))]
    (cond
      (i/open-pairs (i/current-char text)) (match ctx config)
      (i/closed-pairs (i/previous-char text)) (match ctx config)
      :else ctx)))

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
  [ctx :- Context
   event :- e/Event]
  (let [persisted (persisted-hud ctx)
        new-fov   (-> event (:value) (second))]
    (-> ctx
        (with-persisted (h/resize persisted new-fov))
        (refresh))))

(s/defn clear :- Context
  [ctx :- Context]
  (let [size  (view-size ctx)
        nrepl (nrepl-client ctx)]
    (with-hud ctx (init-hud size nrepl))))

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
  (if (-> ctx (preview-hud) (h/scroll-offset) (zero?))
    ctx
    (-> ctx
        (with-persisted (-> ctx (persisted-hud) (h/scroll-stop)))
        (with-unrefreshed-preview (-> ctx (preview-hud) (h/scroll-stop))))))

(s/defn eval-at :- Context
  [ctx :- Context,
   f   :- (=> NReplClient NReplClient)]
  (let [clipboard   (-> ctx (input-area) (:clipboard))
        then-server (-> ctx (nrepl-client) (f))
        then-text (-> then-server
                        (r/then)
                        (i/end)
                        (i/reset-clipboard clipboard))]
    (-> ctx
        (with-client then-server)
        (with-input-area then-text)
        (refresh))))

(s/defn prev-eval :- Context
  [ctx :- Context]
  (eval-at ctx r/travel-back))

(s/defn next-eval :- Context
  [ctx :- Context]
  (eval-at ctx r/travel-forward))

(s/defn evaluate :- Context
  [ctx :- Context]
  (let [current-input (input-area ctx)
        client'       (-> ctx (nrepl-client) (r/evaluate! current-input))
        result        (r/result client')
        new-hud       (-> ctx
                          (persisted-hud)
                          (h/enrich-with [current-input result caret]))]
    (-> ctx
        (with-client client')
        (with-input-area i/empty-line)
        (with-hud new-hud))))

(s/defn suggestion-window :- Hud
  [ctx :- Context]
  (let [text  (input-area ctx)
        repl  (nrepl-client ctx)
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
        repl (nrepl-client ctx)
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
        repl (nrepl-client ctx)
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

(s/defn reformat :- Context
  [ctx :- Context]
  (let [formatted (-> ctx (input-area) (f/format-text))]
    (-> ctx (with-input-area formatted) (refresh))))

(s/defn inject :- Context
  [ctx   :- Context
   event :- e/Event]
  (let [repl (nrepl-client ctx)
        _    (->> event (:value) (i/from-string) (r/evaluate! repl))]
    ctx))

(s/defn input :- Context
  [ctx  :- Context
   f    :- (=> Text Text)]
  (let [new-input (-> ctx (input-area) (f))]
    (-> ctx (with-input-area new-input) (refresh))))

(s/defn continue :- ProcessingStep
  [ctx :- Context]
  {:status  :continue
   :context ctx})

(s/defn terminate :- ProcessingStep
  [ctx :- Context]
  {:status  :terminate
   :context ctx})

(s/defn process :- ProcessingStep
  [ctx    :- Context
   config :- Config
   event  :- e/Event]
  (letfn [(perform [ctx f]
            (-> ctx (gc config) (scroll-stop) (reset-suggestions) (reset-documentation) (reset-signatures) (input f) (calibrate) (highlight config) (match-parens config) (diff-render) (continue)))]
    (condp = (:action event)
      e/inject (-> ctx (inject event) (diff-render) (continue))
      e/docs (-> ctx (gc config) (scroll-stop) (reset-suggestions) (reset-signatures) (deselect) (document) (match-parens config) (diff-render) (continue))
      e/signature (-> ctx (gc config) (scroll-stop) (reset-suggestions) (reset-documentation) (deselect) (signature) (match-parens config) (diff-render) (continue))
      e/suggest (-> ctx (gc config) (scroll-stop) (reset-documentation) (reset-signatures) (suggest) (match-parens config) (diff-render) (continue))
      e/scroll-up (-> ctx (gc config) (scroll-up) (deselect) (highlight config) (diff-render) (continue))
      e/scroll-down (-> ctx (gc config) (scroll-down) (deselect) (highlight config) (diff-render) (continue))
      e/prev-eval (-> ctx (gc config) (scroll-stop) (reset-suggestions) (reset-documentation) (reset-signatures) (prev-eval) (highlight config) (match-parens config) (diff-render) (continue))
      e/next-eval (-> ctx (gc config) (scroll-stop) (reset-suggestions) (reset-documentation) (reset-signatures) (next-eval) (highlight config) (match-parens config) (diff-render) (continue))
      e/indent (-> ctx (gc config) (scroll-stop) (reset-suggestions) (reset-documentation) (reset-signatures) (deselect) (reformat) (highlight config) (match-parens config) (diff-render) (continue))
      e/clear (-> ctx (gc config) (scroll-stop) (reset-suggestions) (reset-documentation) (reset-signatures) (deselect) (clear) (highlight config) (match-parens config) (clear-render) (continue))
      e/evaluate (-> ctx (gc config) (scroll-stop) (reset-suggestions) (reset-documentation) (reset-signatures) (evaluate) (highlight config) (diff-render) (continue))
      e/exit (-> ctx (gc config) (scroll-stop) (deselect) (highlight config) (diff-render) (exit) (terminate))
      e/resize (-> ctx (resize event) (calibrate) (re-render) (continue))
      e/expand-select (-> ctx (perform i/expand-select))
      e/select-all (-> ctx (perform i/select-all))
      e/copy (-> ctx (perform i/copy))
      e/cut (-> ctx (perform i/cut))
      e/paste (-> ctx (perform i/paste))
      e/move-up (-> ctx (perform i/move-up))
      e/move-down (-> ctx (perform i/move-down))
      e/move-left (-> ctx (perform i/move-left))
      e/move-right (-> ctx (perform i/move-right))
      e/jump-left (-> ctx (perform i/jump-left))
      e/jump-right (-> ctx (perform i/jump-right))
      e/select-up (-> ctx (perform i/select-up))
      e/select-down (-> ctx (perform i/select-down))
      e/select-left (-> ctx (perform i/select-left))
      e/select-right (-> ctx (perform i/select-right))
      e/jump-select-left (-> ctx (perform i/jump-select-left))
      e/jump-select-right (-> ctx (perform i/jump-select-right))
      e/delete-previous (-> ctx (perform i/delete-previous))
      e/delete-current (-> ctx (perform i/delete-current))
      e/new-line (-> ctx (perform i/new-line))
      e/undo (-> ctx (perform i/undo))
      e/redo (-> ctx (perform i/redo))
      e/character (-> ctx (perform #(i/insert % (:value event))))
      e/ignore (-> ctx (continue))
      (continue ctx))))
