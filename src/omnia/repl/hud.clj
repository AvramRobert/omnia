(ns omnia.repl.hud
  (:require [schema.core :as s]
            [omnia.repl.nrepl :as r]
            [omnia.repl.view :as h]
            [omnia.repl.text :as i]
            [omnia.repl.format :as f]
            [omnia.schema.event :as e]
            [omnia.schema.hud :refer [Hud ProcessingStep]]
            [omnia.schema.config :refer [Config]]
            [omnia.schema.render :refer [HighlightInstructions HighlightInstructionData HighlightInstructionType RenderingStrategy]]
            [omnia.schema.view :refer [View]]
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

(s/defn init-view :- View
  [size :- s/Int,
   repl :- NReplClient]
  (let [header (header (:host repl) (:port repl))]
    (-> (h/view-of size)
        (h/enrich-with header))))

(s/defn hud :- Hud
  "A `Hud` is the heads-up display of the REPL."
  [view-size :- s/Int
   repl      :- NReplClient]
  (let [input     i/empty-line
        previous  (h/view-of view-size)
        persisted (init-view view-size repl)
        current   (h/enrich-with persisted [input])]
    {:nrepl          repl
     :render         :diff
     :previous-view  previous
     :persisted-view persisted
     :current-view   current
     :input-area     input
     :suggestions    h/empty-view
     :documentation  h/empty-view
     :signatures     h/empty-view
     :highlights     {}
     :garbage        {}}))

(s/defn current-view :- View
  [hud :- Hud]
  (:current-view hud))

(s/defn persisted-view :- View
  [hud :- Hud]
  (:persisted-view hud))

(s/defn previous-view :- View
  [hud :- Hud]
  (:previous-view hud))

(s/defn input-area :- Text
  [hud :- Hud]
  (:input-area hud))

(s/defn view-size :- s/Int
  [hud :- Hud]
  (-> hud (persisted-view) (h/field-of-view)))

(s/defn suggestions :- View
  [hud :- Hud]
  (:suggestions hud))

(s/defn signatures :- View
  [hud :- Hud]
  (:signatures hud))

(s/defn documentation :- View
  [hud :- Hud]
  (:documentation hud))

(s/defn nrepl-client :- NReplClient
  [hud :- Hud]
  (:nrepl hud))

(s/defn highlights :- HighlightInstructions
  [hud :- Hud]
  (:highlights hud))

(s/defn garbage :- HighlightInstructions
  [hud :- Hud]
  (:garbage hud))

(s/defn rendering :- RenderingStrategy
  [hud :- Hud]
  (:render hud))

(s/defn with-previous-view :- Hud
  [hud :- Hud, view :- View]
  (assoc hud :previous-view view))

(s/defn with-current-view :- Hud
  [hud :- Hud, view :- View]
  (assoc hud :current-view view))

(s/defn with-persisted-view :- Hud
  [hud :- Hud, view :- View]
  (assoc hud :persisted-view view))

(s/defn switch-current-view :- Hud
  [hud :- Hud, view :- View]
  (-> hud
      (with-previous-view (current-view hud))
      (with-current-view view)))

(s/defn refresh-view :- Hud
  [hud :- Hud]
  (switch-current-view hud (-> hud (persisted-view) (h/enrich-with [(input-area hud)]))))

(s/defn switch-view :- Hud
  [hud :- Hud, view :- View]
  (-> hud (with-persisted-view view) (refresh-view)))

(s/defn reset-highlights :- Hud
  [hud :- Hud]
  (assoc-new hud :highlights {}))

(s/defn reset-garbage :- Hud
  [hud :- Hud]
  (assoc-new hud :garbage {}))

(s/defn with-garbage :- Hud
  [hud :- Hud, highlights :- HighlightInstructions]
  (assoc hud :garbage highlights))

(s/defn with-highlight :- Hud
  [hud :- Hud,
   h-type :- HighlightInstructionType,
   highlight :- HighlightInstructionData]
  (assoc-in hud [:highlights h-type] highlight))

(s/defn with-selection :- Hud
  [hud :- Hud, highlight :- HighlightInstructionData]
  (with-highlight hud :selection highlight))

(s/defn with-parens :- Hud
  [hud :- Hud, open :- HighlightInstructionData, closed :- HighlightInstructionData]
  (-> hud
      (with-highlight :open-paren open)
      (with-highlight :closed-paren closed)))

(s/defn with-manual :- Hud
  [hud :- Hud, highlights :- HighlightInstructionData]
  (with-highlight hud :manual highlights))

(s/defn create-manual-highlight :- HighlightInstructionData
  [config :- Config, region :- Region]
  {:region region
   :scheme (-> config (:syntax) (:clean-up))
   :styles []})

(s/defn create-selection-highlight :- HighlightInstructionData
  [config :- Config, region :- Region]
  {:region region
   :scheme (-> config (:syntax) (:selection))
   :styles []})

(s/defn create-paren-highlight :- HighlightInstructionData
  [config :- Config, region :- Region]
  {:region region
   :scheme (-> config (:syntax) (:clean-up))
   :styles [:underline]})

(s/defn create-garbage-highlight :- HighlightInstructionData
  [config :- Config, region :- Region]
  {:region region
   :scheme (-> config (:syntax) (:clean-up))
   :styles []})

(s/defn with-input-area [hud :- Hud, input :- Text] :- Hud
  (let [clipboard (or (:clipboard input)
                      (-> hud (input-area) (:clipboard)))
        new-text  (assoc input :clipboard clipboard)]
    (assoc hud :input-area new-text)))

(s/defn with-client :- Hud
  [hud :- Hud, repl :- NReplClient]
  (assoc hud :nrepl repl))

(s/defn with-suggestions :- Hud
  [hud :- Hud, suggestions :- View]
  (assoc-new hud :suggestions suggestions))

(s/defn with-documentation :- Hud
  [hud :- Hud, documentation :- View]
  (assoc-new hud :documentation documentation))

(s/defn with-signatures :- Hud
  [hud :- Hud, signatures :- View]
  (assoc-new hud :signatures signatures))

(s/defn with-render :- Hud
  [hud :- Hud, render :- RenderingStrategy]
  (assoc-new hud :render render))

(s/defn reset-suggestions :- Hud
  [hud :- Hud]
  (with-suggestions hud h/empty-view))

(s/defn reset-documentation :- Hud
  [hud :- Hud]
  (with-documentation hud h/empty-view))

(s/defn reset-signatures :- Hud
  [hud :- Hud]
  (with-signatures hud h/empty-view))

(s/defn re-render :- Hud
  [hud :- Hud]
  (with-render hud :total))

(s/defn diff-render :- Hud
  [hud :- Hud]
  (with-render hud :diff))

(s/defn clear-render :- Hud
  [hud :- Hud]
  (with-render hud :clear))

(s/defn highlight :- Hud
  [hud :- Hud
   config :- Config]
  (let [text (-> hud (current-view) (h/text))]
    (if (i/selecting? text)
      (with-selection hud (create-selection-highlight config (:selection text)))
      hud)))

(s/defn gc :- Hud
  [hud :- Hud
   config :- Config]
  (let [garbage (->> hud
                     (highlights)
                     (map-vals #(create-garbage-highlight config (:region %))))]
    (-> hud (with-garbage garbage) (reset-highlights))))

(s/defn match :- Hud
  [hud :- Hud
   config :- Config]
  (if-let [pair (-> hud (current-view) (h/text) (i/find-pair))]
    (let [open   (create-paren-highlight config (:left pair))
          closed (create-paren-highlight config (:right pair))]
      (with-parens hud open closed))
    hud))

(s/defn match-parens :- Hud
  [hud :- Hud
   config :- Config]
  (let [text (-> hud (current-view) (h/text))]
    (cond
      (i/open-pairs (i/current-char text)) (match hud config)
      (i/closed-pairs (i/previous-char text)) (match hud config)
      :else hud)))

(s/defn calibrate :- Hud
  [hud :- Hud]
  (let [nov       (h/correct-between (current-view hud)
                                     (previous-view hud))
        persisted (-> hud
                      (persisted-view)
                      (h/with-view-offset nov))
        preview   (-> hud
                      (current-view)
                      (h/with-view-offset nov))]
    (-> hud
        (with-persisted-view persisted)
        (with-current-view preview))))

(s/defn resize :- Hud
  [hud :- Hud
   event :- e/Event]
  (let [persisted (persisted-view hud)
        new-fov   (-> event (:value) (second))]
    (-> hud
        (with-persisted-view (h/resize persisted new-fov))
        (refresh-view))))

(s/defn clear :- Hud
  [hud :- Hud]
  (let [size  (view-size hud)
        nrepl (nrepl-client hud)]
    (switch-view hud (init-view size nrepl))))

(s/defn exit :- Hud
  [hud :- Hud]
  (let [preview (-> hud
                    (current-view)
                    (h/enrich-with [goodbye])
                    (h/reset-view-offset))]
    (switch-current-view hud preview)))

(s/defn deselect :- Hud
  [hud :- Hud]
  (let [preview   (-> hud (current-view) (h/deselect))
        persisted (-> hud (persisted-view) (h/deselect))
        input     (-> hud (input-area) (i/deselect))]
    (-> hud
        (with-current-view preview)
        (with-persisted-view persisted)
        (with-input-area input))))

(s/defn scroll-up :- Hud
  [hud :- Hud]
  (let [preview (-> hud (current-view) (h/scroll-up))]
    (switch-current-view hud preview)))

(s/defn scroll-down :- Hud
  [hud :- Hud]
  (let [preview (-> hud (current-view) (h/scroll-down))]
    (switch-current-view hud preview)))

(s/defn reset-scroll :- Hud
  [hud :- Hud]
  (if (-> hud (current-view) (h/scroll-offset) (zero?))
    hud
    (-> hud
        (with-persisted-view (-> hud (persisted-view) (h/reset-scroll)))
        (with-current-view (-> hud (current-view) (h/reset-scroll))))))

(s/defn eval-at :- Hud
  [hud :- Hud,
   f   :- (=> NReplClient NReplClient)]
  (let [clipboard   (-> hud (input-area) (:clipboard))
        then-server (-> hud (nrepl-client) (f))
        then-text (-> then-server
                        (r/then)
                        (i/end)
                        (i/reset-clipboard clipboard))]
    (-> hud
        (with-client then-server)
        (with-input-area then-text)
        (refresh-view))))

(s/defn prev-eval :- Hud
  [hud :- Hud]
  (eval-at hud r/travel-back))

(s/defn next-eval :- Hud
  [hud :- Hud]
  (eval-at hud r/travel-forward))

(s/defn evaluate :- Hud
  [hud :- Hud]
  (let [current-input (input-area hud)
        client'       (-> hud (nrepl-client) (r/evaluate! current-input))
        result        (r/result client')
        new-view      (-> hud
                          (persisted-view)
                          (h/enrich-with [current-input result caret]))]
    (-> hud
        (with-client client')
        (with-input-area i/empty-line)
        (switch-view new-view))))

(s/defn suggestion-window :- View
  [hud :- Hud]
  (let [text  (input-area hud)
        repl  (nrepl-client hud)
        suggs (suggestions hud)]
    (if (h/hollow? suggs)
      (-> repl (r/complete! text) (r/result) (h/riffle-window 10))
      (h/riffle suggs))))

(s/defn suggest :- Hud
  [hud :- Hud]
  (let [suggestions (suggestion-window hud)
        suggestion  (h/current-line suggestions)
        text        (-> hud
                        (input-area)
                        (i/auto-complete suggestion))
        preview     (-> hud
                        (persisted-view)
                        (h/enrich-with [text])
                        (h/pop-up suggestions))]
    (-> hud
        (with-suggestions suggestions)
        (with-input-area text)
        (switch-current-view preview)
        (deselect))))

(s/defn signature-window :- View
  [hud :- Hud]
  (let [text (input-area hud)
        repl (nrepl-client hud)
        sigs (signatures hud)]
    (if (h/hollow? sigs)
      (-> repl (r/signature! text) (r/result) (h/riffle-window 10))
      (h/riffle sigs))))

(s/defn signature :- Hud
  [hud :- Hud]
  (let [signatures (signature-window hud)
        text       (input-area hud)
        preview    (-> hud
                      (persisted-view)
                      (h/enrich-with [text])
                      (h/pop-up signatures))]
    (-> hud
        (with-signatures signatures)
        (switch-current-view preview))))

(s/defn documentation-window :- View
  [hud :- Hud]
  (let [text (input-area hud)
        repl (nrepl-client hud)
        docs (documentation hud)]
    (if (h/hollow? docs)
      (-> repl (r/docs! text) (r/result) (h/riffle-window 15))
      (h/riffle docs))))

(s/defn document :- Hud
  [hud :- Hud]
  (let [documentation (documentation-window hud)
        text          (input-area hud)
        preview       (-> hud
                          (persisted-view)
                          (h/enrich-with [text])
                          (h/pop-up documentation))]
    (-> hud
        (with-documentation documentation)
        (switch-current-view preview))))

(s/defn reformat :- Hud
  [hud :- Hud]
  (let [formatted (-> hud (input-area) (f/format-text))]
    (-> hud (with-input-area formatted) (refresh-view))))

(s/defn inject :- Hud
  [hud   :- Hud
   event :- e/Event]
  (let [repl (nrepl-client hud)
        _    (->> event (:value) (i/from-string) (r/evaluate! repl))]
    hud))

(s/defn input :- Hud
  [hud  :- Hud
   f    :- (=> Text Text)]
  (let [new-input (-> hud (input-area) (f))]
    (-> hud (with-input-area new-input) (refresh-view))))

(s/defn continue :- ProcessingStep
  [hud :- Hud]
  {:status :continue
   :hud    hud})

(s/defn terminate :- ProcessingStep
  [hud :- Hud]
  {:status :terminate
   :hud    hud})

(s/defn process :- ProcessingStep
  [hud    :- Hud
   config :- Config
   event  :- e/Event]
  (letfn [(perform [hud f]
            (-> hud (gc config) (reset-scroll) (reset-suggestions) (reset-documentation) (reset-signatures) (input f) (calibrate) (highlight config) (match-parens config) (diff-render) (continue)))]
    (condp = (:action event)
      e/inject (-> hud (inject event) (diff-render) (continue))
      e/docs (-> hud (gc config) (reset-scroll) (reset-suggestions) (reset-signatures) (deselect) (document) (match-parens config) (diff-render) (continue))
      e/signature (-> hud (gc config) (reset-scroll) (reset-suggestions) (reset-documentation) (deselect) (signature) (match-parens config) (diff-render) (continue))
      e/suggest (-> hud (gc config) (reset-scroll) (reset-documentation) (reset-signatures) (suggest) (match-parens config) (diff-render) (continue))
      e/scroll-up (-> hud (gc config) (scroll-up) (deselect) (highlight config) (diff-render) (continue))
      e/scroll-down (-> hud (gc config) (scroll-down) (deselect) (highlight config) (diff-render) (continue))
      e/prev-eval (-> hud (gc config) (reset-scroll) (reset-suggestions) (reset-documentation) (reset-signatures) (prev-eval) (highlight config) (match-parens config) (diff-render) (continue))
      e/next-eval (-> hud (gc config) (reset-scroll) (reset-suggestions) (reset-documentation) (reset-signatures) (next-eval) (highlight config) (match-parens config) (diff-render) (continue))
      e/indent (-> hud (gc config) (reset-scroll) (reset-suggestions) (reset-documentation) (reset-signatures) (deselect) (reformat) (highlight config) (match-parens config) (diff-render) (continue))
      e/clear (-> hud (gc config) (reset-scroll) (reset-suggestions) (reset-documentation) (reset-signatures) (deselect) (clear) (highlight config) (match-parens config) (clear-render) (continue))
      e/evaluate (-> hud (gc config) (reset-scroll) (reset-suggestions) (reset-documentation) (reset-signatures) (evaluate) (highlight config) (diff-render) (continue))
      e/exit (-> hud (gc config) (reset-scroll) (deselect) (highlight config) (diff-render) (exit) (terminate))
      e/resize (-> hud (resize event) (calibrate) (re-render) (continue))
      e/expand-select (-> hud (perform i/expand-select))
      e/select-all (-> hud (perform i/select-all))
      e/copy (-> hud (perform i/copy))
      e/cut (-> hud (perform i/cut))
      e/paste (-> hud (perform i/paste))
      e/move-up (-> hud (perform i/move-up))
      e/move-down (-> hud (perform i/move-down))
      e/move-left (-> hud (perform i/move-left))
      e/move-right (-> hud (perform i/move-right))
      e/jump-left (-> hud (perform i/jump-left))
      e/jump-right (-> hud (perform i/jump-right))
      e/select-up (-> hud (perform i/select-up))
      e/select-down (-> hud (perform i/select-down))
      e/select-left (-> hud (perform i/select-left))
      e/select-right (-> hud (perform i/select-right))
      e/jump-select-left (-> hud (perform i/jump-select-left))
      e/jump-select-right (-> hud (perform i/jump-select-right))
      e/delete-previous (-> hud (perform i/delete-previous))
      e/delete-current (-> hud (perform i/delete-current))
      e/new-line (-> hud (perform i/new-line))
      e/undo (-> hud (perform i/undo))
      e/redo (-> hud (perform i/redo))
      e/character (-> hud (perform #(i/insert % (:value event))))
      e/ignore (-> hud (continue))
      (continue hud))))
