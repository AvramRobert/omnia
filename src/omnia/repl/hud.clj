(ns omnia.repl.hud
  (:require [schema.core :as s]
            [omnia.repl.nrepl :as n]
            [omnia.repl.view :as v]
            [omnia.repl.text :as t]
            [omnia.repl.format :as f]
            [omnia.schema.event :as e]
            [omnia.schema.hud :refer [Hud]]
            [omnia.schema.config :refer [Config]]
            [omnia.schema.render :refer [HighlightInstructions HighlightInstructionData HighlightInstructionType RenderingStrategy]]
            [omnia.schema.view :refer [View]]
            [omnia.schema.text :refer [Text]]
            [omnia.schema.nrepl :refer [NReplClient]]
            [omnia.schema.common :refer [=> Region]]
            [omnia.util.collection :refer [map-vals assoc-new]]
            [omnia.util.misc :refer [omnia-version]]))

(def caret (t/from-string "Ω =>"))
(def goodbye (t/from-string "Bye..for now\nFor even the very wise cannot see all ends"))
(def greeting (t/from-string (format "Welcome to Omnia! (Ω) v%s" (omnia-version))))
(def clj-version (t/from-string (format "-- Clojure v%s --" (clojure-version))))
(def java-version (t/from-string (format "-- Java v%s --" (System/getProperty "java.version"))))
(defn nrepl-info [host port] (t/from-string (str "-- nREPL server started on nrepl://" host ":" port " --")))

(s/defn header :- [Text]
  [host :- s/Str
   port :- s/Int]
  (let [repl-info (nrepl-info host port)]
    [greeting
     repl-info
     clj-version
     java-version
     t/empty-line
     caret]))

(s/defn init-view :- View
  [size :- s/Int,
   repl :- NReplClient]
  (let [header (header (:host repl) (:port repl))]
    (-> (v/empty-view-with-size size)
        (v/enrich-with header))))

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
  (-> hud (persisted-view) (v/field-of-view)))

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

(s/defn with-input-area :- Hud
  [hud :- Hud, input :- Text]
  (let [clipboard (or (:clipboard input)
                      (-> hud (input-area) (:clipboard)))
        new-text  (assoc input :clipboard clipboard)]
    (assoc hud :input-area new-text)))

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
  (switch-current-view hud (-> hud (persisted-view) (v/enrich-with [(input-area hud)]))))

(s/defn switch-view :- Hud
  [hud :- Hud, view :- View]
  (-> hud (with-persisted-view view) (refresh-view)))

(s/defn switch-input-area :- Hud
  [hud :- Hud,
   text :- Text]
  (-> hud (with-input-area text) (refresh-view)))

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

(s/defn with-client :- Hud
  [hud :- Hud, repl :- NReplClient]
  (assoc hud :nrepl repl))

(s/defn with-render :- Hud
  [hud :- Hud, render :- RenderingStrategy]
  (assoc-new hud :render render))

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
  (let [text (-> hud (current-view) (v/text))]
    (if (t/selecting? text)
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
  (if-let [pair (-> hud (current-view) (v/text) (t/find-pair))]
    (let [open   (create-paren-highlight config (:left pair))
          closed (create-paren-highlight config (:right pair))]
      (with-parens hud open closed))
    hud))

(s/defn match-parens :- Hud
  [hud :- Hud
   config :- Config]
  (let [text (-> hud (current-view) (v/text))]
    (cond
      (t/open-pairs (t/current-char text)) (match hud config)
      (t/closed-pairs (t/previous-char text)) (match hud config)
      :else hud)))

(s/defn calibrate :- Hud
  [hud :- Hud]
  (let [nov       (v/correct-between (current-view hud)
                                     (previous-view hud))
        persisted (-> hud
                      (persisted-view)
                      (v/with-view-offset nov))
        preview   (-> hud
                      (current-view)
                      (v/with-view-offset nov))]
    (-> hud
        (with-persisted-view persisted)
        (with-current-view preview))))

(s/defn resize :- Hud
  [hud :- Hud
   event :- e/Event]
  (let [persisted (persisted-view hud)
        new-fov   (-> event (:value) (second))]
    (-> hud
        (with-persisted-view (v/resize persisted new-fov))
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
                    (v/enrich-with [goodbye])
                    (v/reset-view-offset))]
    (switch-current-view hud preview)))

(s/defn deselect :- Hud
  [hud :- Hud]
  (let [preview   (-> hud (current-view) (v/deselect))
        persisted (-> hud (persisted-view) (v/deselect))
        input     (-> hud (input-area) (t/deselect))]
    (-> hud
        (with-current-view preview)
        (with-persisted-view persisted)
        (with-input-area input))))

(s/defn scroll-up :- Hud
  [hud :- Hud]
  (let [preview (-> hud (current-view) (v/scroll-up))]
    (switch-current-view hud preview)))

(s/defn scroll-down :- Hud
  [hud :- Hud]
  (let [preview (-> hud (current-view) (v/scroll-down))]
    (switch-current-view hud preview)))

(s/defn reset-scroll :- Hud
  [hud :- Hud]
  (if (-> hud (current-view) (v/scroll-offset) (zero?))
    hud
    (-> hud
        (with-persisted-view (-> hud (persisted-view) (v/reset-scroll)))
        (with-current-view (-> hud (current-view) (v/reset-scroll))))))

(s/defn eval-at :- Hud
  [hud :- Hud,
   f   :- (=> NReplClient NReplClient)]
  (let [clipboard   (-> hud (input-area) (:clipboard))
        then-server (-> hud (nrepl-client) (f))
        then-text (-> then-server
                      (n/then)
                      (t/end)
                      (t/reset-clipboard clipboard))]
    (-> hud
        (with-client then-server)
        (with-input-area then-text)
        (refresh-view))))

(s/defn prev-eval :- Hud
  [hud :- Hud]
  (eval-at hud n/travel-back))

(s/defn next-eval :- Hud
  [hud :- Hud]
  (eval-at hud n/travel-forward))

(s/defn with-evaluation :- Hud
  [hud :- Hud
   text :- Text]
  (let [current-input (input-area hud)
        new-view      (-> hud
                          (persisted-view)
                          (v/enrich-with [current-input text caret]))]
    (-> hud
        (with-input-area t/empty-line)
        (switch-view new-view))))

(s/defn with-popup-autocompleted :- Hud
  [hud :- Hud
   window :- View]
  (let [completion (v/current-line window)
        text       (-> hud
                       (input-area)
                       (t/auto-complete completion))
        preview    (-> hud
                       (persisted-view)
                       (v/enrich-with [text])
                       (v/pop-up window))]
    (-> hud
        (with-input-area text)
        (switch-current-view preview)
        (deselect))))

(s/defn with-popup :- Hud
  [hud :- Hud
   window :- View]
  (let [text       (input-area hud)
        preview    (-> hud
                       (persisted-view)
                       (v/enrich-with [text])
                       (v/pop-up window))]
    (switch-current-view hud preview)))

(s/defn reformat :- Hud
  [hud :- Hud]
  (let [formatted (-> hud (input-area) (f/format-text))]
    (-> hud (with-input-area formatted) (refresh-view))))

(s/defn inject :- Hud
  [hud   :- Hud
   event :- e/Event]
  (let [repl (nrepl-client hud)
        _    (->> event (:value) (t/from-string) (n/evaluate! repl))]
    hud))

(s/defn input :- Hud
  [hud  :- Hud
   f    :- (=> Text Text)]
  (let [new-input (-> hud (input-area) (f))]
    (-> hud (with-input-area new-input) (refresh-view))))

(s/defn create-hud :- Hud
  "A `Hud` is the heads-up display of the REPL."
  [view-size :- s/Int
   repl      :- NReplClient]
  (let [input     t/empty-line
        previous  (v/empty-view-with-size view-size)
        persisted (init-view view-size repl)
        current   (v/enrich-with persisted [input])]
    {:nrepl          repl
     :render         :diff
     :previous-view  previous
     :persisted-view persisted
     :current-view   current
     :input-area     input
     :highlights     {}
     :garbage        {}}))
