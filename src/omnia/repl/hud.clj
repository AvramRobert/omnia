(ns omnia.repl.hud
  (:require [schema.core :as s]
            [omnia.repl.view :as v]
            [omnia.repl.text :as t]
            [omnia.repl.format :as f]
            [omnia.schema.event :as e]
            [omnia.schema.hud :refer [Hud]]
            [omnia.schema.config :refer [Config]]
            [omnia.schema.render :refer [selection-highlight
                                         open-paren-highlight
                                         closed-paren-highlight
                                         manual-highlight
                                         HighlightInstructions
                                         HighlightInstructionData
                                         HighlightInstructionType
                                         RenderingStrategy]]
            [omnia.schema.view :refer [View]]
            [omnia.schema.text :refer [Text]]
            [omnia.schema.common :refer [Region]]
            [omnia.util.collection :refer [map-vals assoc-new]]
            [omnia.util.misc :refer [omnia-version]]))

(s/def caret :- Text (t/from-string "Ω =>"))
(s/def goodbye :- Text (t/from-string "Bye..for now\nFor even the very wise cannot see all ends"))
(s/def greeting :- Text (t/from-string (format "Welcome to Omnia (Ω) v%s" (omnia-version))))
(s/def clj-version :- Text (t/from-string (format "-- Clojure v%s --" (clojure-version))))
(s/def java-version :- Text (t/from-string (format "-- Java v%s --" (System/getProperty "java.version"))))
(s/def header :- Text
  (t/join
    greeting
    clj-version
    java-version
    t/empty-line
    caret))

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

(s/defn with-selection-highlight :- Hud
  [hud :- Hud, highlight :- HighlightInstructionData]
  (with-highlight hud selection-highlight highlight))

(s/defn with-parens-highlight :- Hud
  [hud :- Hud, open :- HighlightInstructionData, closed :- HighlightInstructionData]
  (-> hud
      (with-highlight open-paren-highlight open)
      (with-highlight closed-paren-highlight closed)))

(s/defn with-manual-highlight :- Hud
  [hud :- Hud, highlights :- HighlightInstructionData]
  (with-highlight hud manual-highlight highlights))

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
      (with-selection-highlight hud (create-selection-highlight config (:selection text)))
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
      (with-parens-highlight hud open closed))
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
  (let [view'  (-> hud
                   (view-size)
                   (v/empty-view-with-size)
                   (v/enrich-with [header]))]
    (switch-view hud view')))

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

(s/defn with-injection :- Hud
  [hud :- Hud
   text :- Text]
  (let [new-hud (-> hud
                    (persisted-view)
                    (v/enrich-with [text caret]))]
    (switch-view hud new-hud)))

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

(s/defn create-hud :- Hud
  "A `Hud` is the heads-up display of the REPL."
  [view-size  :- s/Int]
  (let [input     t/empty-line
        previous  (v/empty-view-with-size view-size)
        persisted (v/enrich-with previous [header])
        current   (v/enrich-with persisted [input])]
    {:render         :diff
     :previous-view  previous
     :persisted-view persisted
     :current-view   current
     :input-area     input
     :highlights     {}
     :garbage        {}}))
