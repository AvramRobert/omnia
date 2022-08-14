(ns omnia.repl.context
  (:require [schema.core :as s]
            [omnia.repl.hud :as h]
            [omnia.repl.store :as st]
            [omnia.repl.docs :as d]
            [omnia.repl.text :as t]
            [omnia.repl.view :as v]
            [omnia.repl.nrepl :as n]
            [omnia.schema.event :as e]
            [omnia.schema.text :refer [Text]]
            [omnia.schema.docs :refer [Docs]]
            [omnia.schema.store :refer [Store]]
            [omnia.schema.hud :refer [Hud]]
            [omnia.schema.context :refer [Context EventHandler processing terminated]]
            [omnia.schema.config :refer [Config]]
            [omnia.schema.event :refer [Event Action]]
            [omnia.schema.common :refer [=>]]
            [omnia.schema.nrepl :refer [NReplClient]]))

(s/defn hud :- Hud
  [context :- Context]
  (:hud context))

(s/defn docs :- Docs
  [context :- Context]
  (:docs context))

(s/defn store :- Store
  [context :- Context]
  (:store context))

(s/defn reset-docs :- Docs
  [context :- Context]
  d/empty-docs)

(s/defn reset-store :- Store
  [context :- Context]
  (-> context (store) (st/reset-eval-history)))

(s/defn continue :- Context
  [hud :- Hud
   context :- Context]
  {:status processing
   :hud    hud
   :docs   (reset-docs context)
   :store  (reset-store context)})

(s/defn terminate :- Context
  [hud :- Hud
   context :- Context]
  {:status terminated
   :hud    hud
   :docs   (reset-docs context)
   :store  (reset-store context)})

(s/defn documentation :- Context
  [context :- Context
   event :- Event
   config :- Config
   nrepl :- NReplClient]
  (let [docs           (docs context)
        hud            (hud context)
        current-input  (h/input-area hud)
        documentation  (d/documentation docs)
        documentation' (if (nil? documentation)
                         (-> nrepl (n/docs! current-input) (v/riffle-window 15))
                         (v/riffle documentation))]
    {:status processing
     :store  (reset-store context)
     :docs   (d/with-documentation docs documentation')
     :hud    (-> hud
                 (h/gc config)
                 (h/reset-scroll)
                 (h/deselect)
                 (h/with-popup documentation')
                 (h/match-parens config)
                 (h/diff-render))}))

(s/defn signature :- Context
  [context :- Context
   event :- Event
   config :- Config
   nrepl :- NReplClient]
  (let [docs          (docs context)
        hud           (hud context)
        current-input (h/input-area hud)
        signatures    (d/signatures docs)
        signatures'   (if (nil? signatures)
                        (-> nrepl (n/signature! current-input) (v/riffle-window 10))
                        (v/riffle signatures))]
    {:status processing
     :store  (reset-store context)
     :docs   (d/with-signatures docs signatures')
     :hud    (-> hud
                 (h/gc config)
                 (h/reset-scroll)
                 (h/deselect)
                 (h/with-popup signatures')
                 (h/match-parens config)
                 (h/diff-render))}))

(s/defn suggest :- Context
  [context :- Context
   event :- Event
   config :- Config
   nrepl :- NReplClient]
  (let [docs          (docs context)
        hud           (hud context)
        current-input (h/input-area hud)
        suggestions   (d/suggestions docs)
        suggestions'  (if (nil? suggestions)
                        (-> nrepl (n/complete! current-input) (v/riffle-window 10))
                        (v/riffle suggestions))]
    {:status processing
     :store  (reset-store context)
     :docs   (d/with-suggestions docs suggestions')
     :hud    (-> hud
                 (h/gc config)
                 (h/reset-scroll)
                 (h/with-popup-autocompleted suggestions')
                 (h/match-parens config)
                 (h/diff-render))}))

(s/defn scroll-down :- Context
  [context :- Context
   event :- Event
   config :- Config
   nrepl :- NReplClient]
  (-> context
      (hud)
      (h/gc config)
      (h/scroll-down)
      (h/deselect)
      (h/highlight config)
      (h/diff-render)
      (continue context)))

(s/defn scroll-up :- Context
  [context :- Context
   event :- Event
   config :- Config
   nrepl :- NReplClient]
  (-> context
      (hud)
      (h/gc config)
      (h/scroll-up)
      (h/deselect)
      (h/highlight config)
      (h/diff-render)
      (continue context)))

(s/defn prev-eval :- Context
  [context :- Context
   event :- Event
   config :- Config
   nrepl :- NReplClient]
  (let [hud        (hud context)
        store      (store context)
        store'     (if (-> store (st/eval-history) (st/temp) (nil?))
                     (-> store
                         (st/add-temporary (h/input-area hud))
                         (st/travel-to-previous-instant))
                     (st/travel-to-previous-instant store))
        evaluation (st/evaluation store')]
    {:status processing
     :docs   (reset-docs context)
     :store  store'
     :hud    (-> hud
                 (h/gc config)
                 (h/reset-scroll)
                 (h/switch-input-area evaluation)
                 (h/highlight config)
                 (h/match-parens config)
                 (h/diff-render))}))

(s/defn next-eval :- Context
  [context :- Context
   event :- Event
   config :- Config
   nrepl :- NReplClient]
  (let [hud        (hud context)
        store      (store context)
        store'     (if (-> store (st/eval-history) (st/temp) (nil?))
                     (-> store
                         (st/add-temporary (h/input-area hud))
                         (st/travel-to-next-instant))
                     (st/travel-to-next-instant store))
        evaluation (st/evaluation store')]
    {:status processing
     :docs   (reset-docs context)
     :store  store'
     :hud    (-> hud
                 (h/gc config)
                 (h/reset-scroll)
                 (h/switch-input-area evaluation)
                 (h/highlight config)
                 (h/match-parens config)
                 (h/diff-render))}))

;; FIXME: Rename this to: reformat
(s/defn indent :- Context
  [context :- Context
   event :- Event
   config :- Config
   nrepl :- NReplClient]
  (-> context
      (hud)
      (h/gc config)
      (h/reset-scroll)
      (h/deselect)
      (h/reformat)
      (h/highlight config)
      (h/match-parens config)
      (h/diff-render)
      (continue context)))

(s/defn clear :- Context
  [context :- Context
   event :- Event
   config :- Config
   nrepl :- NReplClient]
  (-> context
      (hud)
      (h/gc config)
      (h/reset-scroll)
      (h/deselect)
      (h/clear)
      (h/highlight config)
      (h/match-parens config)
      (h/clear-render)
      (continue context)))

(s/defn evaluate :- Context
  [context :- Context
   event :- Event
   config :- Config
   nrepl :- NReplClient]
  (let [hud        (hud context)
        store      (store context)
        input-area (h/input-area hud)
        result     (n/evaluate! nrepl input-area)]
    {:status processing
     :store  (st/add-to-eval-history store input-area)
     :docs   (reset-docs context)
     :hud    (-> hud
                 (h/gc config)
                 (h/reset-scroll)
                 (h/with-evaluation result)
                 (h/highlight config)
                 (h/diff-render))}))

(s/defn inject :- Context
  [context :- Context
   event :- Event
   config :- Config
   nrepl :- NReplClient]
  (let [hud        (hud context)
        result     (->> event (:value) (t/from-string) (n/evaluate! nrepl))]
    {:status processing
     :store  (store context)
     :docs   (reset-docs context)
     :hud    (-> hud
                 (h/gc config)
                 (h/reset-scroll)
                 (h/with-injection result)
                 (h/highlight config)
                 (h/diff-render))}))

(s/defn exit :- Context
  [context :- Context
   event :- Event
   config :- Config
   nrepl :- NReplClient]
  (-> context
      (hud)
      (h/gc config)
      (h/reset-scroll)
      (h/deselect)
      (h/highlight config)
      (h/diff-render)
      (h/exit)
      (terminate context)))

(s/defn resize :- Context
  [context :- Context
   event :- Event
   config :- Config
   nrepl :- NReplClient]
  (-> context
      (hud)
      (h/resize event)
      (h/calibrate)
      (h/re-render)
      (continue context)))

(s/defn ignore :- Context
  [context :- Context
   event :- Event
   config :- Config
   nrepl :- NReplClient]
  (-> context (hud) (continue context)))

(s/defn text-event :- Context
  [context :- Context
   event :- Event
   config :- Config
   nrepl :- NReplClient
   f :- (=> Text Text)]
  (let [hud         (hud context)
        input-area' (-> hud (h/input-area) (f))]
    {:status processing
     :store  (reset-store context)
     :docs   (reset-docs context)
     :hud    (-> hud
                 (h/gc config)
                 (h/reset-scroll)
                 (h/switch-input-area input-area')
                 (h/calibrate)
                 (h/highlight config)
                 (h/match-parens config)
                 (h/diff-render))}))

(s/defn character :- Context
  [context :- Context
   event :- Event
   config :- Config
   nrepl :- NReplClient]
  (text-event context event config nrepl #(t/insert % (:value event))))

(s/defn move-up :- Context
  [context :- Context
   event :- Event
   config :- Config
   nrepl :- NReplClient]
  (text-event context event config nrepl t/move-up))

(s/defn move-down :- Context
  [context :- Context
   event :- Event
   config :- Config
   nrepl :- NReplClient]
  (text-event context event config nrepl t/move-down))

(s/defn move-left :- Context
  [context :- Context
   event :- Event
   config :- Config
   nrepl :- NReplClient]
  (text-event context event config nrepl t/move-left))

(s/defn move-right :- Context
  [context :- Context
   event :- Event
   config :- Config
   nrepl :- NReplClient]
  (text-event context event config nrepl t/move-right))

(s/defn jump-left :- Context
  [context :- Context
   event :- Event
   config :- Config
   nrepl :- NReplClient]
  (text-event context event config nrepl t/jump-left))

(s/defn jump-right :- Context
  [context :- Context
   event :- Event
   config :- Config
   nrepl :- NReplClient]
  (text-event context event config nrepl t/jump-right))

(s/defn select-all :- Context
  [context :- Context
   event :- Event
   config :- Config
   nrepl :- NReplClient]
  (text-event context event config nrepl t/select-all))

(s/defn select-up :- Context
  [context :- Context
   event :- Event
   config :- Config
   nrepl :- NReplClient]
  (text-event context event config nrepl t/select-up))

(s/defn select-down :- Context
  [context :- Context
   event :- Event
   config :- Config
   nrepl :- NReplClient]
  (text-event context event config nrepl t/select-down))

(s/defn select-right :- Context
  [context :- Context
   event :- Event
   config :- Config
   nrepl :- NReplClient]
  (text-event context event config nrepl t/select-right))

(s/defn select-left :- Context
  [context :- Context
   event :- Event
   config :- Config
   nrepl :- NReplClient]
  (text-event context event config nrepl t/select-left))

(s/defn jump-select-left :- Context
  [context :- Context
   event :- Event
   config :- Config
   nrepl :- NReplClient]
  (text-event context event config nrepl t/jump-select-left))

(s/defn jump-select-right :- Context
  [context :- Context
   event :- Event
   config :- Config
   nrepl :- NReplClient]
  (text-event context event config nrepl t/jump-select-right))

(s/defn expand-selection :- Context
  [context :- Context
   event :- Event
   config :- Config
   nrepl :- NReplClient]
  (text-event context event config nrepl t/expand-selection))

(s/defn copy :- Context
  [context :- Context
   event :- Event
   config :- Config
   nrepl :- NReplClient]
  (text-event context event config nrepl t/copy))

(s/defn cut :- Context
  [context :- Context
   event :- Event
   config :- Config
   nrepl :- NReplClient]
  (text-event context event config nrepl t/cut))

(s/defn paste :- Context
  [context :- Context
   event :- Event
   config :- Config
   nrepl :- NReplClient]
  (text-event context event config nrepl t/paste))

(s/defn delete-previous :- Context
  [context :- Context
   event :- Event
   config :- Config
   nrepl :- NReplClient]
  (text-event context event config nrepl t/delete-previous))

(s/defn delete-current :- Context
  [context :- Context
   event :- Event
   config :- Config
   nrepl :- NReplClient]
  (text-event context event config nrepl t/delete-current))

(s/defn new-line :- Context
  [context :- Context
   event :- Event
   config :- Config
   nrepl :- NReplClient]
  (text-event context event config nrepl t/new-line))

(s/defn undo :- Context
  [context :- Context
   event :- Event
   config :- Config
   nrepl :- NReplClient]
  (text-event context event config nrepl t/undo))

(s/defn redo :- Context
  [context :- Context
   event :- Event
   config :- Config
   nrepl :- NReplClient]
  (text-event context event config nrepl t/redo))

(s/def handlers :- {Action EventHandler}
  {e/inject            inject
   e/docs              documentation
   e/signature         signature
   e/suggest           suggest
   e/scroll-up         scroll-up
   e/scroll-down       scroll-down
   e/prev-eval         prev-eval
   e/next-eval         next-eval
   e/indent            indent
   e/clear             clear
   e/evaluate          evaluate
   e/exit              exit
   e/resize            resize
   e/move-up           move-up
   e/move-down         move-down
   e/move-left         move-left
   e/move-right        move-right
   e/jump-left         jump-left
   e/jump-right        jump-right
   e/select-all        select-all
   e/select-up         select-up
   e/select-down       select-down
   e/select-left       select-left
   e/select-right      select-right
   e/jump-select-left  jump-select-left
   e/jump-select-right jump-select-right
   e/delete-previous   delete-previous
   e/delete-current    delete-current
   e/new-line          new-line
   e/copy              copy
   e/cut               cut
   e/paste             paste
   e/undo              undo
   e/redo              redo
   e/character         character
   e/expand-selection  expand-selection})

(s/defn process :- Context
  [context :- Context
   event :- Event
   config :- Config
   nrepl :- NReplClient]
  (let [action  (:action event)
        handler (get handlers action ignore)]
    (handler context event config nrepl)))

(s/defn context-from :- Context
  [hud :- Hud
   store :- Store]
  {:status processing
   :store  store
   :docs   d/empty-docs
   :hud    hud})