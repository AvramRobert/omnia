(ns omnia.repl.context
  (:require [schema.core :as s]
            [omnia.repl.hud :as h]
            [omnia.repl.information :as i]
            [omnia.repl.text :as t]
            [omnia.repl.view :as v]
            [omnia.repl.nrepl :as n]
            [omnia.repl.text-history :as th]
            [omnia.repl.eval-history :as eh]
            [omnia.schema.event :as e]
            [omnia.schema.text :refer [Text]]
            [omnia.schema.information :refer [Information]]
            [omnia.schema.hud :refer [Hud]]
            [omnia.schema.context :refer [Context EventHandler processing terminated]]
            [omnia.schema.config :refer [Config]]
            [omnia.schema.event :refer [Event Action]]
            [omnia.schema.common :refer [=>]]
            [omnia.schema.nrepl :refer [NReplClient]]
            [omnia.schema.text-history :refer [TextHistory]]
            [omnia.schema.eval-history :refer [EvalHistory]]))

(s/defn hud :- Hud
  [context :- Context]
  (:hud context))

(s/defn information :- Information
  [context :- Context]
  (:information context))

(s/defn undo-history :- TextHistory
  [context :- Context]
  (:undo-history context))

(s/defn redo-history :- TextHistory
  [context :- Context]
  (:redo-history context))

(s/defn eval-history :- EvalHistory
  [context :- Context]
  (:eval-history context))

(s/defn reset-information :- Information
  [context :- Context]
  i/empty-information)

(s/defn reset-eval-history :- EvalHistory
  [context :- Context]
  (-> context (eval-history) (eh/reset)))

(s/defn continue :- Context
  [hud :- Hud
   context :- Context]
  {:status       processing
   :hud          hud
   :undo-history (undo-history context)
   :redo-history (redo-history context)
   :eval-history (reset-eval-history context)
   :information  (reset-information context)})

(s/defn terminate :- Context
  [hud :- Hud
   context :- Context]
  {:status       terminated
   :hud          hud
   :undo-history (undo-history context)
   :redo-history (redo-history context)
   :eval-history (reset-eval-history context)
   :information  (reset-information context)})

(s/defn documentation :- Context
  [context :- Context
   event :- Event
   config :- Config
   nrepl :- NReplClient]
  (let [information   (information context)
        hud           (hud context)
        current-input (h/input-area hud)
        documentation (if (i/documentation? information)
                        (-> information (i/info-value) (v/riffle))
                        (-> nrepl (n/docs! current-input) (v/riffle-window 15)))]
    {:status       processing
     :undo-history (undo-history context)
     :redo-history (redo-history context)
     :eval-history (reset-eval-history context)
     :information  (i/create-documentation-info documentation)
     :hud          (-> hud
                       (h/gc config)
                       (h/reset-scroll)
                       (h/deselect)
                       (h/with-popup documentation)
                       (h/match-parens config)
                       (h/diff-render))}))

(s/defn signature :- Context
  [context :- Context
   event :- Event
   config :- Config
   nrepl :- NReplClient]
  (let [information   (information context)
        hud           (hud context)
        current-input (h/input-area hud)
        signature     (if (i/signature? information)
                        (-> information (i/info-value) (v/riffle))
                        (-> nrepl (n/signature! current-input) (v/riffle-window 10)))]
    {:status       processing
     :undo-history (undo-history context)
     :redo-history (redo-history context)
     :eval-history (reset-eval-history context)
     :information  (i/create-signature-info signature)
     :hud          (-> hud
                       (h/gc config)
                       (h/reset-scroll)
                       (h/deselect)
                       (h/with-popup signature)
                       (h/match-parens config)
                       (h/diff-render))}))

(s/defn suggestion :- Context
  [context :- Context
   event :- Event
   config :- Config
   nrepl :- NReplClient]
  (let [information   (information context)
        hud           (hud context)
        current-input (h/input-area hud)
        suggestion    (if (i/suggestion? information)
                        (-> information (i/info-value) (v/riffle))
                        (-> nrepl (n/complete! current-input) (v/riffle-window 10)))]
    {:status       processing
     :undo-history (undo-history context)
     :redo-history (redo-history context)
     :eval-history (reset-eval-history context)
     :information  (i/create-suggestion-info suggestion)
     :hud          (-> hud
                       (h/gc config)
                       (h/reset-scroll)
                       (h/with-popup-autocompleted suggestion)
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
  (let [hud           (hud context)
        eval-history  (eval-history context)
        eval-history' (eh/travel-back eval-history)
        evaluation    (eh/current-eval eval-history')]
    {:status       processing
     :information  (reset-information context)
     :undo-history (undo-history context)
     :redo-history (redo-history context)
     :eval-history eval-history'
     :hud          (if (some? evaluation)
                     (-> hud
                         (h/gc config)
                         (h/reset-scroll)
                         (h/switch-input-area evaluation)
                         (h/highlight config)
                         (h/match-parens config)
                         (h/diff-render))
                     hud)}))

(s/defn next-eval :- Context
  [context :- Context
   event :- Event
   config :- Config
   nrepl :- NReplClient]
  (let [hud           (hud context)
        eval-history  (eval-history context)
        eval-history' (eh/travel-forward eval-history)
        evaluation    (eh/current-eval eval-history')]
    {:status       processing
     :information  (reset-information context)
     :undo-history (undo-history context)
     :redo-history (redo-history context)
     :eval-history eval-history'
     :hud          (if (some? evaluation)
                     (-> hud
                         (h/gc config)
                         (h/reset-scroll)
                         (h/switch-input-area evaluation)
                         (h/highlight config)
                         (h/match-parens config)
                         (h/diff-render))
                     hud)}))

(s/defn reformat :- Context
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
  (let [hud          (hud context)
        eval-history (eval-history context)
        input-area   (h/input-area hud)
        result       (n/evaluate! nrepl input-area)]
    {:status       processing
     :information  (reset-information context)
     :undo-history (undo-history context)
     :redo-history (redo-history context)
     :eval-history (eh/insert input-area eval-history)
     :hud          (-> hud
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
  (let [hud    (hud context)
        result (->> event (:value) (t/from-string) (n/evaluate! nrepl))]
    {:status       processing
     :information  (reset-information context)
     :undo-history (undo-history context)
     :redo-history (redo-history context)
     :eval-history (reset-eval-history context)
     :hud          (-> hud
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

(s/defn change-input-area :- Hud
  [hud :- Hud
   config :- Config
   f :- (=> Text Text)]
  (let [input-area' (-> hud (h/input-area) (f))]
    (-> hud
        (h/gc config)
        (h/reset-scroll)
        (h/switch-input-area input-area')
        (h/calibrate)
        (h/highlight config)
        (h/match-parens config)
        (h/diff-render))))

(s/defn undo :- Context
  [context :- Context
   event :- Event
   config :- Config
   nrepl :- NReplClient]
  (let [hud          (hud context)
        input-area   (-> hud (h/input-area) (t/reset))
        undo-history (undo-history context)
        redo-history (redo-history context)
        old-record   (th/next-record undo-history)]
    {:status       processing
     :information  (reset-information context)
     :eval-history (reset-eval-history context)
     :redo-history (if (some? old-record)
                     (th/insert input-area redo-history)
                     redo-history)
     :undo-history (if (some? old-record)
                     (th/revert undo-history)
                     undo-history)
     :hud          (if (some? old-record)
                     (change-input-area hud config (constantly old-record))
                     hud)}))

(s/defn redo :- Context
  [context :- Context
   event :- Event
   config :- Config
   nrepl :- NReplClient]
  (let [hud          (hud context)
        undo-history (undo-history context)
        redo-history (redo-history context)
        input-area   (-> hud (h/input-area) (t/reset))
        old-record   (th/next-record redo-history)]
    {:status       processing
     :information  (reset-information context)
     :eval-history (reset-eval-history context)
     :undo-history (if (some? old-record)
                     (th/insert input-area undo-history)
                     undo-history)
     :redo-history (if (some? old-record)
                     (th/revert redo-history)
                     redo-history)
     :hud          (if (some? old-record)
                     (change-input-area hud config (constantly old-record))
                     hud)}))

(s/defn undoable-text-event :- Context
  [context :- Context
   event :- Event
   config :- Config
   nrepl :- NReplClient
   f :- (=> Text Text)]
  (let [hud          (hud context)
        undo-history (undo-history context)
        input-area   (-> hud (h/input-area) (t/reset))]
    {:status       processing
     :information  (reset-information context)
     :redo-history (redo-history context)
     :undo-history (th/insert input-area undo-history)
     :eval-history (reset-eval-history context)
     :hud          (change-input-area hud config f)}))

(s/defn text-event :- Context
  [context :- Context
   event :- Event
   config :- Config
   nrepl :- NReplClient
   f :- (=> Text Text)]
  (let [hud (hud context)]
    {:status       processing
     :information  (reset-information context)
     :undo-history (undo-history context)
     :redo-history (redo-history context)
     :eval-history (reset-eval-history context)
     :hud          (change-input-area hud config f)}))

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
  (undoable-text-event context event config nrepl t/cut))

(s/defn paste :- Context
  [context :- Context
   event :- Event
   config :- Config
   nrepl :- NReplClient]
  (undoable-text-event context event config nrepl t/paste))

(s/defn delete-previous :- Context
  [context :- Context
   event :- Event
   config :- Config
   nrepl :- NReplClient]
  (undoable-text-event context event config nrepl t/delete-previous))

(s/defn delete-current :- Context
  [context :- Context
   event :- Event
   config :- Config
   nrepl :- NReplClient]
  (undoable-text-event context event config nrepl t/delete-current))

(s/defn character :- Context
  [context :- Context
   event :- Event
   config :- Config
   nrepl :- NReplClient]
  (undoable-text-event context event config nrepl #(t/insert % (:value event))))

(s/defn new-line :- Context
  [context :- Context
   event :- Event
   config :- Config
   nrepl :- NReplClient]
  (undoable-text-event context event config nrepl t/new-line))

(s/def handlers :- {Action EventHandler}
  {e/inject            inject
   e/documentation     documentation
   e/signature         signature
   e/suggest           suggestion
   e/scroll-up         scroll-up
   e/scroll-down       scroll-down
   e/prev-eval         prev-eval
   e/next-eval         next-eval
   e/reformat          reformat
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
   eval-history :- EvalHistory]
  (let [history-size (eh/limit eval-history)
        text-history (th/create-text-history history-size)]
    {:status       processing
     :eval-history eval-history
     :undo-history text-history
     :redo-history text-history
     :information  i/empty-information
     :hud          hud}))
