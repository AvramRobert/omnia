(ns omnia.repl.context
  (:require [schema.core :as s]
            [omnia.repl.hud :as h]
            [omnia.repl.history :as hi]
            [omnia.repl.text :as i]
            [omnia.schema.event :as e]
            [omnia.schema.text :refer [Text]]
            [omnia.schema.hud :refer [Hud]]
            [omnia.schema.context :refer [Context EventHandler processing terminated]]
            [omnia.schema.config :refer [Config]]
            [omnia.schema.event :refer [Event Action]]
            [omnia.schema.common :refer [=>]]
            [omnia.schema.nrepl :refer [NReplClient]]))

(s/defn hud :- Hud
  [context :- Context]
  (:hud context))

(s/defn continue :- Context
  [hud :- Hud
   context :- Context]
  {:status  processing
   :hud     hud
   :history (:history context)})

(s/defn terminate :- Context
  [hud :- Hud
   context :- Context]
  {:status  terminated
   :hud     hud
   :history (:history context)})

(s/defn inject :- Context
  [context :- Context
   event   :- Event
   config  :- Config]
  (-> context
      (hud)
      (h/inject event)
      (h/diff-render)
      (continue context)))

(s/defn docs :- Context
  [context :- Context
   event :- Event
   config :- Config]
  (-> context
      (hud)
      (h/gc config)
      (h/reset-scroll)
      (h/reset-suggestions)
      (h/reset-signatures)
      (h/deselect)
      (h/document)
      (h/match-parens config)
      (h/diff-render)
      (continue context)))

(s/defn signature :- Context
  [context :- Context
   event :- Event
   config :- Config]
  (-> context
      (hud)
      (h/gc config)
      (h/reset-scroll)
      (h/reset-suggestions)
      (h/reset-documentation)
      (h/deselect)
      (h/signature)
      (h/match-parens config)
      (h/diff-render)
      (continue context)))

(s/defn suggest :- Context
  [context :- Context
   event :- Event
   config :- Config]
  (-> context (hud)
      (h/gc config)
      (h/reset-scroll)
      (h/reset-documentation)
      (h/reset-signatures)
      (h/suggest)
      (h/match-parens config)
      (h/diff-render)
      (continue context)))

(s/defn scroll-down :- Context
  [context :- Context
   event   :- Event
   config  :- Config]
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
   event   :- Event
   config  :- Config]
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
   event   :- Event
   config  :- Config]
  (-> context
      (hud)
      (h/gc config)
      (h/reset-scroll)
      (h/reset-suggestions)
      (h/reset-documentation)
      (h/reset-signatures)
      (h/prev-eval)
      (h/highlight config)
      (h/match-parens config)
      (h/diff-render)
      (continue context)))

(s/defn next-eval :- Context
  [context :- Context
   event   :- Event
   config  :- Config]
  (-> context
      (hud)
      (h/gc config)
      (h/reset-scroll)
      (h/reset-suggestions)
      (h/reset-documentation)
      (h/reset-signatures)
      (h/next-eval)
      (h/highlight config)
      (h/match-parens config)
      (h/diff-render)
      (continue context)))

;; FIXME: Rename this to: reformat
(s/defn indent :- Context
  [context :- Context
   event   :- Event
   config  :- Config]
  (-> context
      (hud)
      (h/gc config)
      (h/reset-scroll)
      (h/reset-suggestions)
      (h/reset-documentation)
      (h/reset-signatures)
      (h/deselect)
      (h/reformat)
      (h/highlight config)
      (h/match-parens config)
      (h/diff-render)
      (continue context)))

(s/defn clear :- Context
  [context :- Context
   event   :- Event
   config  :- Config]
  (-> context
      (hud)
      (h/gc config)
      (h/reset-scroll)
      (h/reset-suggestions)
      (h/reset-documentation)
      (h/reset-signatures)
      (h/deselect)
      (h/clear)
      (h/highlight config)
      (h/match-parens config)
      (h/clear-render)
      (continue context)))

(s/defn evaluate :- Context
  [context :- Context
   event   :- Event
   config  :- Config]
  (-> context
      (hud)
      (h/gc config)
      (h/reset-scroll)
      (h/reset-suggestions)
      (h/reset-documentation)
      (h/reset-signatures)
      (h/evaluate)
      (h/highlight config)
      (h/diff-render)
      (continue context)))

(s/defn exit :- Context
  [context :- Context
   event   :- Event
   config  :- Config]
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
   event   :- Event
   config  :- Config]
  (-> context
      (hud)
      (h/resize event)
      (h/calibrate)
      (h/re-render)
      (continue context)))

(s/defn ignore :- Context
  [context :- Context
   event   :- Event
   config  :- Config]
  (-> context (hud) (continue context)))

(s/defn text-event :- Context
  [context :- Context
   event   :- Event
   config  :- Config
   f       :- (=> Text Text)]
  (-> context
      (hud)
      (h/gc config)
      (h/reset-scroll)
      (h/reset-suggestions)
      (h/reset-documentation)
      (h/reset-signatures)
      (h/input f)
      (h/calibrate)
      (h/highlight config)
      (h/match-parens config)
      (h/diff-render)
      (continue context)))

(s/defn character :- Context
  [context :- Context
   event :- Event
   config :- Config]
  (text-event context event config #(i/insert % (:value event))))

(s/defn move-up :- Context
  [context :- Context
   event :- Event
   config :- Config]
  (text-event context event config i/move-up))

(s/defn move-down :- Context
  [context :- Context
   event :- Event
   config :- Config]
  (text-event context event config i/move-down))

(s/defn move-left :- Context
  [context :- Context
   event   :- Event
   config  :- Config]
  (text-event context event config i/move-left))

(s/defn move-right :- Context
  [context :- Context
   event   :- Event
   config  :- Config]
  (text-event context event config i/move-right))

(s/defn jump-left :- Context
  [context :- Context
   event   :- Event
   config  :- Config]
  (text-event context event config i/jump-left))

(s/defn jump-right :- Context
  [context :- Context
   event   :- Event
   config  :- Config]
  (text-event context event config i/jump-right))

(s/defn select-all :- Context
  [context :- Context
   event   :- Event
   config  :- Config]
  (text-event context event config i/select-all))

(s/defn select-up :- Context
  [context :- Context
   event   :- Event
   config  :- Config]
  (text-event context event config i/select-up))

(s/defn select-down :- Context
  [context :- Context
   event   :- Event
   config  :- Config]
  (text-event context event config i/select-down))

(s/defn select-right :- Context
  [context :- Context
   event   :- Event
   config  :- Config]
  (text-event context event config i/select-right))

(s/defn select-left :- Context
  [context :- Context
   event   :- Event
   config  :- Config]
  (text-event context event config i/select-left))

(s/defn jump-select-left :- Context
  [context :- Context
   event   :- Event
   config  :- Config]
  (text-event context event config i/jump-select-left))

(s/defn jump-select-right :- Context
  [context :- Context
   event   :- Event
   config  :- Config]
  (text-event context event config i/jump-select-right))

(s/defn expand-selection :- Context
  [context :- Context
   event   :- Event
   config  :- Config]
  (text-event context event config i/expand-selection))

(s/defn copy :- Context
  [context :- Context
   event   :- Event
   config  :- Config]
  (text-event context event config i/copy))

(s/defn cut :- Context
  [context :- Context
   event   :- Event
   config  :- Config]
  (text-event context event config i/cut))

(s/defn paste :- Context
  [context :- Context
   event   :- Event
   config  :- Config]
  (text-event context event config i/paste))

(s/defn delete-previous :- Context
  [context :- Context
   event   :- Event
   config  :- Config]
  (text-event context event config i/delete-previous))

(s/defn delete-current :- Context
  [context :- Context
   event   :- Event
   config  :- Config]
  (text-event context event config i/delete-current))

(s/defn new-line :- Context
  [context :- Context
   event   :- Event
   config  :- Config]
  (text-event context event config i/new-line))

(s/defn undo :- Context
  [context :- Context
   event   :- Event
   config  :- Config]
  (text-event context event config i/undo))

(s/defn redo :- Context
  [context :- Context
   event   :- Event
   config  :- Config]
  (text-event context event config i/redo))

(s/def handlers :- {Action EventHandler}
  {e/inject            inject
   e/docs              docs
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
   e/new-line          newline
   e/copy              copy
   e/cut               cut
   e/paste             paste
   e/undo              undo
   e/redo              redo
   e/character         character
   e/expand-selection  expand-selection})

(s/defn context-from :- Context
  [hud :- Hud]
  {:status  processing
   :history (hi/create-history 50)
   :hud     hud})

(s/defn create-context :- Context
  [view-size :- s/Int
   repl-client :- NReplClient]
  {:status  processing
   :history (hi/create-history 50)
   :hud     (h/create-hud view-size repl-client)})

(s/defn process :- Context
  [context :- Context
   event   :- Event
   config  :- Config]
  (let [action  (:action event)
        handler (get handlers action ignore)]
    (handler context event config)))
