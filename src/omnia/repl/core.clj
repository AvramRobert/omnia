(ns omnia.repl.core
  (:require [schema.core :as s]
            [halfling.task :as tsk]
            [omnia.repl.context :as c]
            [omnia.display.terminal :as t]
            [omnia.display.render :as r]
            [omnia.repl.events :as e]
            [omnia.schema.context :refer [Context processing]]
            [omnia.schema.event :refer [Event]]
            [omnia.schema.nrepl :refer [NReplClient]]
            [omnia.schema.config :refer [Config]]
            [omnia.schema.terminal :refer [Terminal]]
            [omnia.schema.eval-history :refer [EvalHistory]]
            [omnia.repl.hud :as h]))

(def prelude [(e/inject "(require '[omnia.repl.resolution :refer [retrieve retrieve-from]])") e/clear])

(s/defn consume :- Context
  [context  :- Context,
   config   :- Config,
   terminal :- Terminal,
   nrepl    :- NReplClient,
   events   :- [Event]]
  (let [context' (c/process context (first events) config nrepl)
        status   (:status context')
        _        (r/render! context' config terminal)]
    (if (= status processing)
      (recur context' config terminal nrepl (rest events))
      context')))

(s/defn events-from :- [Event]
  [terminal :- Terminal]
  (iterate (fn [_] (t/get-event! terminal)) e/ignore))

(s/defn read-eval-print
  [config :- Config
   terminal :- Terminal
   nrepl :- NReplClient
   eval-history :- EvalHistory]
  (let [events  (concat prelude (events-from terminal))
        hud     (h/create-hud (t/size terminal))
        context (c/context-from hud eval-history)]
    (-> (tsk/task (consume context config terminal nrepl events))
        (tsk/then #(do (Thread/sleep 1200) %))
        (tsk/run))))
