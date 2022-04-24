(ns omnia.repl.core
  (:require [schema.core :as s]
            [halfling.task :as tsk]
            [omnia.view.terminal :as t]
            [omnia.view.render :as r]
            [omnia.repl.context :as c]
            [omnia.repl.events :as e]
            [omnia.schema.event :refer [Event]]
            [omnia.view.terminal :refer [Terminal]]
            [omnia.schema.context :refer [Context]]
            [omnia.schema.nrepl :refer [NReplClient]]
            [omnia.schema.config :refer [Config]]))

(def prelude [(e/inject "(require '[omnia.repl.resolution :refer [retrieve retrieve-from]])")])

(s/defn consume :- Context
        [ctx      :- Context,
         config   :- Config,
         terminal :- Terminal
         events   :- [Event]]
   (let [step   (c/process ctx config (first events))
         status (:status step)
         ctx'   (:context step)
         _      (r/render! ctx' config terminal)]
     (case status
       :continue (recur ctx' config terminal (rest events))
       :terminate ctx')))

(s/defn events-from :- [Event]
   [terminal :- Terminal]
     (iterate (fn [_] (t/get-event! terminal)) e/ignore))

(s/defn read-eval-print
  [config   :- Config
   terminal :- Terminal
   nrepl    :- NReplClient]
  (let [events  (concat prelude (events-from terminal))
        context (c/context (t/size terminal) nrepl)]
    (-> (tsk/task (consume context config terminal events))
        (tsk/then #(do (Thread/sleep 1200) %))
        (tsk/run))))