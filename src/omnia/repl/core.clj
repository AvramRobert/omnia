(ns omnia.repl.core
  (:require [schema.core :as s]
            [halfling.task :as tsk]
            [omnia.repl.context :as c]
            [omnia.repl.hud :as h]
            [omnia.display.terminal :as t]
            [omnia.display.render :as r]
            [omnia.repl.events :as e]
            [omnia.schema.context :refer [Context processing terminated]]
            [omnia.schema.event :refer [Event]]
            [omnia.schema.hud :refer [Hud]]
            [omnia.schema.nrepl :refer [NReplClient]]
            [omnia.schema.config :refer [Config]])
  (:import (omnia.display.terminal Terminal)))

(def prelude [(e/inject "(require '[omnia.repl.resolution :refer [retrieve retrieve-from]])")])

(s/defn consume :- Context
  [context  :- Context,
   config   :- Config,
   terminal :- Terminal
   events   :- [Event]]
  (let [context' (c/process context (first events) config)
        status   (:status context')
        _        (r/render! context' config terminal)]
    (if (= status processing)
      (recur context' config terminal (rest events))
      context')))

(s/defn events-from :- [Event]
  [terminal :- Terminal]
  (iterate (fn [_] (t/get-event! terminal)) e/ignore))

(s/defn read-eval-print
  [config   :- Config
   terminal :- Terminal
   nrepl    :- NReplClient]
  (let [events  (concat prelude (events-from terminal))
        context (c/create (t/size terminal) nrepl)]
    (-> (tsk/task (consume context config terminal events))
        (tsk/then #(do (Thread/sleep 1200) %))
        (tsk/run))))