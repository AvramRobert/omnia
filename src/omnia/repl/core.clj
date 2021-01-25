(ns omnia.repl.core
  (:require [schema.core :as s]
            [halfling.task :as tsk]
            [omnia.view.terminal :as t]
            [omnia.view.render :as r]
            [omnia.repl.context :as c]
            [omnia.config.components.event :as e]
            [omnia.config.components.event :refer [Event]]
            [omnia.view.terminal :refer [Terminal]]
            [omnia.repl.context :refer [Context]]
            [omnia.repl.nrepl :refer [REPLClient]]
            [omnia.config.core :refer [Config]]))

(def prelude [(e/inject-event "(require '[omnia.resolution :refer [retrieve retrieve-from]])")])

(s/defn consume :- Context
        [ctx    :- Context,
         events :- [Event]]
   (let [step   (c/process ctx (first events))
         status (:status step)
         ctx'   (:ctx step)
         _      (r/render! ctx')]
     (case status
       :continue (recur ctx' (rest events))
       :terminate ctx')))

(s/defn events-from :- [Event]
   [terminal :- Terminal]
     (iterate (fn [_] (t/get-event! terminal)) e/ignore))

(s/defn read-eval-print
  [config   :-  Config
   terminal :-  Terminal
   repl     :-  REPLClient]
  (let [events          (concat prelude (events-from terminal))
        initial-context (c/context config terminal repl)]
    (-> (tsk/task (consume initial-context events))
        (tsk/then #(do (Thread/sleep 1200) %))
        (tsk/run))))