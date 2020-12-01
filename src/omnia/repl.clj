(ns omnia.repl
  (:require [schema.core :as s]
            [omnia.terminal :as t]
            [omnia.context :as c]
            [omnia.render :as r]
            [omnia.event :as e]
            [halfling.task :as tsk]
            [omnia.event :refer [Event]]
            [omnia.terminal :refer [Terminal]]
            [omnia.context :refer [Context]]
            [omnia.nrepl :refer [REPLClient]]
            [omnia.config :refer [InternalConfig]]))

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
  [config   :-  InternalConfig,
   terminal :- Terminal,
   repl     :- REPLClient]
  (let [events          (concat prelude (events-from terminal))
        initial-context (c/context config terminal repl)]
    (-> (tsk/task (consume initial-context events))
        (tsk/then #(do (Thread/sleep 1200) %))
        (tsk/run))))