(ns omnia.repl.core
  (:require [schema.core :as s]
            [halfling.task :as tsk]
            [omnia.display.terminal :as t]
            [omnia.display.render :as r]
            [omnia.repl.hud :as c]
            [omnia.repl.events :as e]
            [omnia.schema.event :refer [Event]]
            [omnia.schema.hud :refer [Hud]]
            [omnia.schema.nrepl :refer [NReplClient]]
            [omnia.schema.config :refer [Config]])
  (:import (omnia.display.terminal Terminal)))

(def prelude [(e/inject "(require '[omnia.repl.resolution :refer [retrieve retrieve-from]])")])

(s/defn consume :- Hud
  [hud :- Hud,
   config :- Config,
   terminal :- Terminal
   events :- [Event]]
  (let [step   (c/process hud config (first events))
        status (:status step)
        hud'   (:hud step)
        _      (r/render! hud' config terminal)]
    (case status
      :continue (recur hud' config terminal (rest events))
      :terminate hud')))

(s/defn events-from :- [Event]
  [terminal :- Terminal]
  (iterate (fn [_] (t/get-event! terminal)) e/ignore))

(s/defn read-eval-print
  [config :- Config
   terminal :- Terminal
   nrepl :- NReplClient]
  (let [events (concat prelude (events-from terminal))
        hud    (c/hud (t/size terminal) nrepl)]
    (-> (tsk/task (consume hud config terminal events))
        (tsk/then #(do (Thread/sleep 1200) %))
        (tsk/run))))