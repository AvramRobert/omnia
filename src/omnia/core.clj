(ns omnia.core
  (:gen-class)
  (:require [schema.core :as s]
            [halfling.task :as tsk]
            [omnia.display.terminal :as t]
            [omnia.repl.nrepl :as n]
            [omnia.repl.core :as r]
            [omnia.repl.context :as ct]
            [omnia.repl.hud :as h]
            [omnia.config.core :as c]
            [omnia.schema.context :refer [Context]]
            [omnia.schema.main :refer [ThrowableMap ArgMap]]
            [clojure.string :refer [join split starts-with?]])
  (:import (java.util Calendar)
           (halfling.task Task)))

(def ^:const error-path ".omnia.error")
(def ^:const repl-ns 'user)
(def ^:const repl-host "127.0.0.1")

(s/defn config-path :- s/Str
  [dir :- s/Str]
  (format "%s/omnia.edn" dir))

(s/defn history-path :- s/Str
  [dir :- s/Str]
  (format "%s/.omnia.history" dir))

(s/defn rand-port :- s/Int
  []
  (rand-int 65535))

(s/defn error-msg :- s/Str
  [{:keys [cause trace]} :- ThrowableMap]
  (format
    "Time: %s\nMessage: %s\n%s"
    (-> (Calendar/getInstance) (.getTime) (str))
    (or cause "Unknown cause")
    (->> trace
         (mapv #(str "   " (.toString %)))
         (join "\n"))))

(s/defn failure-msg :- s/Str
  [result :- ThrowableMap]
  (->>
    [""
     "-----"
     "I don't have the heart to tell you.. but something went wrong"
     (format "Take a look at ~/%s for a complete trace of the error" error-path)
     (format "Message - %s" (:cause result))
     "-----"
     ""]
    (join "\n")))

(s/defn log-error! :- Task
  [result :- ThrowableMap]
  (tsk/task (spit error-path (error-msg result))))

(s/defn read-args! :- ArgMap
  [args :- [s/Str]]
  (letfn [(read [re]
            (some-> (some #(when (starts-with? % re) %) args)
                    (split (re-pattern re))
                    (second)))]
    {:dir (or (read "path=") ".")}))

(s/defn hooks! :- Task
  [context :- Context
   argmap :- ArgMap]
  (let [history-dir (-> argmap (:dir) (history-path))
        repl        (-> context (ct/hud) (h/nrepl-client))]
    (-> (n/write-history history-dir repl)
        (tsk/task)
        (tsk/recover (fn [_] ())))))

(defn succeed! [_]
  (System/exit 1))

(defn fail! [result]
  (-> (tsk/do-tasks
        [msg (failure-msg result)
         _   (log-error! result)
         _   (println msg)
         _   (Thread/sleep 3000)]
        (System/exit -1))
      (tsk/recover (fn [_] (System/exit -1)))
      (tsk/run)))

(defn -main [& args]
  (-> (tsk/do-tasks
        [argmap       (read-args! args)
         config       (-> argmap (:dir) (config-path) (c/read-config!))
         history      (-> argmap (:dir) (history-path) (n/read-history))
         terminal     (t/terminal config)
         repl-config  {:history history
                       :host    repl-host
                       :port    (rand-port)
                       :ns      repl-ns}
         server       (n/start-server! repl-config)
         repl         (n/client repl-config)
         _            (t/start! terminal)
         ctx          (r/read-eval-print config terminal repl)
         _            (hooks! (:hud ctx) argmap)
         _            (t/stop! terminal)
         _            (n/stop-server! server)])
      (tsk/recover fail!)
      (tsk/then succeed!)
      (tsk/run)))
