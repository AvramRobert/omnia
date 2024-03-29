(ns omnia.core
  (:gen-class)
  (:require [schema.core :as s]
            [clojure.string :as string]
            [halfling.task :as tsk]
            [omnia.display.terminal :as t]
            [omnia.repl.nrepl :as n]
            [omnia.repl.eval-history :as eh]
            [omnia.repl.context :as ct]
            [omnia.repl.core :as r]
            [omnia.config.core :as c]
            [omnia.config.defaults :as d]
            [omnia.util.misc :as m]
            [omnia.schema.config :refer [Config]]
            [omnia.schema.context :refer [Context]]
            [omnia.schema.main :refer [ThrowableMap AppArgs app-path-arg]])
  (:import (java.util Calendar)
           (halfling.task Task)))

(s/defn error-msg :- s/Str
  [{:keys [cause trace]} :- ThrowableMap]
  (format
    "Time: %s\nMessage: %s\n%s"
    (-> (Calendar/getInstance) (.getTime) (str))
    (or cause "Unknown cause")
    (->> trace
         (mapv #(str "   " (.toString %)))
         (string/join "\n"))))

(s/defn failure-msg :- s/Str
  [result :- ThrowableMap]
  (->>
    [""
     "-----"
     "I don't have the heart to tell you.. but something went wrong"
     (format "Take a look at ~/%s for a complete trace of the error" d/default-error-file-name)
     (format "Message - %s" (:cause result))
     "-----"
     ""]
    (string/join "\n")))

(s/defn write-error! :- Task
  [result :- ThrowableMap]
  (->> result
       (error-msg)
       (spit d/default-error-file-name)
       (tsk/task)))

(s/defn hooks! :- Task
  [context :- Context
   config  :- Config]
  (-> context
      (ct/eval-history)
      (eh/write-eval-history config)
      (tsk/task)))

(s/defn read-args :- AppArgs
  [args :- [s/Str]]
  {:app-path (-> app-path-arg (m/read-arg args) (or "."))})

(defn succeed! [_]
  (System/exit 1))

(defn fail! [result]
  (-> (tsk/do-tasks
        [msg (failure-msg result)
         _   (write-error! result)
         _   (println msg)
         _   (Thread/sleep 3000)]
        (System/exit -1))
      (tsk/recover (fn [_] (System/exit -1)))
      (tsk/run)))

(defn -main
  [& args]
  (-> (tsk/do-tasks
        [args         (read-args args)
         config       (c/read-config! (:app-path args))
         eval-history (eh/read-eval-history config)
         terminal     (t/create-terminal config)
         repl-config  {:host d/default-repl-host
                       :port (m/rand-port)
                       :ns   d/default-repl-namespace}
         server       (n/start-server! repl-config)
         repl         (n/client repl-config)
         _            (t/start! terminal)
         context      (r/read-eval-print config terminal repl eval-history)
         _            (hooks! context config)
         _            (t/stop! terminal)
         _            (n/stop-server! server)])
      (tsk/recover fail!)
      (tsk/then succeed!)
      (tsk/run)))
