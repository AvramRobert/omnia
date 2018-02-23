(ns omnia.core
  (:gen-class)
  (require [omnia.terminal :as t]
           [omnia.repl :as r]
           [omnia.hud :as h]
           [omnia.config :as c]
           [clojure.string :as s]
           [halfling.task :as tsk])
  (:import (java.util Calendar)))

(defn config-path [dir] (format "%s/omnia.edn" dir))
(defn history-path [dir] (format "%s/.omnia.history" dir))
(def ^:const error-path ".omnia.error")
(def ^:const repl-ns 'user)

(defn error [{:keys [message trace]}]
  (format
    "Time: %s\nMessage: %s\n%s"
    (-> (Calendar/getInstance) (.getTime) (str))
    (or message "Unknown message")
    (->> trace
         (mapv #(str "   " (.toString %)))
         (s/join "\n"))))

(defn failure [result]
  (->>
    [""
     "-----"
     "I don't have the heart to tell you.. but something went wrong internally"
     (format "Take a look at ~/%s for a complete trace of the error" error-path)
     (format "Message - %s" (:message result))
     "-----"
     ""]
    (s/join "\n")))

(defn log! [result]
  (tsk/task (spit error-path (error result))))

(defn read-args! [args]
  (letfn [(read [re]
            (some-> (some #(when (s/starts-with? % re) %) args)
                    (s/split (re-pattern re))
                    (second)))]
    (tsk/task
      {:dir (or (read "path=") ".")})))

(defn hooks! [{:keys [repl]} {:keys [dir]}]
  (-> (tsk/task (r/write-history (history-path dir) repl))
      (tsk/recover (fn [_] ()))))

(defn shutdown! [{:keys [terminal repl]}]
  (tsk/task
    (t/stop! terminal)
    (r/stop! repl)))

(defn start! [{:keys [dir]}]
  (tsk/do-tasks
    [config   (c/read-config (config-path dir))
     history  (r/read-history (history-path dir))
     terminal (t/terminal :text)
     repl     (r/repl {:kind    :local
                       :history history
                       :ns      repl-ns})
     _        (t/start! terminal)]
    (h/read-eval-print
      (assoc config :terminal terminal :repl repl))))

(defn succeed! [xs]
  (System/exit 1))

(defn fail! [result]
  (->
    (tsk/do-tasks
      [msg (failure result)
       _   (log! result)
       _   (println msg)
       _   (Thread/sleep 3000)]
      (System/exit -1))
    (tsk/recover (fn [_] (System/exit -1)))
    (tsk/run)))

(defn -main [& args]
  (-> (tsk/do-tasks
        [argmap (read-args! args)
         ctx    (start! argmap)
         _      (hooks! ctx argmap)
         _      (shutdown! ctx)])
      (tsk/recover fail!)
      (tsk/then succeed!)
      (tsk/run)))