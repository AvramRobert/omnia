(ns omnia.core
  (:gen-class)
  (:require [omnia.terminal :as t]
            [omnia.nrepl :as n]
            [omnia.repl :as r]
            [omnia.config :as c]
            [clojure.string :as s]
            [halfling.task :as tsk])
  (:import (java.util Calendar)))

(defn config-path [dir] (format "%s/omnia.edn" dir))
(defn history-path [dir] (format "%s/.omnia.history" dir))
(def ^:const error-path ".omnia.error")

(def ^:const repl-ns 'user)
(def ^:const repl-host "127.0.0.1")
(defn rand-port [] (rand-int 65535))

(defn error-msg [{:keys [message trace]}]
  (format
    "Time: %s\nMessage: %s\n%s"
    (-> (Calendar/getInstance) (.getTime) (str))
    (or message "Unknown message")
    (->> trace
         (mapv #(str "   " (.toString %)))
         (s/join "\n"))))

(defn failure-msg [result]
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
  (tsk/task (spit error-path (error-msg result))))

(defn read-args! [args]
  (letfn [(read [re]
            (some-> (some #(when (s/starts-with? % re) %) args)
                    (s/split (re-pattern re))
                    (second)))]
    (tsk/task
      {:dir (or (read "path=") ".")})))

(defn hooks! [{:keys [repl]} {:keys [dir]}]
  (-> (tsk/task (n/write-history (history-path dir) repl))
      (tsk/recover (fn [_] ()))))

(defn succeed! [_]
  (System/exit 1))

(defn fail! [result]
  (-> (tsk/do-tasks
        [msg (failure-msg result)
         _   (log! result)
         _   (println msg)
         _   (Thread/sleep 3000)]
        (System/exit -1))
      (tsk/recover (fn [_] (System/exit -1)))
      (tsk/run)))

(defn -main [& args]
  (-> (tsk/do-tasks
        [argmap       (read-args! args)
         config       (-> argmap (:dir) (config-path) (c/read-config))
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
         _            (hooks! ctx argmap)
         _            (t/stop! terminal)
         _            (n/stop-server! server)])
      (tsk/recover fail!)
      (tsk/then succeed!)
      (tsk/run)))