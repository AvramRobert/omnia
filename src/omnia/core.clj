(ns omnia.core
  (require [lanterna.terminal :as t]
           [omnia.repl :as r]
           [omnia.hud :as h]
           [omnia.config :as c]
           [halfling.task :as tsk]))

(def ^:const dir (System/getProperty "user.dir"))
(def ^:const config-path (format "%s/omnia.edn" dir))
(def ^:const history-path (format "%s/.omnia.history" dir))
(def ^:const error-path (format "%s/.omnia.error" dir))

(defn error [{:keys [message cause trace]}]
  (format
    "java.lang.Exception: %s\nCause: %s\n%s"
    (or message "Unknown message")
    (or cause "Unknown cause")
    (->> trace
         (mapv #(str "   " (.toString %)))
         (clojure.string/join "\n"))))

(defn failure [result]
  (println "I don't have the heart to tell you.. but something went wrong internally")
  (println (format "Take a look at %s for a complete trace of the error" error-path))
  (spit error-path (error result))
  (Thread/sleep 1200)
  (System/exit -1))

(defn hooks [{:keys [repl]}]
  (r/write-history history-path repl))

(defn shutdown [{:keys [terminal repl]}]
  (t/stop terminal)
  (r/stop repl)
  (System/exit 1))

(def start
  (tsk/do-tasks
    [config (c/read-config config-path)
     history (r/read-history history-path)
     terminal (t/get-terminal :text)
     repl (r/repl {:kind    :local
                   :history history})
     _ (t/start terminal)]
    (h/read-eval-print
      (assoc config :terminal terminal :repl repl))))

(defn -main [& _]
  (-> (tsk/do-tasks
        [ctx start
         _ (hooks ctx)
         _ (shutdown ctx)])
      (tsk/recover failure)
      (tsk/run)))