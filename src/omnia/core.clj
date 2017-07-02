(ns omnia.core
  (require [lanterna.terminal :as t]
           [omnia.repl :as r]
           [omnia.hud :as h]
           [halfling.result :refer [fold]]
           [omnia.config :refer [read-config]]
           [halfling.task :as tsk]))

(def ^:const dir (System/getProperty "user.dir"))
(def ^:const config (format "%s/omnia.edn" dir))
(def ^:const history (format "%s/.omnia.history" dir))

(defn hooks [conf]
  conf)

(defn shutdown [{:keys [terminal repl]}]
  (t/stop terminal)
  (r/stop repl)
  (System/exit 1))

(defn start [config]
  (let [internal-config {:terminal (t/get-terminal :text)
                         :repl     (r/repl {:kind :local})}]
    (t/start (:terminal internal-config))
    (h/read-eval-print (merge internal-config config))))

(defn -main [& _]
  (-> (read-config config)
      (tsk/then start)
      (tsk/then hooks)
      (tsk/then shutdown)
      (tsk/recover #(println
                      (format "Error: %s\n%s"
                              (:cause %)
                              (:message %))))
      (tsk/run)))