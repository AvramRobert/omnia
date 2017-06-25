(ns omnia.core
  (require [lanterna.terminal :as t]
           [omnia.repl :as r]
           [omnia.hud :as h]
           [halfling.result :refer [fold]]
           [omnia.config :refer [get-local-config]]))

(defn shutdown [{:keys [terminal repl]}]
  (t/stop terminal)
  (r/stop repl)
  (System/exit 1))

(defn start [config]
  (let [internal-config {:terminal (t/get-terminal :text)
                         :repl     (r/repl {:kind :local})}]
    (t/start (:terminal internal-config))
    (h/read-eval-print (merge internal-config config))
    (shutdown internal-config)))

(defn -main [& args]
  (fold get-local-config
        #(start (:val %))
        #(println (format "Error: %s\n%s"
                          (-> % :val :cause)
                          (-> % :val :message)))))