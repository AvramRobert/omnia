(ns omnia.core
  (require [lanterna.terminal :as t]
           [omnia.repl :as r]
           [omnia.hud :as h]
           [omnia.config :refer [local-config]]))

(defn shutdown [{:keys [terminal repl]}]
  (t/stop terminal)
  (r/stop repl)
  (System/exit 1))

(defn -main [& args]
  (let [internal-config {:terminal (t/get-terminal :text)
                         :repl     (r/repl {:kind :local})}]
    (t/start (:terminal internal-config))
    (h/read-eval-print (merge
                         internal-config
                         local-config))
    (shutdown internal-config)))