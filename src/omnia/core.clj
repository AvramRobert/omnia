(ns omnia.core
  (:gen-class)
  (require [lanterna.terminal :as t]
           [omnia.repl :as r]
           [omnia.hud :as h]
           [clojure.core.match :as m]))

(defn shutdown [terminal repl]
  (t/stop terminal)
  (r/stop repl)
  (System/exit 1))

(defn -main [& args]
    (let [terminal (t/get-terminal :text)
          repl (r/repl {:kind :local})]
      (t/start terminal)
      (h/read-eval-print terminal repl)
      (shutdown terminal repl)))