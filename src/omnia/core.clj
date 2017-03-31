(ns omnia.core
  (:gen-class)
  (require [lanterna.terminal :as t]
           [omnia.hud :as hud]))

(defn start-terminal                                        ;; I don't really like this
  ([kind]
   (start-terminal kind nil nil :identity))
  ([kind port]
   (start-terminal kind "localhost" port))
  ([kind host port]
   (start-terminal kind host port :nrepl))
  ([kind host port repl-type]
   (let [terminal (t/get-terminal :text)
         _ (t/start terminal)]
     (hud/read-eval-print terminal host port))))

(defn -main [& args]
  (start-terminal :text))
