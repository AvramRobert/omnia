(ns omnia.core
  (:gen-class)
  (require [lanterna.terminal :as t]
           [omnia.repl :as r]
           [omnia.hud :as h]))

(defn shutdown [terminal]
  (t/stop terminal)
  (System/exit 1))

(defn startup                                               ;; I don't really like this
  ([kind]
   (startup kind nil nil :identity))
  ([kind port]
   (startup kind "localhost" port))
  ([kind host port]
   (startup kind host port :nrepl))
  ([kind host port repl-type]
   (let [terminal (t/get-terminal kind)
         repl (r/repl host port repl-type)]
     (t/start terminal)
     (h/read-eval-print terminal repl)
     (shutdown terminal))))

(defn -main [& args]
  (startup :text 46501))
