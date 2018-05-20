"" (ns omnia.srepl
  (:require [clojure.tools.nrepl.transport :as t]
            [clojure.tools.nrepl.helpers :as h]
            [clojure.tools.nrepl.server :as s]
            [clojure.tools.nrepl.middleware :as m]
            [clojure.tools.nrepl :as n]
            [clojure.tools.nrepl.misc :refer [response-for]]
            [cider.nrepl :as c]
            [cider.nrepl.middleware.out :as out]
            [clojure.core.async :as a]
            [clojure.tools.nrepl :as nrepl]
            [clojure.tools.nrepl.middleware.interruptible-eval :as ieval]
            [clojure.tools.nrepl.middleware :as middleware]
            [clojure.tools.nrepl.transport :as transport]))

(def host "localhost")
(def port 1111)

(defn start-server []
  (s/start-server :bind host
                  :port port
                  :handler c/cider-nrepl-handler))

(defn run-code [code]
  (with-open [conn (n/connect :host host
                              :port port)]
    (-> (n/client conn 10000)
        (n/message {:op :eval
                    :code code})
        (clojure.pprint/pprint))))

(defn sender []
  (let [conn (n/connect :host host :port port)]
    (n/client conn 1000)))


(defn consume-out [client f]
  (future
    (loop [c client]
      (some-> (meta c)
              (:clojure.tools.nrepl/transport)
              (n/response-seq 100)
              (->> (filter #(or (:out %) (:err %))))
              (seq)
              (f))
      (recur c))))

(defn -main [& args]
  (start-server))

