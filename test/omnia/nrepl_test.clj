(ns omnia.nrepl-test
  (:require [clojure.test :refer [deftest is]]
            [schema.core :as s]
            [omnia.more :refer [=>]]
            [omnia.nrepl :as r]
            [omnia.test-utils :refer :all]
            [omnia.input :as i]))

(s/defn with-server :- s/Any
  [f :- (=> r/REPLClient s/Any)]
  (let [config {:host    "127.0.0.1"
                :port    11111
                :timeout 1000}
        server (r/start-server! config)
        client (r/client config)]
    (try
      (f client)
      (catch Exception e
        (.printStackTrace e))
      (finally
        (r/stop-server! server)))))

(defn completion [client]
  (let [expected (->> ["println" "println-str"] (mapv vec) (i/seeker))]
    (->> "println"
         (i/from-string)
         (r/complete! client)
         (equivalent expected)
         (is))))

(defn signature [client]
  (let [expected (->> ["clojure.core/println [& more]"]
                      (mapv vec)
                      (i/seeker))]
    (->> "println"
         (i/from-string)
         (r/signature! client)
         (equivalent expected)
         (is))))

(defn documentation [client])

(defn evaluation [client])

(deftest nrepl-test
  (with-server
    (fn [client]
      (completion client)
      (signature client))))