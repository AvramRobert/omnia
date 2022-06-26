(ns omnia.nrepl-test
  (:require [schema.core :as s]
            [omnia.repl.nrepl :as r]
            [omnia.test-utils :refer :all]
            [omnia.repl.text :as i]
            [clojure.test :refer [deftest is]]
            [clojure.string :refer [includes?]]
            [omnia.schema.nrepl :refer [NReplClient]]
            [omnia.schema.common :refer [=>]]))

(s/defn with-server :- s/Any
  [f :- (=> NReplClient s/Any)]
  (let [config {:host    "127.0.0.1"
                :port    11111}
        server (r/start-server! config)
        client (r/client config)]
    (try
      (f client)
      (catch Exception e
        (.printStackTrace e))
      (finally
          (r/stop-server! server)))))

(defn filled-completion [client]
  (let [expected (->> ["println"
                       "println-str"]
                      (derive-text))
        result (->> ["println"] (derive-text) (r/complete! client))]
    (is (= (:lines result) (:lines expected)))))

(defn empty-completion [client]
  (let [expected i/empty-text
        result   (->> ["nonesense-this"] (derive-text) (r/complete! client))]
    (is (= (:lines result) (:lines expected)))))

(defn completion [client]
  (filled-completion client)
  (empty-completion client))

(defn filled-signature [client]
  (let [expected (-> ["clojure.core/println [& more]"]
                     (derive-text))
        result   (->> ["println"] (derive-text) (r/signature! client))]
    (is (= (:lines result) (:lines expected)))))

(defn missing-signature [client]
  (let [expected i/empty-text
        _        (->> ["(def a 1)"] (derive-text) (r/evaluate! client))
        result   (->> ["a"] (derive-text) (r/signature! client))]
    (is (= (:lines result) (:lines expected)))))

(defn empty-signature [client]
  (let [expected i/empty-text
        result   (->> ["a"] (derive-text) (r/signature! client))]
    (is (= (:lines result) (:lines expected)))))

(defn signature [client]
  (filled-signature client)
  (missing-signature client)
  (empty-signature client))

(defn filled-documentation [client]
  (let [expected (->> ["Same as print followed by (newline)"]
                      (derive-text))
        result   (->> ["println"] (derive-text) (r/docs! client))]
    (is (= (:lines result) (:lines expected)))))

(defn missing-docs [client]
  (let [expected i/empty-text
        _        (->> ["(defn bla [x] x)"] (derive-text) (r/evaluate! client))
        result   (->> ["bla"] (derive-text) (r/docs! client))]
    (is (= (:lines result) (:lines expected)))))

(defn empty-documentation [client]
  (let [expected i/empty-text
        result (->> ["nonesense-this"] (derive-text) (r/docs! client))]
    (is (= (:lines result) (:lines expected)))))

(defn documentation [client]
  (filled-documentation client)
  (missing-docs client)
  (empty-documentation client))

(defn value-evaluation [client]
  (let [result (->> ["(+ 1 1)"]
                    (derive-text)
                    (r/evaluate! client))
        expected (-> ["" "2" ""]
                     (derive-text))]
    (is (= (:lines expected) (:lines result)))))

(defn out-evaluation [client]
  (let [result   (->> ["(println (+ 1 1))"]
                      (derive-text)
                      (r/evaluate! client))
        expected (-> ["2" "" "nil" ""]
                     (derive-text))]
    (is (= (:lines result) (:lines expected)))))

(defn exception-evaluation [client]
  (let [exception  (str '(throw (IllegalArgumentException. "bla")))
        evaluation (->> [exception]
                        (derive-text)
                        (r/evaluate! client)
                        (i/as-string))]
    (is (includes? evaluation "bla"))
    (is (includes? evaluation "IllegalArgumentException"))))

(defn empty-evaluation [client]
  (let [expected (-> [""
                      ""]
                     (derive-text))
        result (->> [""]
                    (derive-text)
                    (r/evaluate! client))]
    (is (= (:lines result) (:lines expected)))))

(defn evaluation [client]
  (value-evaluation client)
  (out-evaluation client)
  (exception-evaluation client)
  (empty-evaluation client))

(deftest nrepl-test
  (with-server
    (fn [client]
      (completion client)
      (signature client)
      (documentation client)
      (evaluation client))))
