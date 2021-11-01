(ns omnia.nrepl-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :refer [includes?]]
            [omnia.util.schema :refer [=>]]
            [schema.core :as s]
            [omnia.repl.nrepl :as r]
            [omnia.test-utils :refer :all]
            [omnia.text.core :as i]))

(s/defn with-server :- s/Any
  [f :- (=> r/REPLClient s/Any)]
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
  (let [expected (i/from-string "println\nprintln-str")]
    (->> "println"
         (i/from-string)
         (r/complete! client)
         (r/result)
         (i/equivalent? expected)
         (is))))

(defn empty-completion [client]
  (let [expected i/empty-seeker]
    (->> "nonesense-this"
         (i/from-string)
         (r/complete! client)
         (r/result)
         (i/equivalent? expected)
         (is))))

(defn completion [client]
  (filled-completion client)
  (empty-completion client))

(defn filled-signature [client]
  (let [expected (i/from-string "clojure.core/println [& more]")]
    (->> "println"
         (i/from-string)
         (r/signature! client)
         (r/result)
         (i/equivalent? expected)
         (is))))

(defn missing-signature [client]
  (let [expected i/empty-seeker
        client'  (->> "(def a 1)" (i/from-string) (r/evaluate! client))]
    (->> "a"
         (i/from-string)
         (r/signature! client')
         (r/result)
         (i/equivalent? expected)
         (is))))

(defn empty-signature [client]
  (let [expected i/empty-seeker]
    (->> "nonesense-this"
         (i/from-string)
         (r/signature! client)
         (r/result)
         (i/equivalent? expected)
         (is))))

(defn signature [client]
  (filled-signature client)
  (missing-signature client)
  (empty-signature client))

(defn filled-documentation [client]
  (let [expected (i/from-string "Same as print followed by (newline)")]
    (->> "println"
         (i/from-string)
         (r/docs! client)
         (r/result)
         (i/equivalent? expected)
         (is))))

(defn missing-docs [client]
  (let [expected i/empty-seeker
        client'  (->> "(defn bla [x] x)" (i/from-string) (r/evaluate! client))]
    (->> "bla"
         (i/from-string)
         (r/docs! client')
         (r/result)
         (i/equivalent? expected)
         (is))))

(defn empty-documentation [client]
  (let [expected i/empty-seeker]
    (->> "nonesense-this"
         (i/from-string)
         (r/docs! client)
         (r/result)
         (i/equivalent? expected)
         (is))))

(defn documentation [client]
  (filled-documentation client)
  (missing-docs client)
  (empty-documentation client))

(defn value-evaluation [client]
  (let [expected (i/from-string "2\n\n")]
    (->> "(+ 1 1)"
         (i/from-string)
         (r/evaluate! client)
         (r/result)
         (i/equivalent? expected)
         (is))))

(defn out-evaluation [client]
  (let [expected (i/from-string "2\nnil\n\n")]
    (->> "(println (+ 1 1))"
         (i/from-string)
         (r/evaluate! client)
         (r/result)
         (i/equivalent? expected)
         (is))))

(defn exception-evaluation [client]
  (let [expected   "Exception bla"
        evaluation (->> '(throw (Exception. "bla"))
                        (str)
                        (i/from-string)
                        (r/evaluate! client)
                        (r/result)
                        (i/stringify))]
    (-> evaluation (includes? expected) (is))))

(defn empty-evaluation [client]
  (let [expected i/empty-line]
    (->> ""
         (i/from-string)
         (r/evaluate! client)
         (r/result)
         (i/equivalent? expected)
         (is))))

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