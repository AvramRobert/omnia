(ns omnia.srepl
  (:require [clojure.tools.nrepl.misc :refer [response-for]]
            [clojure.tools.nrepl.transport :as t]
            [clojure.tools.nrepl.middleware :as m]
            [clojure.tools.nrepl.server :as s]
            [clojure.tools.nrepl :as repl]
            [cider.nrepl :as n]
            [clojure.core.match :refer [match]]
            [clojure.tools.nrepl :as nrepl]
            [clojure.tools.nrepl.transport :as transport]))

(def port 1111)
(def bind "localhost")

(defn random-uuid [] (str (java.util.UUID/randomUUID)))

(defn start-server []
  (s/start-server :bind bind
                  :port port
                  :handler n/cider-nrepl-handler))

(def idle {:type :idle})
(defn executing [id] {:type :executing :id id})

(defn gather [t responses]
  (loop [messages []]
    (when (not-empty messages)
      (dosync (alter responses #(concat % messages))))
    (recur (nrepl/response-seq t 100))))

(defn read-out! [responses op]
  (letfn [(consume-only [msg-id output]
            (let [id? #(= msg-id (:id %))
                  res  (filter id? @output)
                  _    (dosync (alter output #(filter (comp not id?) %)))]
              res))
          (consume [output]
            (let [res @output
                  _   (dosync (ref-set output []))]
              res))]
    (let [{type   :type
           msg-id :id} @op]
      (if (= :execute type)
        (consume-only msg-id responses)
        (consume responses)))))

(defn execute! [message responses op]
  (let [_      (dosync (ref-set op (executing (:id message))))
        id?   #(= (:id message) (:id %))
        done?  (fn [res]
                 (some #(and (id? %) (= "done" (-> % (:status) (first)))) res))]
    (loop [all []]
      (if (done? all)
        (let [res (filter id? all)
              _   (dosync (alter responses #(filter (comp not id?) %))
                          (ref-set op idle))]
          res)
        (recur @responses)))))

(defn client [transport]
  (let [responses (ref [])
        operation (ref idle)]
    (future (gather transport responses))
    (fn [msg]
      (if (->> msg (:op) (= :read-out))
        (future (read-out! responses operation))
        (future
          (let [message (assoc msg :id (random-uuid))]
            (transport/send transport message)
            (execute! message responses operation)))))))

(defn -main [& args]
  (start-server))