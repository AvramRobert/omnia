(ns omnia.repl.nrepl
  (:require [omnia.repl.text :as i]
            [omnia.repl.format :as f]
            [schema.core :as s]
            [nrepl.server :as nrepl-server]
            [nrepl.core :as nrepl-core]
            [cider.nrepl :as cider-nrepl]
            [clojure.string :refer [split-lines trim-newline]]
            [omnia.util.arithmetic :refer [dec< inc<]]
            [omnia.util.misc :refer [slurp-or-else]]
            [omnia.schema.nrepl :refer :all]
            [omnia.schema.text :refer [Text]]))

(def handler
  (->> '[cider-nrepl/wrap-complete
         cider-nrepl/wrap-info
         cider-nrepl/wrap-out]
       (map resolve)
       (apply nrepl-server/default-handler)))

(s/defn send! :- [NReplResponse]
  [repl :- NReplClient
   req :- NReplRequest]
  ((:client repl) req))

(s/defn make-eval-request :- EvalRequest
  [text :- Text]
  {:op   :eval
   :code (i/as-string text)})

(s/defn make-complete-request :- InfoRequest
  [text :- Text
   ns :- s/Symbol]
  {:op     :complete
   :ns     ns
   :symbol (-> text
               (i/expand-selection)
               (i/extract)
               (i/as-string)
               (trim-newline))})

(s/defn make-info-request :- InfoRequest
  [text :- Text
   ns :- s/Symbol]
  {:op     :info
   :ns     ns
   :symbol (-> text
               (i/expand-selection)
               (i/extract)
               (i/as-string)
               (trim-newline))})

(s/defn complete! :- Text
  [repl :- NReplClient
   text :- Text]
  (->> (:ns repl)
       (make-complete-request text)
       (send! repl)
       (first)
       (:completions)
       (mapv (comp i/from-string :candidate))
       (i/joined)))

(s/defn accrete-response :- s/Str
  "Accretes repl responses to a string.
   Pads value responses with empty lines before and after."
  [output :- s/Str
   response :- NReplResponse]
  (cond
    (out? response) (->> response (:out) (f/format-str) (format "%s%s" output))
    (err? response) (->> response (:err) (format "%s%s" output))
    (exc? response) (->> response (:ex) (format "%sType: %s" output))
    (val? response) (->> response (:value) (f/format-str) (format "%s\n%s" output))
    :else output))

(s/defn evaluate! :- Text
  [repl :- NReplClient
   text :- Text]
  (->> (make-eval-request text)
       (send! repl)
       (reduce accrete-response "")
       (i/from-string)
       (i/end)
       (i/new-line)))

(s/defn info! :- (s/maybe NReplResponse)
  [repl :- NReplClient
   text :- Text]
  (let [result (->> (:ns repl) (make-info-request text) (send! repl) (first))
        [_ no-info] (:status result)]
    (when (nil? no-info) result)))

(s/defn docs! :- Text
  [repl :- NReplClient
   text :- Text]
  (or (some-> repl (info! text) (:doc) (i/from-string))
      i/empty-text))

(s/defn make-candidates :- (s/maybe [Text])
  [response :- NReplResponse]
  (let [name      (:name response)
        ns        (:ns response)
        candidate #(i/from-string (str ns "/" name " " %))]
    (some->> response (:arglists-str) (split-lines) (mapv candidate))))

(s/defn signature! :- Text
  [repl :- NReplClient
   text :- Text]
  (or (some-> repl (info! text) (make-candidates) (i/joined))
      i/empty-text))

(s/defn start-server!
  [config :- REPLConfig]
  (nrepl-server/start-server
    :bind (:host config)
    :port (:port config)
    :handler handler))

(defn stop-server! [server]
  (nrepl-server/stop-server server))

(s/defn connect :- InternalClient
  [config :- REPLConfig]
  (let [host      (:host config)
        port      (:port config)
        timeout   (:timeout config 10000)
        transport (nrepl-core/connect :port port :host host)
        client    (nrepl-core/client transport timeout)]
    (fn [msg] (nrepl-core/message client msg))))

(s/defn client :- NReplClient
  [config :- REPLConfig]
  (let [ns     (:ns config (ns-name *ns*))
        port   (:port config)
        host   (:host config)
        client (:client config)]
    {:ns     ns
     :host   host
     :port   port
     :client (or client (connect config))}))
