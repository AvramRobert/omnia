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

(s/defn read-history :- Text
  [path :- s/Str]
  (->> [""]
       (slurp-or-else path)
       (mapv i/from-string)))

(s/defn write-history
  [repl :- NReplClient
   path :- s/Str]
  (->> repl
       (:history)
       (mapv i/as-string)
       (take-last 20)
       (vec)
       (spit path)))

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

(s/defn with-result :- NReplClient
  [repl   :- NReplClient
   result :- Text]
  (assoc repl :result result))

(s/defn remember :- NReplClient
  [repl :- NReplClient
   text :- Text]
  (if (or (i/equivalent? text i/empty-text)
          (i/equivalent? text i/empty-line))
    repl
    (update repl :history #(conj % text))))

(s/defn reset-timeline :- NReplClient
  [repl :- NReplClient]
  (let [current (-> repl (:history) (count))]
    (assoc repl :timeline current)))

(s/defn travel-back :- NReplClient
  [repl :- NReplClient]
  (update repl :timeline #(dec< % 0)))

(s/defn travel-forward :- NReplClient
  [repl :- NReplClient]
  (let [max (-> repl (:history) (count))]
    (update repl :timeline #(inc< % max))))

(s/defn then :- Text
  [repl :- NReplClient]
  (nth (:history repl) (:timeline repl) i/empty-text))

(s/defn result :- Text
  [repl :- NReplClient]
  (:result repl))

(s/defn complete! :- NReplClient
  [repl :- NReplClient
   text :- Text]
  (let [result (->> (:ns repl)
                    (make-complete-request text)
                    (send! repl)
                    (first)
                    (:completions)
                    (mapv (comp i/from-string :candidate))
                    (i/joined))]
    (-> repl (with-result result) (reset-timeline))))

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

(s/defn evaluate! :- NReplClient
  [repl   :- NReplClient
   text :- Text]
  (let [result (->> (make-eval-request text)
                    (send! repl)
                    (reduce accrete-response "")
                    (i/from-string)
                    (i/end)
                    (i/new-line))]
    (-> repl
        (remember text)
        (with-result result)
        (reset-timeline))))

(s/defn info! :- (s/maybe NReplResponse)
  [repl   :- NReplClient
   text :- Text]
  (let [result      (->> (:ns repl) (make-info-request text) (send! repl) (first))
        [_ no-info] (:status result)]
    (when (nil? no-info) result)))

(s/defn docs! :- NReplClient
  [repl   :- NReplClient
   text :- Text]
  (let [result (or (some-> repl (info! text) (:doc) (i/from-string))
                   i/empty-text)]
    (-> repl (with-result result) (reset-timeline))))

(s/defn make-candidates :- (s/maybe [Text])
  [response :- NReplResponse]
  (let [name      (:name response)
        ns        (:ns response)
        candidate #(i/from-string (str ns "/" name " " %))]
    (some->> response (:arglists-str) (split-lines) (mapv candidate))))

(s/defn signature! :- NReplClient
  [repl :- NReplClient
   text :- Text]
  (let [result (or (some-> repl (info! text) (make-candidates) (i/joined))
                   i/empty-text)]
    (-> repl (with-result result) (reset-timeline))))

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
  (let [ns      (:ns config (ns-name *ns*))
        port    (:port config)
        host    (:host config)
        history (:history config [])
        client  (:client config)]
    {:ns       ns
     :host     host
     :port     port
     :client   (or client (connect config))
     :history  history
     :timeline (count history)
     :result   i/empty-text}))
