(ns omnia.repl.nrepl
  (:require [omnia.repl.text :as i]
            [omnia.repl.format :as f]
            [schema.core :as s]
            [nrepl.server :as nrepl-server]
            [nrepl.core :as nrepl-core]
            [cider.nrepl :as cider-nrepl]
            [clojure.string :refer [split-lines trim-newline join]]
            [halfling.task :refer [task]]
            [omnia.util.arithmetic :refer [dec< inc<]]
            [omnia.util.misc :refer [slurp-or-else]]
            [omnia.schema.nrepl :refer :all]
            [omnia.schema.common :refer [=> StringUUID StringBool]]
            [omnia.schema.text :refer [Seeker]]))

(def handler
  (->> '[cider-nrepl/wrap-complete
         cider-nrepl/wrap-info
         cider-nrepl/wrap-out]
       (map resolve)
       (apply nrepl-server/default-handler)))

(s/defn read-history :- Seeker
  [path :- s/Str]
  (->> [""]
       (slurp-or-else path)
       (mapv i/from-string)))

(s/defn write-history
  [repl :- NReplClient
   path :- s/Str]
  (->> repl
       (:history)
       (mapv i/stringify)
       (take-last 20)
       (vec)
       (spit path)))

(s/defn send! :- [NReplResponse]
  [repl :- NReplClient
   req :- NReplRequest]
  ((:client repl) req))

(s/defn make-eval-request :- EvalRequest
  [seeker :- Seeker]
  {:op   :eval
   :code (i/stringify seeker)})

(s/defn make-complete-request :- InfoRequest
  [seeker :- Seeker
   ns :- s/Symbol]
  {:op     :complete
   :ns     ns
   :symbol (-> seeker
               (i/expand-select)
               (i/extract)
               (i/stringify)
               (trim-newline))})

(s/defn make-info-request :- InfoRequest
  [seeker :- Seeker
   ns :- s/Symbol]
  {:op     :info
   :ns     ns
   :symbol (-> seeker
               (i/expand-select)
               (i/extract)
               (i/stringify)
               (trim-newline))})

(s/defn with-result :- NReplClient
  [repl   :- NReplClient
   result :- Seeker]
  (assoc repl :result result))

(s/defn remember :- NReplClient
  [repl :- NReplClient
   seeker :- Seeker]
  (if (or (i/equivalent? seeker i/empty-seeker)
          (i/equivalent? seeker i/empty-line))
    repl
    (update repl :history #(conj % seeker))))

(s/defn reset-timeline :- NReplClient
  [repl :- NReplClient]
  (let [current (-> repl (:history) (count))]
    (assoc repl :timeline current)))

(s/defn travel-back :- NReplClient
  [repl :- NReplClient]
  (update repl :timeline #(dec< % 0)))

(s/defn travel-forward [repl] :- NReplClient
  [repl :- NReplClient]
  (let [max (-> repl (:history) (count))]
    (update repl :timeline #(inc< % max))))

(s/defn then :- Seeker
  [repl :- NReplClient]
  (nth (:history repl) (:timeline repl) i/empty-seeker))

(s/defn result :- Seeker
  [repl :- NReplClient]
  (:result repl))

(s/defn complete! :- NReplClient
  [repl :- NReplClient
   seeker :- Seeker]
  (let [result (->> (:ns repl)
                    (make-complete-request seeker)
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
   seeker :- Seeker]
  (let [result (->> (make-eval-request seeker)
                    (send! repl)
                    (reduce accrete-response "")
                    (i/from-string)
                    (i/end)
                    (i/new-line))]
    (-> repl
        (remember seeker)
        (with-result result)
        (reset-timeline))))

(s/defn info! :- (s/maybe NReplResponse)
  [repl   :- NReplClient
   seeker :- Seeker]
  (let [result      (->> (:ns repl) (make-info-request seeker) (send! repl) (first))
        [_ no-info] (:status result)]
    (when (nil? no-info) result)))

(s/defn docs! :- NReplClient
  [repl   :- NReplClient
   seeker :- Seeker]
  (let [result (or (some-> repl (info! seeker) (:doc) (i/from-string))
                   i/empty-seeker)]
    (-> repl (with-result result) (reset-timeline))))

(s/defn make-candidates :- (s/maybe [Seeker])
  [response :- NReplResponse]
  (let [name      (:name response)
        ns        (:ns response)
        candidate #(i/from-string (str ns "/" name " " %))]
    (some->> response (:arglists-str) (split-lines) (mapv candidate))))

(s/defn signature! :- NReplClient
  [repl :- NReplClient
   seeker :- Seeker]
  (let [result (or (some-> repl (info! seeker) (make-candidates) (i/joined))
                   i/empty-seeker)]
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
     :result   i/empty-seeker}))