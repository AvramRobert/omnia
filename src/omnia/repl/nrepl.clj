(ns omnia.repl.nrepl
  (:require [clojure.string :refer [split-lines trim-newline join]]
            [halfling.task :refer [task]]
            [omnia.util.arithmetic :refer [dec< inc<]]
            [omnia.util.schema :refer [=> StringUUID StringBool]]
            [omnia.util.misc :refer [gulp-or-else]]
            [omnia.text.core :refer [Seeker]]
            [omnia.text.core :as i]
            [omnia.text.format :as f]
            [schema.core :as s]
            [nrepl.server :as nrepl-server]
            [nrepl.core :as nrepl-core]
            [cider.nrepl :as cider-nrepl]))

(def EvalRequest
  {:op  (s/eq :eval)
   :code s/Str})

(def InfoRequest
  {:op     s/Keyword
   :symbol s/Str
   :ns     s/Symbol})

(def ResponseHeader
  {:id      StringUUID
   :session StringUUID
   s/Any    s/Any})

(def ResponseNamespace
  {:ns s/Str})

(def ResponseStatus
  {:status [s/Str]})

(def ResponseInfo
  {:name   s/Str})

(def Completion
  {:candidate s/Str
   :ns        s/Str
   :type      s/Str})

(defn- out? [response]
  (contains? response :out))

(defn- err? [response]
  (contains? response :err))

(defn- exc? [response]
  (contains? response :ex))

(defn- val? [response]
  (contains? response :value))

(defn- com? [response]
  (contains? response :completions))

(defn- doc? [response]
  (contains? response :doc))

(defn- arg? [response]
  (contains? response :arglists-str))

(def ValueResponse
  (merge ResponseHeader
         ResponseNamespace
         {:value s/Str}))

(def OutResponse
  (merge ResponseHeader
         {:out s/Str}))

(def ExceptionResponse
  (merge ResponseHeader
         ResponseStatus
         {:ex      s/Str
          :root-ex s/Str}))

(def ErrorResponse
  (merge ResponseHeader
         {:err s/Str}))

(def TerminatingResponse
  (merge ResponseHeader
         ResponseStatus))

(def CompletionResponse
  (merge ResponseHeader
         ResponseStatus
         {:completions [Completion]}))

(def ArgumentResponse
  (merge ResponseHeader
         ResponseStatus
         ResponseNamespace
         ResponseInfo
         {:arglists-str s/Str}))

(def DocResponse
  (merge ResponseHeader
         ResponseStatus
         ResponseNamespace
         ResponseInfo
         {:doc s/Str}))

(def NReplResponse
  (s/conditional
    val?        ValueResponse
    out?        OutResponse
    err?        ErrorResponse
    exc?        ExceptionResponse
    com?        CompletionResponse
    arg?        ArgumentResponse
    doc?        DocResponse
    :else       TerminatingResponse))

(def NReplRequest
  (let [eval? #(-> % (:op) (= :eval))]
    (s/conditional
      eval? EvalRequest
      :else InfoRequest)))

(def InternalClient (=> NReplRequest [NReplResponse]))

(def REPLConfig
  {:host                     s/Str
   :port                     s/Int
   (s/optional-key :client)  InternalClient
   (s/optional-key :ns)      s/Symbol
   (s/optional-key :history) [Seeker]
   (s/optional-key :timeout) s/Int})

(def REPLClient
  {:ns       s/Symbol
   :host     s/Str
   :port     s/Int
   :client   InternalClient
   :history  [Seeker]
   :timeline s/Int
   :result   Seeker})

(def handler
  (->> '[cider-nrepl/wrap-complete
         cider-nrepl/wrap-info
         cider-nrepl/wrap-out]
       (map resolve)
       (apply nrepl-server/default-handler)))

(s/defn read-history :- Seeker
  [path :- s/Str]
  (->> [""]
       (gulp-or-else path)
       (mapv i/from-string)))

(s/defn write-history
  [repl :- REPLClient
   path :- s/Str]
  (->> repl
       (:history)
       (mapv i/stringify)
       (take-last 20)
       (vec)
       (spit path)))

(s/defn response->seeker :- Seeker
  [response :- NReplResponse]
  (cond
    (out? response) (-> response (:out) (f/format-str) (i/from-string))
    (err? response) (-> response (:err) (i/from-string))
    (val? response) (-> response (:value) (f/format-str) (i/from-string))
    :else i/empty-seeker))

(s/defn send! :- [NReplResponse]
  [repl :- REPLClient
   req :- NReplRequest]
  ((:client repl) req))

(s/defn eval-msg :- EvalRequest
  [seeker :- Seeker]
  {:op   :eval
   :code (i/stringify seeker)})

(s/defn complete-msg :- InfoRequest
  [seeker :- Seeker
   ns     :- s/Symbol]
  {:op     :complete
   :ns     ns
   :symbol (-> seeker
               (i/expand)
               (i/extract)
               (i/stringify)
               (trim-newline))})

(s/defn info-msg :- InfoRequest
  [seeker :- Seeker
   ns     :- s/Symbol]
  {:op      :info
   :ns      ns
   :symbol  (-> seeker
                (i/expand)
                (i/extract)
                (i/stringify)
                (trim-newline))})

(s/defn with-result :- REPLClient
  [repl   :- REPLClient
   result :- Seeker]
  (assoc repl :result result))

(s/defn remember :- REPLClient
  [repl :- REPLClient
   seeker :- Seeker]
  (if (or (i/equivalent? seeker i/empty-seeker)
          (i/equivalent? seeker i/empty-line))
    repl
    (update repl :history #(conj % seeker))))

(s/defn reset-timeline :- REPLClient
  [repl :- REPLClient]
  (let [current (-> repl (:history) (count))]
    (assoc repl :timeline current)))

(s/defn travel-back :- REPLClient
  [repl :- REPLClient]
  (update repl :timeline #(dec< % 0)))

(s/defn travel-forward [repl] :- REPLClient
  [repl :- REPLClient]
  (let [max (-> repl (:history) (count))]
    (update repl :timeline #(inc< % max))))

(s/defn then :- Seeker
  [repl :- REPLClient]
  (nth (:history repl) (:timeline repl) i/empty-seeker))

(s/defn result :- Seeker
  [repl :- REPLClient]
  (:result repl))

(s/defn complete! :- REPLClient
  [repl :- REPLClient
   seeker :- Seeker]
  (let [result (->> (:ns repl)
                    (complete-msg seeker)
                    (send! repl)
                    (first)
                    (:completions)
                    (mapv (comp i/from-string :candidate))
                    (i/conjoined))]
    (-> repl (with-result result) (reset-timeline))))

(s/defn evaluate! :- REPLClient
  [repl   :- REPLClient
   seeker :- Seeker]
  (let [new-line #(conj % i/empty-line)
        result    (->> (eval-msg seeker)
                       (send! repl)
                       (mapv response->seeker)
                       (new-line)
                       (i/conjoined))]
    (-> repl
        (remember seeker)
        (with-result result)
        (reset-timeline))))

(s/defn info! :- (s/maybe NReplResponse)
  [repl   :- REPLClient
   seeker :- Seeker]
  (let [result      (->> (:ns repl) (info-msg seeker) (send! repl) (first))
        [_ no-info] (:status result)]
    (when (nil? no-info) result)))

(s/defn docs! :- REPLClient
  [repl   :- REPLClient
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

(s/defn signature! :- REPLClient
  [repl :- REPLClient
   seeker :- Seeker]
  (let [result (or (some-> repl (info! seeker) (make-candidates) (i/conjoined))
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

(s/defn client :- REPLClient
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