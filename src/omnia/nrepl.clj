(ns omnia.nrepl
  (:require [clojure.tools.nrepl.server :as server]
            [clojure.tools.nrepl :as nrepl]
            [omnia.more :refer [dec< inc< gulp-or-else => StringUUID StringBool]]
            [clojure.string :refer [split-lines trim-newline join]]
            [omnia.input :refer [Seeker]]
            [halfling.task :refer [task]]
            [omnia.input :as i]
            [omnia.format :as f]
            [schema.core :as s]
            [cider.nrepl.middleware.complete]
            [cider.nrepl.middleware.info]
            [cider.nrepl.middleware.out]))

;; FIXME: Use version 15 of cider.nrepl until the main line fixes issue #447

(def EvalRequest
  {:op  (s/eq :eval)
   :code s/Str})

(def InfoRequest
  {:op     s/Keyword
   :symbol s/Str
   :ns     s/Symbol})

(def ResponseHeader
  {:id      StringUUID
   :session StringUUID})

(def ResponseNamespace
  {:ns s/Str})

(def ResponseStatus
  {:status [s/Str]})

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

(def EvalResponse
  (s/conditional
    val? ValueResponse
    out? OutResponse
    err? ErrorResponse
    exc? ExceptionResponse
    :else TerminatingResponse))

(def CompletionResponse
  (merge ResponseHeader
         ResponseStatus
         {:completions [Completion]}))

(def InfoResponse
  (merge ResponseHeader
         ResponseStatus
         ResponseNamespace
         {:name         s/Str
          :doc          s/Str
          :arglists-str s/Str
          :resource     s/Str
          :added        s/Str
          :see-also     [s/Str]
          :static       StringBool
          :file         s/Str}))

(def NReplResponse
  (let [eval?       #(contains? % :value)
        completion? #(contains? % :completions)
        info?       #(contains? % :doc)]
    (s/conditional
      eval?       EvalResponse
      info?       InfoResponse
      completion? CompletionResponse)))

(def NReplRequest
  (let [eval? #(-> % (:op) (= :eval))]
    (s/conditional
      eval? EvalRequest
      :else InfoRequest)))

(def InternalClient (=> NReplRequest [NReplResponse]))

(def REPLResponse
  {:name s/Str
   :ns   s/Str
   :doc  s/Str
   :args [s/Str]})

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
  (->> '[cider.nrepl.middleware.complete/wrap-complete
         cider.nrepl.middleware.info/wrap-info
         cider.nrepl.middleware.out/wrap-out]
       (map resolve)
       (apply server/default-handler)))

(defn read-history [path]
  (task
    (->> [""]
         (gulp-or-else path)
         (mapv i/from-string))))

(defn write-history [path repl]
  (task (->> (:history repl)
             (mapv i/stringify)
             (take-last 1000)
             (vec)
             (spit path))))

(defn- response->seeker [response]
  (cond
    (out? response) (-> response (:out) (f/format-str) (i/from-string))
    (err? response) (-> response (:err) (i/from-string))
    (val? response) (-> response (:value) (str) (f/format-str) (i/from-string))
    :else i/empty-seeker))

(s/defn send! :- [NReplResponse]
  [repl :- REPLClient
   req  :- NReplRequest]
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

(defn- hsize [repl] (count (:history repl)))

(defn- cache-result [repl result]
  (assoc repl :result result))

(defn- remember [repl seeker]
  (let [lines (:lines seeker)]
    (if (or (empty? lines)
            (-> lines first empty?))
      repl
      (update repl :history #(conj % seeker)))))

(defn- reset-timeline [repl]
  (update repl :timeline (fn [_] (hsize repl))))

(defn travel-back [repl]
  (update repl :timeline #(dec< % 0)))

(defn travel-forward [repl]
  (update repl :timeline #(inc< % (hsize repl))))

(defn then [repl]
  (nth (:history repl) (:timeline repl) i/empty-seeker))

(defn last-eval [repl] (:result repl))

(s/defn complete! :- Seeker
  [repl :- REPLClient
   seeker :- Seeker]
  (->> (:ns repl)
       (complete-msg seeker)
       (send! repl)
       (first)
       (:completions)
       (mapv (comp i/from-string :candidate))
       (i/conjoined)))

(s/defn evaluate! :- Seeker
  [repl   :- REPLClient
   seeker :- Seeker]
  (let [new-line #(conj % i/empty-line)
        result    (->> (eval-msg seeker)
                       (send! repl)
                       (mapv response->seeker)
                       (new-line)
                       (i/conjoined))]
    (-> (remember repl seeker)
        (cache-result result)
        (reset-timeline))))

(s/defn info! :- REPLResponse
  [repl   :- REPLClient
   seeker :- Seeker]
  (let [result (->> (:ns repl) (info-msg seeker) (send! repl) (first))]
    {:name (:name result)
     :ns   (:ns result)
     :doc  (:doc result "")
     :args (-> result (:arglists-str "") (split-lines))}))

(s/defn docs! :- Seeker
  [repl   :- REPLClient
   seeker :- Seeker]
  (->> seeker (info! repl) (:doc) (i/from-string)))

(s/defn signature! :- Seeker
  [repl :- REPLClient
   seeker :- Seeker]
  (let [candidates (fn [{:keys [ns name args]}]
                     (mapv #(i/from-string (str ns "/" name " " %)) args))]
    (->> seeker (info! repl) (candidates) (i/conjoined))))

(s/defn start-server!
  [config :- REPLConfig]
  (server/start-server
    :host (:host config)
    :port (:port config)
    :handler handler))

(defn stop-server! [server]
  (server/stop-server server))

(s/defn connect :- InternalClient
  [config :- REPLConfig]
  (let [host      (:host config)
        port      (:port config)
        timeout   (:timeout config 10000)
        transport (nrepl/connect :port port :host host)
        client    (nrepl/client transport timeout)]
    (fn [msg] (nrepl/message client msg))))

(s/defn client :- REPLClient
  [config :- REPLConfig]
  (let [ns      (:ns config (ns-name *ns*))
        port    (:port config)
        host    (:host config)
        history (:history config [i/empty-seeker])
        client  (:client config)]
    {:ns       ns
     :host     host
     :port     port
     :client   (or client (connect config))
     :history  history
     :timeline (count history)
     :result   i/empty-seeker}))