(ns omnia.schema.nrepl
  (:require [schema.core :as s]
            [omnia.schema.text :as t]
            [omnia.schema.common :as c]))

(defn out? [response]
  (contains? response :out))

(defn err? [response]
  (contains? response :err))

(defn exc? [response]
  (contains? response :ex))

(defn val? [response]
  (contains? response :value))

(defn com? [response]
  (contains? response :completions))

(defn doc? [response]
  (contains? response :doc))

(defn arg? [response]
  (contains? response :arglists-str))

(def EvalRequest
  {:op  (s/eq :eval)
   :code s/Str})

(def InfoRequest
  {:op     s/Keyword
   :symbol s/Str
   :ns     s/Symbol})

(def ResponseHeader
  {:id      c/StringUUID
   :session c/StringUUID
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
  (let [eval?  #(-> % (:op) (= :eval))]
    (s/conditional
      eval? EvalRequest
      :else InfoRequest)))

(def InternalClient (c/=> NReplRequest [NReplResponse]))

(def NReplClient
  {:ns       s/Symbol
   :host     s/Str
   :port     s/Int
   :client   InternalClient
   :history  [t/Text]
   :timeline s/Int
   :result   t/Text})

(def REPLConfig
  {:host                     s/Str
   :port                     s/Int
   (s/optional-key :client)  InternalClient
   (s/optional-key :ns)      s/Symbol
   (s/optional-key :history) [t/Text]
   (s/optional-key :timeout) s/Int})