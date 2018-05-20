(ns omnia.repl
  (:require [clojure.tools.nrepl.server :as s]
            [clojure.tools.nrepl :as nrepl]
            [omnia.more :refer [dec< inc< gulp-or-else]]
            [clojure.string :refer [split trim-newline join]]
            [halfling.task :refer [task]]
            [omnia.input :as i]
            [omnia.format :as f]
            [cider.nrepl :refer [cider-nrepl-handler]]
            [clojure.tools.nrepl :as n]))

;; FIXME: Use version 15 of cider.nrepl until the main line fixes issue #447

(defrecord REPL [ns
                 host
                 port
                 client
                 history
                 timeline
                 result])

(def predef-resolution
  "(require '[omnia.resolution :refer [retrieve retrieve-from]])")

(def predef
  (->> [predef-resolution]
       (join "\n")
       (i/from-string)))

(def empty-history [i/empty-seeker])

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

(defn- out? [response]
  (contains? response :out))

(defn- err? [response]
  (contains? response :err))

(defn- ex? [response]
  (contains? response :ex))

(defn- eff? [response]
  (and (contains? response :value)
       (nil? (:value response))))

(defn- response->seeker [response]
  (cond
    (out? response) (-> response (:out) (f/format-str) (i/from-string))
    (err? response) (-> response (:err) (i/from-string))
    (eff? response) [[\n \i \l]]
    (ex? response) i/empty-seeker
    :else (-> response (:value) (str) (f/format-str) (i/from-string))))

(defn- connect [host port timeout]
  (n/client (n/connect :host host :port port) timeout))

(defn- send! [repl msg]
  (-> (:client repl) (n/message msg)))

(defn- eval-msg [seeker]
  {:op   :eval
   :code (i/stringify seeker)})

(defn- complete-msg [seeker ns]
  {:op     :complete
   :symbol (-> seeker
               (i/expand)
               (i/extract)
               (i/stringify)
               (trim-newline))
   :ns     ns})

(defn- info-msg [seeker ns]
  {:op      :info
   :symbol  (-> seeker
                (i/expand)
                (i/extract)
                (i/stringify)
                (trim-newline))
   :ns      ns})

(defn- hsize [repl] (count (:history repl)))

(defn- cache-result [repl result]
  (update repl :result (fn [_] result)))

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

(defn result [repl] (:result repl))

(defn complete! [repl seeker]
  (->> (complete-msg seeker (:ns repl))
       (send! repl)
       (first)
       (:completions)
       (mapv (comp i/from-string :candidate))
       (apply i/join-many)))

(defn evaluate! [repl seeker]
  (let [result (->> (eval-msg seeker)
                    (send! repl)
                    (mapv response->seeker)
                    (apply i/join-many))]
    (-> (remember repl seeker)
        (cache-result result)
        (reset-timeline))))

(defn info! [repl seeker]
  (let [{arg-list    :arglists-str
         ns          :ns
         doc         :doc
         name        :name
         [status]    :status} (->> (:ns repl) (info-msg seeker) (send! repl) (first))]
    (when (= "done" status)
      {:name name
       :args (-> arg-list (or "") (split #"\n"))
       :ns   ns
       :doc  (if (empty? doc) "" doc)})))

(defn read-out! [repl]
  (let [transport (-> repl :client meta :clojure.tools.nrepl/transport)]
    (->> (nrepl/response-seq transport 1000)
         (filter #(or (:out %) (:err %)))
         (map response->seeker))))

(defn add-predef! [repl]
  (evaluate! repl predef))

(defn start-server! [{:keys [host port]}]
  (s/start-server :host host
                  :port port
                  :handler cider-nrepl-handler))

(defn stop-server! [server]
  (s/stop-server server))

(defn repl [{:as   params
             :keys [ns port host client timeout history]
             :or   {timeout 1000
                    history empty-history
                    ns      (ns-name *ns*)}}]
  (assert (map? params) "Input to `repl` must be a map.")
  (assert (and (not (nil? port))
               (not (nil? host))) "`repl` must receive a host and a port")
  (let [client   (or client (connect host port timeout))
        timeline (count history)]
    (map->REPL {:ns       ns
                :host     host
                :port     port
                :client   client
                :history  history
                :timeline timeline
                :result   i/empty-seeker})))