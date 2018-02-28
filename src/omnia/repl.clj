(ns omnia.repl
  (:require [clojure.tools.nrepl.server :as s]
            [clojure.tools.nrepl :as nrepl]
            [clojure.core.match :as m]
            [omnia.more :refer [dec< inc< gulp-or-else]]
            [clojure.string :refer [split trim-newline join]]
            [halfling.task :refer [task]]
            [omnia.input :as i]
            [omnia.format :as f]
            [cider.nrepl :refer [cider-nrepl-handler]]))

;; FIXME: Use version 15 of cider.nrepl until the main line fixes issue #447

(defrecord REPL [ns
                 host
                 port
                 send!
                 history
                 timeline
                 result])

(def predef
  (->> ['(require '[omnia.resolution :refer [retrieve retrieve-from]])]
       (mapv str)
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
    (out? response) (-> response (:out) (f/string-format) (i/from-string))
    (err? response) (-> response (:err) (i/from-string))
    (eff? response) [[\n \i \l]]
    (ex? response) i/empty-seeker
    :else (-> response (:value) (str) (f/string-format) (i/from-string))))

(defn- connect [host port timeout]
  (let [conn (nrepl/connect :host host :port port)]
    (fn [msg]
      (-> (nrepl/client conn timeout)
          (nrepl/message msg)))))

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

(defn complete! [{:keys [ns send!]} seeker]
  (->> (complete-msg seeker ns)
       (send!)
       (first)
       (:completions)
       (mapv (comp i/from-string :candidate))
       (apply i/join-many)))

(defn evaluate! [{:keys [send!] :as repl} seeker]
  (let [result (->> (eval-msg seeker)
                    (send!)
                    (mapv response->seeker)
                    (apply i/join-many))]
    (-> (remember repl seeker)
        (cache-result result)
        (reset-timeline))))

(defn info! [{:keys [ns send!]} seeker]
  (let [{arg-list    :arglists-str
         ns          :ns
         doc         :doc
         name        :name
         [_ no-info] :status} (-> (info-msg seeker ns) (send!) (first))]
    (when-not no-info
      {:name name
       :args (-> arg-list (or "") (split #"\n"))
       :ns ns
       :doc (or doc "")})))

(defn add-predef! [repl]
  (evaluate! repl predef))

(defn start-server! [{:keys [host port]}]
  (s/start-server :host host
                  :port port
                  :handler cider-nrepl-handler))

(defn stop-server! [server]
  (s/stop-server server))

;; TODO: the repl may stream responses and would basically signal an end to the messages with a final message containing the value ["done"]
(defn repl [{:as   params
             :keys [ns port host send! timeout history]
             :or   {timeout 10000                            ;; fixme: kill infinite processes and return warning
                    history empty-history
                    ns      (ns-name *ns*)}}]
  (assert (map? params) "Input to `repl` must be a map.")
  (assert (and (not (nil? port))
               (not (nil? host))) "`repl` must receive a host and a port")
  (map->REPL {:ns ns
              :host host
              :port port
              :send! (or send! (connect host port timeout))
              :history history
              :timeline (count history)
              :result i/empty-seeker}))

#_(defn -main [& args]
  (let [config {:host "localhost"
                :port 8080}
        server (start-server! config)
        repl (repl config)
        _ (clojure.pprint/pprint (info! repl (i/from-string "print-ctor")))
        _ (stop-server! server)]))