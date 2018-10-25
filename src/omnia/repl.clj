(ns omnia.repl
  (:require [clojure.tools.nrepl.server :as s]
            [clojure.tools.nrepl :as nrepl]
            [omnia.more :refer [dec< inc< gulp-or-else]]
            [clojure.string :refer [split trim-newline join]]
            [halfling.task :refer [task]]
            [omnia.input :as i]
            [omnia.format :as f]
            [cider.nrepl.middleware.complete]
            [cider.nrepl.middleware.info]
            [cider.nrepl.middleware.out]))

;; FIXME: Use version 15 of cider.nrepl until the main line fixes issue #447

(defrecord REPL [ns
                 host
                 port
                 client
                 history
                 timeline
                 result])

(def handler
  (->> '[cider.nrepl.middleware.complete/wrap-complete
         cider.nrepl.middleware.info/wrap-info
         cider.nrepl.middleware.out/wrap-out]
       (map resolve)
       (apply s/default-handler)))

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

(defn- val? [response]
  (contains? response :value))

(defn- response->seeker [response]
  (cond
    (out? response) (-> response (:out) (f/format-str) (i/from-string))
    (err? response) (-> response (:err) (i/from-string))
    (eff? response) [[\n \i \l]]
    (val? response) (-> response (:value) (str) (f/format-str) (i/from-string))
    :else i/empty-seeker))

(defn- send! [repl msg]
  (let [client (:client repl)]
    (client msg)))

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
  (->> (:ns repl)
       (complete-msg seeker)
       (send! repl)
       (first)
       (:completions)
       (mapv (comp i/from-string :candidate))
       (apply i/join-many)))

(defn evaluate! [repl seeker]
  (let [new-line #(conj % i/empty-line)
        result    (->> (eval-msg seeker)
                       (send! repl)
                       (mapv response->seeker)
                       (new-line)
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

(defn out-subscribe! [repl]
  (send! repl {:op :out-subscribe}))

(defn read-out! [repl]
  (send! repl {:op :read-out}))

(defn start-server! [{:keys [host port]}]
  (s/start-server :host host
                  :port port
                  :handler handler))

(defn stop-server! [server]
  (s/stop-server server))

(defn- pipe-out! [transport pipe]
  (letfn [(wait! [] (reset! pipe :waiting) nil)
          (gather! [timeout]
            (some->> (nrepl/response-seq transport timeout)
                     (filter #(or (:out %) (:err %) (:ex %)))
                     (mapv response->seeker)
                     (seq)))]
    (case @pipe
      :stop (wait!)
      :continue (gather! 100)
      nil)))

(defn- wait-ack [pipe]
  (loop [x @pipe]
    (if (= :waiting x) nil (recur @pipe))))

(defn connect [host port timeout]
  (let [pipe      (atom :continue)
        transport (nrepl/connect :port port :host host)
        client    (nrepl/client transport timeout)]
    (fn [msg]
      (if (->> msg (:op) (= :read-out))
        (pipe-out! transport pipe)
        (let [_      (reset! pipe :stop)
              _      (wait-ack pipe)
              result (-> client (nrepl/message msg) (vec))
              _      (reset! pipe :continue)]
          result)))))

(defn repl [{:as   params
             :keys [ns port host client timeout history]
             :or   {timeout 10000
                    history [i/empty-seeker]
                    ns      (ns-name *ns*)}}]
  (assert (map? params) "Input to `repl` must be a map.")
  (assert (and (not (nil? port))
               (not (nil? host))) "`repl` must receive a host and a port")
  (map->REPL {:ns ns
              :host host
              :port port
              :client (or client (connect host port timeout))
              :history history
              :timeline (count history)
              :result i/empty-seeker}))