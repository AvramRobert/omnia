(ns omnia.repl
  (require
    ritz.nrepl.middleware.javadoc
    ritz.nrepl.middleware.simple-complete
    [clojure.tools.nrepl.server :as s]
    [clojure.tools.nrepl :as nrepl]
    [omnia.more :refer [dec< inc< gulp-or-else]]
    [clojure.string :refer [split trim-newline]]
    [halfling.task :refer [task]]
    [omnia.input :as i]
    [clojure.core.match :as m]
    [omnia.format :as f]
    [clojure.edn :as edn]))

(defrecord REPL [ns
                 host
                 port
                 eval-f
                 complete-f
                 stop-f
                 history
                 timeline
                 result])

(def ritz-middleware
  [#'ritz.nrepl.middleware.javadoc/wrap-javadoc
   #'ritz.nrepl.middleware.simple-complete/wrap-simple-complete])

(def predef
  (-> '(require '[omnia.resolution :refer [retrieve retrieve-from]])
      (str)
      (i/from-string)))

(def localhost "127.0.0.1")
(def gibberish "~/~")
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

(defn- suggestion [responses]
  (->> responses
       (first)
       (:value)
       (str)
       (edn/read-string)
       (first)
       (mapv i/from-string)
       (apply i/join-many)))

(defn- seekerise [responses]
  (->> responses
       (map response->seeker)
       (apply i/join-many)))

(defn- connect [host port timeout]
  (let [conn (nrepl/connect :host host :port port)]
    (fn [msg transform]
      (-> (nrepl/client conn timeout)
          (nrepl/message msg)
          (transform)))))

(defn- eval-msg [seeker]
  {:op   :eval
   :code (i/stringify seeker)})

(defn- complete-msg [seeker ns]
  (letfn [(purge [word] (if (empty? word) gibberish word))]
    {:op     :complete
     :symbol (-> seeker
                 (i/expand-word)
                 (i/extract)
                 (i/stringify)
                 (trim-newline)
                 (purge))
     :ns     ns}))

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

(defn result [repl]
  (:result repl))

(defn evaluate [repl seeker]
  (let [f (:eval-f repl)]
    (-> repl
        (remember seeker)
        (cache-result (f seeker))
        (reset-timeline))))

(defn suggest [repl seeker]
  (let [f (:complete-f repl)]
    (f seeker)))

(defn stop! [repl] ((:stop-f repl)))

(defn- repl-with [ns host port eval-f complete-f stop-f history]
  (REPL. ns host port eval-f complete-f stop-f history (hsize history) i/empty-seeker))

(defn- rand-port [] (rand-int 65535))

(defn repl [{:as   params
             :keys [kind ns port host timeout history]
             :or   {kind    :local
                    timeout 10000                            ;; fixme: kill infinite processes and return warning
                    port    (rand-port)
                    host    localhost
                    history empty-history
                    ns      (ns-name *ns*)}}]
  (assert (map? params) "Input to `repl` must be a map.")
  (case kind
    :identity (repl-with ns "" "" identity identity (constantly nil) history)
    :remote (repl-with ns host port (connect host port timeout) identity (constantly nil) history)
    :local (let [handler (apply s/default-handler ritz-middleware)
                 server  (s/start-server :port port
                                         :handler handler)
                 send-f  (connect localhost port timeout)
                 eval-f  #(send-f (eval-msg %) seekerise)
                 comp-f  #(send-f (complete-msg % ns) suggestion)
                 stop-f  #(s/stop-server server)]
             (eval-f predef)
             (repl-with ns host port eval-f comp-f stop-f history))))