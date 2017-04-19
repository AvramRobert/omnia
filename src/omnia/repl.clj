(ns omnia.repl
  (use omnia.more)
  (require [clojure.tools.nrepl.server :as s]
           [clojure.tools.nrepl :as nrepl]
           [omnia.input :as i]
           [clojure.core.match :as m]
           [clojure.string :refer [split]]
           [omnia.formatting :as f]))

(comment
  ;; FIXME
  " 1. Add nrepl server start and stop. // done
    2. Refactor repl initiation // done
    3. Add preloading of functions and dependencies.
    4. Add repl session loading.")

(defrecord REPL [eval-f stop-f history hsize timeline result])

(defn- out? [response]
  (contains? response :out))

(defn- err? [response]
  (contains? response :err))

(defn- ex? [response]
  (contains? response :ex))

(defn- eff? [response]
  (and (contains? response :value)
       (nil? (:value response))))

(defn- response->lines [response]
  (cond
    (out? response) (-> response (:out) (f/fmt-edn) (i/str->lines))
    (err? response) (-> response (:err) (i/str->lines))
    (eff? response) [[\n \i \l]]
    (ex? response) []
    :else (-> response (:value) (str) (f/fmt-edn) (i/str->lines))))

(defn- seekify-responses [responses]
  (->> responses
       (map response->lines)
       (apply i/join-lines)
       (i/seeker)))

(defn- connect [host port timeout]
  (fn [seeker]
    (with-open [conn (nrepl/connect :host host :port port)]
      (-> (nrepl/client conn timeout)
          (nrepl/message {:op   :eval
                          :code (i/stringify seeker)})
          (seekify-responses)))))

(defn- cache-result [repl result]
  (update repl :result (fn [_] result)))

(defn- remember [repl seeker]
  (let [lines (:lines seeker)]
    (if (or (i/is-empty? seeker)
            (-> lines first empty?))
      repl
      (-> repl
          (update :history #(conj % seeker))
          (update :hsize inc)))))

(defn- reset-timeline [repl]
  (update repl :timeline (fn [_] (:hsize repl))))

(defn travel-back [repl]
  (update repl :timeline #(bound-dec % 0)))

(defn travel-forward [repl]
  (update repl :timeline #(bound-inc % (:hsize repl))))

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

(defn stop [repl] ((:stop-f repl)))

(defn- repl-with [eval-f stop-f]
  (REPL. eval-f stop-f [i/empty-seeker] 1 0 i/empty-seeker))

(defn repl [{:as   params
             :keys [kind port host timeout]
             :or   {kind    :local
                    timeout 5000                           ;; fixme: kill infinte processes and return warning
                    port    11111
                    host    "localhost"}}]
  (assert (map? params) "Input to `repl` must be a map.")
  (case kind
    :identity (repl-with identity (fn [] nil))
    :remote (repl-with (connect host port timeout) (fn [] nil))
    :local (let [server (s/start-server :port port)]
             (repl-with (connect "localhost" port timeout)
                        (fn [] (s/stop-server server) nil)))))

