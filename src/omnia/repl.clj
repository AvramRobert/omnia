(ns omnia.repl
  (require [clojure.tools.nrepl.server :as s]
           [clojure.tools.nrepl :as repl]
           [omnia.input :as i]
           [clojure.core.match :as m]
           [clojure.string :refer [split]]))

(defn seeker [lines]
  (i/->Seeker lines [0 0]))

(defn out? [response]
  (contains? response :out))

(defn str->line [string]
  (->> #"\n"
       (clojure.string/split string)
       (map #(vec (.toCharArray %)))
       (vec)))

(defn response->lines [response]
  (m/match [response]
           [_ :guard out?] (-> response (:out) (str->line))
           [{:value nil}] [[\n \i \l]]
           :else (-> response (:value) (str) (str->line))))

(defn seekify-responses [responses]
  (->> responses
       (map response->lines)
       (reduce concat)
       (vec)
       (seeker)))

(defn evaluate!
  ([conn seeker]
   (evaluate! conn seeker 10000))
  ([conn seeker timeout]
   (-> (repl/client conn timeout)
       (repl/message {:op   :eval
                      :code (i/stringify seeker)})
       (seekify-responses))))

(defn evaluator
  ([port]
   (evaluator "localhost" port))
  ([host port]
   (fn [seeker]
     (with-open [conn (repl/connect :port port
                                    :host host)]
       (evaluate! conn seeker)))))
