(ns omnia.repl
  (require [clojure.tools.nrepl.server :as s]
           [clojure.tools.nrepl :as nrepl]
           [omnia.input :as i]
           [clojure.core.match :as m]
           [clojure.string :refer [split]]))


(defrecord REPL [host port history hsize timeline result])

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
   (-> (nrepl/client conn timeout)
       (nrepl/message {:op   :eval
                       :code (i/stringify seeker)})
       (seekify-responses))))

(defn bound-inc [value max]
  (let [x (inc value)]
    (if (>= x max) value x)))                               ;; >= because we count from 0

(defn bound-dec [value min]
  (let [x (dec value)]
    (if (< x min) value x)))

(defn cache-result [repl result]
  (update repl :result (fn [_] result)))

(defn store [repl seeker]
  (-> repl
      (update :history #(conj % seeker))
      (update :hsize inc)))

(defn travel-back [repl]
  (update repl :timeline #(bound-dec % 0)))

(defn travel-forward [repl]
  (update repl :timeline #(bound-inc % (:hsize repl))))

(defn now [repl]
  (nth (:history repl) (:timeline repl) i/empty-seeker))

(defn reset-timeline [repl]
  (update repl :timeline (fn [_] (:hsize repl))))

(defn result [repl]
  (:result repl))

(defn evaluate [repl seeker]
  (with-open [conn (nrepl/connect :port (:port repl)
                                  :host (:host repl))]
    (-> repl
        (store seeker)
        (cache-result (evaluate! conn seeker))
        (reset-timeline))))

(defn repl
  ([port]
   (repl "localhost"))
  ([host port]
   (->REPL host port [i/empty-seeker] 1 0 i/empty-seeker)))

