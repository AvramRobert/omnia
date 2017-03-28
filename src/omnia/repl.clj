(ns omnia.repl
  (require [clojure.tools.nrepl.server :as s]
           [clojure.tools.nrepl :as nrepl]
           [omnia.input :as i]
           [clojure.core.match :as m]
           [clojure.string :refer [split]]))


(defrecord REPL [host port history hsize timeline result evaluator])

(defn seeker [lines]
  (i/->Seeker lines [0 0]))

(defn out? [response]
  (contains? response :out))

(defn err? [response]
  (contains? response :err))

(defn ex? [response]
  (contains? response :ex))

(defn eff? [response]
  (and (contains? response :value)
       (nil? (:value response))))

(defn str->line [string]
  (->> #"\n"
       (split string)
       (map #(vec (.toCharArray %)))
       (vec)))

(defn response->lines [response]
  (cond
    (out? response) (-> response (:out) (str->line))
    (err? response) (-> response (:err) (str->line))
    (eff? response) [[\n \i \l]]
    (ex? response) []
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

(defn n-repl [repl seeker]
  (with-open [conn (nrepl/connect :port (:port repl)
                                  :host (:host repl))]
    (evaluate! conn seeker)))

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

(defn evaluate-with [repl seeker f]
  (-> repl
      (store seeker)
      (cache-result (f repl seeker))
      (reset-timeline)))

(defn evaluate [repl seeker]
  (case (:evaluator repl)
    :nrepl (evaluate-with repl seeker n-repl)
    :identity (evaluate-with repl seeker (fn [_ x] x))))

(defn repl                                                  ;; I don't really like this
  ([]
   (repl nil nil :identity))
  ([host port]
   (m/match [host port]
            [nil nil] (repl)
            [_   nil] (repl)
            [nil   _] (repl "localhost" port :nrepl)
            :else     (repl host port :nrepl)))
  ([host port evaluator]
   (->REPL host port [i/empty-seeker] 1 0 i/empty-seeker evaluator)))

