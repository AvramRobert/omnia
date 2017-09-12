(ns omnia.resolution
  (require
    [halfling.result :as r]
    [cemerick.pomegranate :as p]
    [cemerick.pomegranate.aether :refer [maven-central]]))

(def default (merge maven-central
                    {"clojars" "https://clojars.org/repo/"}))

(defn- make-output [dependency-map]
  (let [primary (keys dependency-map)
        secondary (->> dependency-map (vals) (remove nil?) (reduce concat))]
    (set (concat primary secondary))))

(defn- retrieval [repos releases]
  (-> (r/attempt
        (-> (p/add-dependencies :coordinates releases
                                :repositories repos)
            (make-output)))
      (r/fold
        identity
        #(symbol (str "An error occurred during retrieval\nMessage: " (:message %))))))

(defn retrieve [& releases]
  (retrieval default releases))

(defn retrieve-from [repos & releases]
  (retrieval (merge default repos) releases))
