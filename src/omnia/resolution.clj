(ns omnia.resolution
  (require
    [halfling.result :as r]
    [cemerick.pomegranate :as p]
    [cemerick.pomegranate.aether :refer [maven-central]]))


(def default (merge maven-central
                    {"clojars" "https://clojars.org/repo/"}))

(defn prettify [d-map]
  (let [primary (keys d-map)
        secondary (->> d-map vals (remove nil?) (reduce concat))]
    (-> primary
        (concat secondary)
        (set))))

(defn- retrieval [repos releases]
  (-> (r/attempt
        (-> (p/add-dependencies :coordinates releases
                                :repositories (merge default repos))
            (prettify)))
      (r/fold
        identity
        #(symbol (str "An error occurred during retrieval\nMessage: " (:message %))))))

(defn retrieve [& releases]
  (retrieval default releases))

(defn retrieve-from [repos & releases]
  (retrieval repos releases))
