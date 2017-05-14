(ns omnia.resolution
  (require [cemerick.pomegranate :as p]
           [cemerick.pomegranate.aether :refer [maven-central]]))


(def default (merge maven-central
                    {"clojars" "https://clojars.org/repo/"}))

(defn prettify [d-map]
  (let [primary (keys d-map)
        secondary (->> d-map vals (remove nil?) (reduce concat))]
    (-> primary
        (concat secondary)
        (set))))

(defn retrieve [& releases]
  (-> (p/add-dependencies :coordinates releases
                          :repositories default)
      (prettify)))

(defn retrieve-from [repos & releases]
  (-> (p/add-dependencies :coordinates releases
                          :repositories (merge default repos))
      (prettify)))
