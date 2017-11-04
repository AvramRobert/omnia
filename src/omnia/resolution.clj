(ns omnia.resolution
  (require
    [halfling.task :as t]
    [cemerick.pomegranate :as p]
    [cemerick.pomegranate.aether :refer [maven-central]]))

(def default (merge maven-central
                    {"clojars" "https://clojars.org/repo/"}))

(defn- make-output [dependency-map]
  (let [primary (keys dependency-map)
        secondary (->> dependency-map (vals) (remove nil?) (reduce concat))]
    (set (concat primary secondary))))

(defn- retrieval [repos releases]
  @(-> (t/task
         (-> (p/add-dependencies :coordinates releases
                                 :repositories repos)
             (make-output)))
       (t/recover #(symbol (str "An error occurred during retrieval\nMessage: " (:message %))))
       (t/run)))

(defn retrieve [& releases]
  (retrieval default releases))

(defn retrieve-from [repos & releases]
  (retrieval (merge default repos) releases))
