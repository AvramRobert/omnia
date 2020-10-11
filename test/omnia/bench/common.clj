(ns omnia.bench.common
  (:require [clojure.test :refer :all]
            [omnia.more :refer [time-return]]))

(def ^:dynamic *benchmarks* [])

(defmacro defbench [name f]
  (let [ns-f# (-> (ns-name *ns*)
                  (str "/" f)
                  (symbol)
                  (resolve))]
    (alter-var-root
      #'*benchmarks*
      #(conj % [ns-f# (str name)]))))

(defn quick-bench [f n]
  (letfn [(avg [a] (/ a n))]
    (->> (range 0 n)
         (mapv (fn [_]
                 (->> (time-return (f))
                      (first)
                      (drop-last 3)
                      (apply str)
                      (Double/parseDouble))))
         (reduce +)
         (avg)
         (format "Avg time: %s ms"))))
(defn bench-all! [n]
  (run!
    (fn [[f desc]]
      (let [_ (println "Benchmarking `" desc "`")
            result-string (quick-bench f n)]
        (println result-string)
        (println))) *benchmarks*))
