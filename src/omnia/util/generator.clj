(ns omnia.util.generator
  (:require [clojure.test.check.generators :as gen]))

(defmacro do-gen [binding & body]
  (let [[[n# b#] & bound] (->> binding (destructure) (partition 2) (reverse))]
    (reduce
      (fn [expr [bn# bb#]]
        `(gen/bind ~bb# (fn [~bn#] ~expr))) `(gen/fmap (fn [~n#] ~@body) ~b#) bound)))
