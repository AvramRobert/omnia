(ns omnia.misc-test
  (:require [clojure.test :refer :all]
            [omnia.util.misc :as m]))

(deftest reads-arguments
  (testing "Reads path arguments"
    (let [arg    "app-path"
          value   "/home/where/i/am"
          args   (list (format "%s=%s" arg value))
          actual (m/read-arg "app-path" args)]
      (is (= value actual)))))
