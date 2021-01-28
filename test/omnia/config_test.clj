(ns omnia.config-test
  (:require [clojure.test :refer :all]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :refer [for-all]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [omnia.test-utils :refer [one should-be]]
            [halfling.task :as t]
            [omnia.config.defaults :refer [default-user-keymap]]
            [omnia.config.core :as c]))

(def ^:const NR-OF-TESTS 100)

(def gen-keybind (gen/elements default-user-keymap))
(defn gen-keybind-unlike [k] (gen/such-that (fn [[ik _]] (not= k ik)) gen-keybind))

(defspec detect-duplicate-bindings
         NR-OF-TESTS
         (for-all [[ik v] gen-keybind]
                  (let [[k _] (one (gen-keybind-unlike ik))]
                    (-> (assoc-in c/default-user-config [:keymap k] v)
                        (c/validate!)
                        (t/task)
                        (t/run)
                        (t/broken?)
                        (is)))))

(deftest normalise-keymap
  (run! (fn [[k _]]
          (is (contains? k :key))
          (is (contains? k :ctrl))
          (is (contains? k :shift))
          (is (contains? k :alt))) (c/fix-keymap default-user-keymap)))