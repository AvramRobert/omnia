(ns omnia.config-test
  (:require [clojure.test :refer :all]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :refer [for-all]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [omnia.test-utils :refer [one can-be]]
            [omnia.highlight :as h]
            [omnia.config :as c]
            [halfling.task :as t]))

(def gen-keybind (gen/elements c/default-keymap))
(defn gen-keybind-unlike [k] (gen/such-that (fn [[ik _]] (not= k ik)) gen-keybind))

(defspec detect-duplicate-bindings
         100
         (for-all [[ik v] gen-keybind]
                  (let [[k _] (one (gen-keybind-unlike ik))]
                    (-> (assoc-in c/default-config [c/keymap k] v)
                        (c/validate)
                        (t/task)
                        (t/run)
                        (t/broken?)
                        (is)))))

(deftest normalise-keymap
  (run! (fn [[_ v]]
          (is (contains? v :key))
          (is (contains? v :ctrl))
          (is (contains? v :shift))
          (is (contains? v :alt))) (c/normalise c/default-keymap)))