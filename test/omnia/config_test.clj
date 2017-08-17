(ns omnia.config-test
  (:require [clojure.test :refer :all]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :refer [for-all]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [omnia.test-utils :refer [just-one]]
            [omnia.config :as c]
            [halfling.result :as r]))

(def gen-keybind (gen/elements c/default-keymap))
(defn gen-keybind-unlike [k] (gen/such-that (fn [[ik _]] (not= k ik)) gen-keybind))

(defspec detect-duplicate-bindings
         100
         (for-all [[ik v] gen-keybind]
                  (let [[k _] (just-one (gen-keybind-unlike ik))]
                    (-> (assoc-in c/default-config [c/keymap k] v)
                        (c/validate)
                        (r/attempt)
                        (r/failed?)
                        (is)))))

(deftest patch-missing-keys true)

(deftest deactivate-features true)


