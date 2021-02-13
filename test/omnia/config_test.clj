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

(def gen-keymap-entry
  (gen/elements default-user-keymap))

(defn gen-keymap-entry-unlike [[event _]]
  (gen/elements (dissoc default-user-keymap event)))

(defspec detect-duplicate-bindings NR-OF-TESTS
  (for-all [[event binding] gen-keymap-entry]
           (let [binding' (-> (gen-keymap-entry-unlike [event binding]) (one) (val))
                 result   (-> c/default-config
                              (assoc-in [:keymap event] binding')
                              (c/validate!)
                              (t/task)
                              (t/run))]
             (is (t/broken? result)))))

(deftest normalise-keymap
  (run! (fn [[_ binding]]
          (is (contains? binding :key))
          (is (contains? binding :ctrl))
          (is (contains? binding :shift))
          (is (contains? binding :alt))) (c/fix-keymap default-user-keymap)))