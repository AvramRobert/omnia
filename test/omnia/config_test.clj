(ns omnia.config-test
  (:require [halfling.task :as t]
            [omnia.config.defaults :as d]
            [omnia.config.core :as c]
            [schema.core :as s]
            [omnia.util.generator :as g]
            [clojure.test :refer [is]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :refer [for-all]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [omnia.schema.config :refer [KeyMap Highlighting Terminal Persistence]]))

(def ^:const NR-OF-TESTS 100)

(defspec detect-duplicate-bindings NR-OF-TESTS
  (let [entries (keys d/default-user-keymap)
        half    (/ (count entries) 2)]
    (for-all [binding g/gen-user-key-binding
              entry1  (gen/elements (take half entries))
              entry2  (gen/elements (drop half entries))]
      (let [result (-> d/default-user-config
                       (assoc-in [:keymap entry1] binding)
                       (assoc-in [:keymap entry2] binding)
                       (c/validate!)
                       (t/task)
                       (t/run))]
        (is (t/broken? result))))))

(defspec normalise-keymap NR-OF-TESTS
  (for-all [custom-keymap g/gen-user-keymap]
    (is (s/validate KeyMap (c/fix-keymap custom-keymap)))))

(defspec normalise-colours NR-OF-TESTS
  (for-all [custom-highlighting g/gen-user-highlighting]
    (is (s/validate Highlighting (c/fix-highlighting custom-highlighting)))))

(defspec normalise-terminal NR-OF-TESTS
  (for-all [terminal g/gen-user-terminal]
    (is (s/validate Terminal (c/fix-terminal terminal)))))

(defspec normalise-persistence NR-OF-TESTS
  (for-all [persistence g/gen-user-persistence]
    (is (s/validate Persistence (c/fix-persistence persistence)))))
