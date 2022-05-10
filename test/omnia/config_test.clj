(ns omnia.config-test
  (:require [halfling.task :as t]
            [omnia.config.defaults :as d]
            [omnia.config.core :as c]
            [schema.core :as s]
            [clojure.test :refer [is]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :refer [for-all]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [omnia.util.generator :refer [gen-user-keymap
                                          gen-user-terminal
                                          gen-user-highlighting
                                          gen-user-key-binding]]
            [omnia.schema.config :refer [KeyMap Highlighting Terminal]]))

(def ^:const NR-OF-TESTS 100)

(defspec detect-duplicate-bindings
         NR-OF-TESTS
  (let [entries (keys d/default-user-keymap)
        half    (/ (count entries) 2)]
    (for-all [binding gen-user-key-binding
              entry1  (gen/elements (take half entries))
              entry2  (gen/elements (drop half entries))]
             (let [result (-> d/default-user-config
                              (assoc-in [:keymap entry1] binding)
                              (assoc-in [:keymap entry2] binding)
                              (c/validate!)
                              (t/task)
                              (t/run))]
               (is (t/broken? result))))))

(defspec normalise-keymap
         NR-OF-TESTS
  (for-all [custom-keymap gen-user-keymap]
    (is (s/validate KeyMap (c/fix-keymap custom-keymap)))))

(defspec normalise-colours
         NR-OF-TESTS
  (for-all [custom-highlighting gen-user-highlighting]
    (is (s/validate Highlighting (c/fix-highlighting custom-highlighting)))))

(defspec normalise-terminal
         NR-OF-TESTS
  (for-all [terminal gen-user-terminal]
    (is (s/validate Terminal (c/fix-terminal terminal)))))
