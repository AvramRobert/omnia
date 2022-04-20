(ns omnia.config-test
  (:require [clojure.test :refer :all]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :refer [for-all]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.set :refer [map-invert]]
            [omnia.util.generator :refer [do-gen
                                          one
                                          gen-user-keymap
                                          gen-user-terminal
                                          gen-user-highlighting
                                          gen-user-key-binding]]
            [omnia.util.collection :refer [map-vals]]
            [halfling.task :as t]
            [omnia.schema.config :as cc]
            [omnia.config.defaults :as d]
            [omnia.config.core :as c]
            [schema.core :as s]))

(def ^:const NR-OF-TESTS 100)

(defspec detect-duplicate-bindings
         NR-OF-TESTS
  (let [entries (keys d/default-user-keymap)
        half    (/ (count entries) 2)]
    (for-all [binding gen-user-key-binding
              entry1  (gen/elements (take half entries))
              entry2  (gen/elements (drop half entries))]
             (let [result (-> c/default-config
                              (assoc-in [:keymap entry1] binding)
                              (assoc-in [:keymap entry2] binding)
                              (c/validate!)
                              (t/task)
                              (t/run))]
               (is (t/broken? result))))))

(defspec normalise-keymap
         NR-OF-TESTS
  (for-all [custom-keymap gen-user-keymap]
    (is (s/validate cc/KeyMap (c/fix-keymap custom-keymap)))))

(defspec normalise-colours
         NR-OF-TESTS
  (for-all [custom-highlighting gen-user-highlighting]
    (is (s/validate cc/Highlighting (c/fix-highlighting custom-highlighting)))))

(defspec normalise-terminal
         NR-OF-TESTS
  (for-all [terminal gen-user-terminal]
    (is (s/validate cc/Terminal (c/fix-terminal terminal)))))