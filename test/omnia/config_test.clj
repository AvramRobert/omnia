(ns omnia.config-test
  (:require [clojure.test :refer :all]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :refer [for-all]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [omnia.test-utils :refer [just-one can-be]]
            [omnia.highlight :as h]
            [omnia.config :as c]
            [halfling.result :as r]))

(def gen-entry (-> (dissoc c/default-config c/keymap c/colourscheme) (gen/elements)))
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

(defspec patch-missing-keys
         100
         (for-all [[bk bv] gen-keybind
                   [ek ev] gen-entry]
                  (-> (update c/default-config c/keymap #(dissoc % bk))
                      (dissoc c/default-config ek)
                      (c/patch)
                      (can-be #(-> % (get-in [c/keymap bk]) (= bv))
                              #(-> % (get ek) (= false))))))

(deftest deactivate-highlighting
  (-> c/default-config
      (update c/highlighting (constantly false))
      (c/with-features)
      (get c/colourscheme)
      (= h/default-selection-cs)
      (is)))

(deftest deactivate-scrolling
  (-> c/default-config
      (update c/scrolling (constantly false))
      (c/with-features)
      (get c/keymap)
      (can-be #(is (not (contains? % :scroll-up)))
              #(is (not (contains? % :scroll-down))))))

(deftest deactivate-suggestions
  (-> c/default-config
      (update c/suggestions (constantly false))
      (c/with-features)
      (get c/keymap)
      (can-be #(is (not (contains? % :suggest))))))

(deftest normalise-keymap
  (-> (c/with-features c/default-config)
      (get c/keymap)
      (->> (every? (fn [[k _]]
                     (and (contains? k :key)
                          (contains? k :ctrl)
                          (contains? k :shift)
                          (contains? k :alt)))))
      (is)))


