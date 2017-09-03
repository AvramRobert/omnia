(ns omnia.rendering-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :refer [for-all]]
            [clojure.test.check.generators :as gen]
            [omnia.test-utils :refer :all]
            [omnia.rendering :as r]))

;; I. Total rendering

(defn total-render-within-fov [ctx] true)
(defn total-render-projected [ctx] true)

(defn total-render [ctx]
  (total-render-within-fov ctx)
  (total-render-projected ctx))

(defspec test-total-render
         100
         (for-all [tctx (gen-context {:size   5
                                      :fov    27
                                      :seeker (one (gen-seeker-of 29))})]
                  (total-render tctx)))

;; II. Diffed rendering
(defn diff-render [ctx] true)

;; III. No rendering
(defn nothing-render [ctx] true)

;; IV. Selection highlighting
(defn selection-render [ctx] true)

;; V. Clean-up highlighting
(defn clean-up-render [ctx] true)

;; VI. Hud projection
(defn hud-projection [ctx] true)

;; VII. Selection projection
(defn selection-projection [ctx] true)

;; VIII. Coordinate projection
(defn cursor-projection [ctx] true)
(defn y-projection [ctx] true)

;; IX. Line printing ???
(defn line-print [] true)