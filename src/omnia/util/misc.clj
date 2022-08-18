(ns omnia.util.misc
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(defn rand-port []
  (rand-int 65535))

(defn slurp-or-else [path else]
  (if (-> path (io/file) (.exists))
    (-> path (slurp) (edn/read-string))
    else))

(defmacro omnia-version []
  (System/getProperty "omnia.version"))
