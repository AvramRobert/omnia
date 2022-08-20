(ns omnia.util.misc
  (:require [schema.core :as s]
            [clojure.java.io :as io]
            [clojure.edn :as edn]))

(s/defn rand-port :- s/Int
  []
  (rand-int 65535))

(defn slurp-or-else [path else]
  (if (-> path (io/file) (.exists))
    (-> path (slurp) (edn/read-string))
    else))

(defmacro omnia-version []
  (System/getProperty "omnia.version"))
