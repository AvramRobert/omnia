(ns omnia.util.misc
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(defn gulp-or-else [path else]
  (if (-> path (io/file) (.exists))
    (-> path (slurp) (edn/read-string))
    else))

(defmacro omnia-version []
  (System/getProperty "omnia.version"))
