(ns omnia.util.misc
  (:require [schema.core :as s]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.string :as string]))

(s/defn read-arg :- (s/maybe s/Str)
  [argument :- s/Str
   args :- [s/Str]]
  (some-> (some #(when (string/starts-with? % argument) %) args)
          (string/split (re-pattern argument))
          (second)
          (string/split (re-pattern "="))
          (second)))

(s/defn rand-port :- s/Int
  []
  (rand-int 65535))

(defn slurp-or-else [path else]
  (if (-> path (io/file) (.exists))
    (-> path (slurp) (edn/read-string))
    else))

(defmacro omnia-version []
  (System/getProperty "omnia.version"))
