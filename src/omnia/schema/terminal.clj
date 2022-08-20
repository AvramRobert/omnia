(ns omnia.schema.terminal
  (:require [schema.core :as s]
            [omnia.display.terminal :as t]))

(def Terminal (s/pred #(satisfies? t/Terminal %)))
