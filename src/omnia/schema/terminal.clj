(ns omnia.schema.terminal
  (:require [schema.core :as s]))

(def TerminalFn (s/enum :move! :put! :size :clear! :refresh! :stop! :start! :get-event!))

(def TerminalSpec
  {TerminalFn s/Any})
