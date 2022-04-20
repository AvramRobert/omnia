(ns omnia.schema.context
  (:require [omnia.schema.nrepl :as n]
            [omnia.schema.hud :as h]
            [omnia.schema.text :as t]
            [omnia.schema.config :as c]
            [omnia.schema.render :as r]))

(def Context
  {:repl          n/REPLClient
   :config        c/Config
   :render        r/RenderingStrategy
   :previous-hud  h/Hud
   :persisted-hud h/Hud
   :preview-hud   h/Hud
   :input-area    t/Seeker
   :suggestions   h/Hud
   :documentation h/Hud
   :signatures    h/Hud
   :highlights    r/Highlights
   :garbage       r/Highlights})