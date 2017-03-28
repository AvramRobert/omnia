(ns omnia.hud
  (:gen-class))


;; The idea is to avoid reprocessing past seekers and just redisplay them in an already processed version.
;; Or. Is it possible to refresh only part of the screen? That would be a good solution.