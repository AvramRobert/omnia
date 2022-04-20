(ns omnia.schema.release
  (:require [schema.core :as s]))

(def OS (s/enum :linux :macOS :windows))

(def Executable (s/enum :sh :bat))

(def ReleaseConfig
  {:file-type     Executable
   :template      s/Str
   :configuration s/Str})

(def Releases {OS ReleaseConfig})
