(ns omnia.schema.release
  (:require [schema.core :as s]))

(def OS (s/enum :linux :macOS :windows))

(def Executable (s/enum :sh :bat))

(def ReleaseConfig
  {:file-type     Executable
   :file-name     s/Str
   :config-file   s/Str
   :jar-file      s/Str
   :template      s/Str})

(def Releases {OS ReleaseConfig})
