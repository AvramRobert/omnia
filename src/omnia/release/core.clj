(ns omnia.release.core
  (:require [schema.core :as s]
            [clojure.java.shell :as shell]
            [clojure.string :as string]
            [clojure.pprint :as pp]
            [halfling.task :as t]
            [omnia.config.defaults :as d]
            [omnia.util.misc :as m]
            [omnia.schema.release :refer [Releases OS ReleaseConfig]])
  (:import (java.io FileWriter)))

(s/def releases :- Releases
  {:linux   {:file-type     :sh
             :template      "resources/release/templates/linux/executable.sh"}

   :windows {:file-type     :bat
             :template      "resources/release/templates/windows/executable.bat"}

   :macOS   {:file-type     :sh
             :template      "resources/release/templates/mac/executable.sh"}})

(s/defn sh :- nil
  [& args :- [s/Any]]
  (let [{:keys [out exit err]} (apply shell/sh args)]
    (if (neg? exit)
      (throw (Exception. ^String err))
      (println out))))

(s/defn make-executable! :- s/Str
  [path :- s/Str
   file-name :- s/Str
   version :- s/Str]
  (-> path
      (slurp)
      (string/replace "%%FILENAME%%" file-name)
      (string/replace "%%VERSION%%" version)))

(s/defn mkdir :- nil
  [directory :- s/Str]
  (sh "mkdir" directory))

(s/defn cp :- nil
  [that :- s/Str there :- s/Str]
  (sh "cp" that there))

(s/defn zip-dir :- nil
  [to :- s/Str from :- s/Str]
  (sh "zip" "-r" to from))

(s/defn rm-dir :- nil
  [directory :- s/Str]
  (sh "rm" "-rf" directory))

(s/defn lein :- nil
  [command :- s/Str]
  (sh "lein" command))

(s/defn spit-formatted :- nil
  [file-name :- s/Str
   data :- s/Any]
  (pp/pprint data (FileWriter. ^String file-name)))

(s/defn release-for :- nil
  [os :- OS
   release-config :- ReleaseConfig]
  (let [system      (name os)
        version     (m/omnia-version)
        file-name   "omnia"
        _           (println "Releasing for: " system)
        target-ext  (-> release-config (:file-type) (name))
        target-jar  (str file-name ".jar")
        target-conf (str file-name ".edn")
        target-exec (str file-name "." target-ext)
        target-font "default_font.otf"
        target-dir  (format "%s-%s-%s" file-name version system)
        jar-file    (format "target/uberjar/%s-%s-standalone.jar" file-name version)
        executable  (-> release-config (:template) (make-executable! file-name version))
        font-file   "resources/release/Hasklig-Regular.otf"
        _           (mkdir (str "./" target-dir))
        _           (println "Creating release files..")
        _           (cp jar-file (str target-dir "/" target-jar))
        _           (cp font-file (str target-dir "/" target-font))
        _           (spit-formatted (str target-dir "/" target-conf) d/default-user-config)
        _           (spit (str target-dir "/" target-exec) executable)
        _           (println "Creating archive..")
        _           (zip-dir (str target-dir ".zip") target-dir)
        _           (println "Removing directory..")
        _           (rm-dir target-dir)
        _           (println (name os) ": done!")]))

(def release-task
  (t/do-tasks
    [_ (println "Running tests")
     _ (lein "test")
     _ (println "--------------")
     _ (println "Creating jar")
     _ (lein "uberjar")
     _ (println "--------------")
     _ (run! (fn [[os config]] (release-for os config)) releases)
     _ (rm-dir "target")
     :recover pp/pprint]))

(defn release! []
  (t/run release-task))
