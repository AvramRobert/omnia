(ns omnia.release.core
  (:require [clojure.java.shell :as shell]
            [schema.core :as s]
            [halfling.task :as t]
            [clojure.string :as string]
            [omnia.config.core :refer [UserConfig read-user-config!]]
            [omnia.util.misc :refer [omnia-version]]))

(def OS (s/enum :linux :macOS :windows))

(def Executable (s/enum :sh :bat))

(def ReleaseConfig
  {:file-type     Executable
   :template      s/Str
   :configuration s/Str})

(def Releases {OS ReleaseConfig})

(s/def releases :- Releases
  {:linux   {:file-type     :sh
             :template      "resources/release/templates/linux/executable.sh"
             :configuration "resources/release/templates/linux/config.edn"}

   :windows {:file-type     :bat
             :template      "resources/release/templates/windows/executable.bat"
             :configuration "resources/release/templates/windows/config.edn"}

   :macOS   {:file-type     :sh
             :template      "resources/release/templates/mac/executable.sh"
             :configuration "resources/release/templates/mac/config.edn"}})

(defn- sh [& args]
  (let [{:keys [out exit err]} (apply shell/sh args)]
    (if (neg? exit)
      (throw (Exception. err))
      (println out))))

(defn- make-executable! [path file-name version]
  (-> path
      (slurp)
      (string/replace "%%FILENAME%%" file-name)
      (string/replace "%%VERSION%%" version)))

(defn mkdir [directory]
  (sh "mkdir" directory))

(defn cp [that there]
  (sh "cp" that there))

(defn zip-dir [to from]
  (sh "zip" "-r" to from))

(defn rm-dir [directory]
  (sh "rm" "-rf" directory))

(defn lein [command]
  (sh "lein" command))


(defn release-for [[os release]]
  (let [system      (name os)
        version     (omnia-version)
        file-name   "omnia"
        _           (println "Releasing for: " system)
        target-ext  (-> release (:file-type) (name))
        target-jar  (str file-name ".jar")
        target-conf (str file-name ".edn")
        target-exec (str file-name "." target-ext)
        target-font "default_font.otf"
        target-dir  (format "%s-%s-%s" file-name version system)

        jar-file    (format "target/uberjar/%s-%s-standalone.jar" file-name version)
        executable  (-> release (:template) (make-executable! file-name version))
        config      (-> release (:configuration) (read-user-config!))
        font-file   "resources/release/Hasklig-Regular.otf"
        _           (mkdir (str "./" target-dir))
        _           (println "Creating release files..")
        _           (cp jar-file (str target-dir "/" target-jar))
        _           (cp font-file (str target-dir "/" target-font))
        _           (spit (str target-dir "/" target-conf) config)
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
     _ (run! release-for releases)
     _ (rm-dir "target")
     :recover #(do (.printStackTrace %) (System/exit -1))]))

(defn release! []
  (t/run release-task))