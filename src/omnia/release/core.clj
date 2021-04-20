(ns omnia.release.core
  (:require [clojure.java.shell :refer [sh]]
            [clojure.java.io :as io]
            [halfling.task :as t]
            [clojure.string :as s]
            [omnia.config.core :refer [default-user-config]]
            [omnia.util.misc :refer [omnia-version]]))

(def release-configuration
  {:linux   {:template      "executable.sh"
             :configuration default-user-config}
   :macOS   {:template      "executable.sh"
             :configuration default-user-config}
   :windows {:template      "executable.bat"
             :configuration default-user-config}})

(defn- make-executable [path file-name version]
  (-> path
      (io/resource)
      (s/replace "%%FILENAME%%" file-name)
      (s/replace "%%VERSION%%" version)))

(def prepare-jar
  (-> (t/task    (println "Running tests.."))
      (t/then-do (sh "lein" "test"))
      (t/then-do (println "Creating uberjar.."))
      (t/then-do (sh "lein" "uberjar"))))

(defn release-for [os]
  (let [system      (name os)
        _           (println "Preparing release for:" system)
        version     (omnia-version)
        title       (format "omnia-%s-%s" version system)
        sa-title    (format "%s-standalone" title)
        directory   (format "%s/" title)
        config-file (format "%s/omnia.edn" directory)
        exec-file   (format "%s/omnia" directory)
        _           (println "Creating release files..")
        exec        (-> release-configuration (get os) (:template) (make-executable title version))
        config      (-> release-configuration (get os) (:configuration))]
    (sh "mkdir" directory)
    (sh "cp" (format "target/uberjar/%s.jar" sa-title) directory)
    (sh "mv" (format "%s%s.jar" directory sa-title) (format "%s%s.jar" directory title))
    (spit config-file config)
    (spit exec-file exec)
    (println "Creating archive..")
    (sh "zip" "-r" (format "%s.zip" title) (format "./%s" directory))
    (println "Removing directory..")
    (sh "rm" "-rf" directory)
    (println "Done!")))

(defn release []
  (-> prepare-jar
      (t/then-do (->> release-configuration (keys) (run! release-for)))
      (t/recover (fn [{:keys [message trace]}]
                   (println (format "Release failed with: %s" message))
                   (run! (comp println str) trace)
                   (System/exit -1)))
      (t/run)))