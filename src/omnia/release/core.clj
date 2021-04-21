(ns omnia.release.core
  (:require [clojure.java.shell :as shell]
            [halfling.task :as t]
            [clojure.string :as s]
            [omnia.config.core :refer [default-user-config]]
            [omnia.util.misc :refer [omnia-version]]))

(def release-configuration
  {:linux   {:template      "release/templates/executable.sh"
             :file-type     "sh"
             :configuration default-user-config}
   :macOS   {:template      "release/templates/executable.sh"
             :file-type     "sh"
             :configuration default-user-config}
   ;:windows {:template      "release/templates/executable.bat"
   ;          :file-type     "bat"
   ;          :configuration default-user-config}
   ;
   })

(defn- sh [& args]
  (-> (t/task (apply shell/sh args))
      (t/then (fn [{:keys [out exit err]}]
                (if (neg? exit)
                  (t/failure err)
                  (println out))))))

(defn- make-executable [path jar-filename version]
  (-> path
      (slurp)
      (s/replace "%%FILENAME%%" jar-filename)
      (s/replace "%%VERSION%%" version)))

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

(defn release-for [os]
  (t/do-tasks
    [system      (name os)
     version     (omnia-version)
     name        "omnia"
     release     (get release-configuration os)
     _           (println "Releasing for: " system)

     target-jar  (str name ".jar")
     target-conf (str name ".edn")
     target-exec (str name "." (:file-type release))
     target-font "default_font.otf"
     target-dir  (format "%s-%s-%s" name version system)

     jar-file    (format "target/uberjar/%s-%s-standalone.jar" name version)
     executable  (-> release (:template) (make-executable name version))
     config      (-> release-configuration (get os) (:configuration))
     font-file   "release/Hasklig-Regular.otf"
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
     _           (println "Done!")]))

(def release-task
  (t/do-tasks
    [_ (println "Running tests")
     _ (lein "test")
     _ (println "--------------")
     _ (println "Creating jar")
     _ (lein "uberjar")
     _ (println "--------------")
     _ (->> release-configuration (keys) (mapv release-for) (t/sequenced))
     :recover #(do (.printStackTrace %) (System/exit -1))]))

(defn release []
  (t/run release-task))