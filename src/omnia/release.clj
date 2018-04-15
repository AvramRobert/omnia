(ns omnia.release
  (:require [omnia.config :as c]
            [omnia.more :as m]
            [clojure.java.shell :as s]
            [halfling.task :as t]
            [omnia.format :as f]))

(defn sh [& args]
  (-> (t/task (apply s/sh args))
      (t/then (fn [{:keys [exit out]}]
                (when (not (empty? out)) (println out))
                (when (not (zero? exit)) (t/failure (format "%s failed! Release stopped." (apply str args))))))))

(defn executable [version file-name]
  (format
    "#!/bin/bash

     SOURCE=\"${BASH_SOURCE[0]}\"
     while [ -h \"$SOURCE\" ]
     do
       DIR=\"$( cd -P \"$( dirname \"$SOURCE\" )\" && pwd )\"
       SOURCE=\"$(readlink \"$SOURCE\")\"
       [[ $SOURCE != /* ]] && SOURCE=\"$DIR/$SOURCE\"
     done

     DIR=\"$( cd -P \"$( dirname \"$SOURCE\" )\" && pwd )\"

     JAR=\"$DIR/%s.jar\"

     echo \"Starting Omnia %s..\"

     java -jar $JAR \"path=$DIR\"" file-name version))

(def prepare-jar
  (-> (t/task    (println "Running tests.."))
      (t/then-do (sh "lein" "test"))
      (t/then-do (println "Creating uberjar.."))
      (t/then-do (sh "lein" "uberjar"))))

(defn release-for [os]
  (t/do-tasks
    [version     (m/omnia-version)
     system      (name os)
     title       (format "omnia-%s" version)
     sa-title    (format "%s-standalone" title)
     directory   (format "%s-%s/" title system)
     config-file (format "%s/omnia.edn" directory)
     exec-file   (format "%s/omnia" directory)
     config      (-> (c/config-for os) (str) (f/format-str))
     exec        (executable version title)
     _     (println (format "Creating release files for %s.." system))
     _     (sh "mkdir" directory)
     _     (sh "cp" (format "target/uberjar/%s.jar" sa-title) directory)
     _     (sh "mv" (format "%s%s.jar" directory sa-title) (format "%s%s.jar" directory title))
     _     (spit config-file config)
     _     (spit exec-file exec)
     _     (println "Creating tar..")
     _     (sh "tar" "-cvf" (format "%s-%s.tar" title system) (format "./%s" directory))
     _     (println "Removing directory..")
     _     (sh "rm" "-rf" directory)
     _     (println "Done!")]))


(defn release []
  (-> prepare-jar
      (t/then-do (->> [c/linux c/macOS] (mapv release-for) (t/sequenced)))
      (t/recover (fn [{:keys [message]}]
                   (println (format "Release failed with: %s" message))
                   (System/exit -1)))
      (t/run)))
