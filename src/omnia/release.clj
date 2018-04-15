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

(defn release []
  (->
    (t/do-tasks
      [version     (m/omnia-version)
       title       (format "omnia-%s" version)
       sa-title    (format "%s-standalone" title)
       directory   (format "%s/" title)
       config-file (format "%s/omnia.edn" directory)
       exec-file   (format "%s/omnia" directory)
       config      (-> c/default-config (str) (f/format-str))
       exec        (executable version title)
       _     (println "Running tests..")
       _     (sh "lein" "test")
       _     (println "Creating uberjar..")
       _     (sh "lein" "uberjar")
       _     (println "Creating release files..")
       _     (sh "mkdir" title)
       _     (sh "mv" (format "target/uberjar/%s.jar" sa-title) directory)
       _     (sh "mv" (format "%s/%s.jar" title sa-title) (format "%s/%s.jar" title title))
       _     (spit config-file config)
       _     (spit exec-file exec)
       _     (println "Creating tar..")
       _     (sh "tar" "-cvf" (format "%s.tar" title) (format "./%s" directory))
       _     (println "Removing directory..")
       _     (sh "rm" "-rf" directory)
       _     (println "Done!")
       _     (System/exit 1)])
    (t/recover (fn [{:keys [message]}]
                 (println (format "Release failed with: %s" message))
                 (System/exit -1)))
    (t/run)))
