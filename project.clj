(defproject omnia "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [clojure-lanterna "0.10.0"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [fipp "0.6.8"]]
  :main ^:skip-aot omnia.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
