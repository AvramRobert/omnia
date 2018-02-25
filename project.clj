(defproject omnia "0.2.0"
  :description "A Clojure REPL for prototyping and experimenting"
  :url "https://github.com/AvramRobert/omnia"
  :license {:name "Apache-2.0 License"
            :url  "https://www.apache.org/licenses/LICENSE-2.0"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [clojure-lanterna "0.11.0"]
                 [org.clojure/core.match "0.3.0-alpha5"]
                 [fipp "0.6.12"]
                 [org.clojure/tools.nrepl "0.2.13"]
                 [halfling "1.1.0"]
                 [instaparse "1.4.8"]
                 [cider/cider-nrepl "0.15.1"]
                 [com.cemerick/pomegranate "1.0.0"]
                 [org.tcrawley/dynapath "1.0.0"]
                 [org.slf4j/slf4j-simple "1.7.22"]]

  :main ^:skip-aot omnia.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
