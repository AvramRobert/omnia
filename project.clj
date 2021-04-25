(defproject omnia "1.0.0"
  :description "A Clojure REPL for prototyping and experimenting"
  :url "https://github.com/AvramRobert/omnia"
  :license {:name "Apache-2.0 License"
            :url  "https://www.apache.org/licenses/LICENSE-2.0"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [com.googlecode.lanterna/lanterna "3.0.3"]
                 [org.clojure/core.match "0.3.0-alpha5"]
                 [fipp "0.6.23"]
                 [prismatic/schema "1.1.12"]
                 [halfling "1.3.0"]
                 [instaparse "1.4.10"]
                 [cider/cider-nrepl "0.25.11"]
                 [clj-commons/pomegranate "1.2.1"]
                 [org.tcrawley/dynapath "1.1.0"]
                 [org.slf4j/slf4j-simple "1.7.30"]]
  :main ^:skip-aot omnia.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}
             :dev {:dependencies [[criterium "0.4.4"]]}}
  :aliases  {"release" ["run" "-m" "omnia.release.core/release"]})
