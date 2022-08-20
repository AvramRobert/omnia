(defproject omnia "1.0.0"
  :description "A Clojure REPL for prototyping and experimenting"
  :url "https://github.com/AvramRobert/omnia"
  :license {:name "Apache-2.0 License"
            :url  "https://www.apache.org/licenses/LICENSE-2.0"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [com.googlecode.lanterna/lanterna "3.1.1"]
                 [org.clojure/core.match "1.0.0"]
                 [fipp "0.6.26"]
                 [prismatic/schema "1.4.0"]
                 [halfling "1.3.1"]
                 [instaparse "1.4.12"]
                 [cider/cider-nrepl "0.28.5"]
                 [clj-commons/pomegranate "1.2.1"]
                 [org.tcrawley/dynapath "1.1.0"]
                 [org.slf4j/slf4j-simple "1.7.36"]
                 [nrepl "0.9.0"]]
  :main ^:skip-aot omnia.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}
             :dev     {:dependencies [[criterium "0.4.6"]
                                      [org.clojure/java.data "1.0.95"]]}}
  :aliases  {"release" ["run" "-m" "omnia.release.core/release!"]})
