(defproject omnia "0.1.0"
  :description "A Clojure REPL for prototyping and experimenting"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [clojure-lanterna "0.10.0"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [fipp "0.6.8"]
                 [org.clojure/tools.nrepl "0.2.12"]
                 [halfling "1.0.2"]
                 [com.cemerick/pomegranate "0.3.1"]
                 [instaparse "1.4.5"]
                 [ritz/ritz-nrepl-middleware "0.7.0"]]

  :main ^:skip-aot omnia.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}
             :dev {:dependencies [[criterium "0.4.4"]]}})
