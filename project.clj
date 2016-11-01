(defproject calliope/calliope-vdom "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.8.0"
                  :scope "provided"]
                 [org.clojure/clojurescript "1.9.229"
                  :scope "provided"]
                 [calliope/calliope-dom "0.1.0-SNAPSHOT"]]

  :plugins [[lein-cljsbuild "1.1.4"]
            [speclj "3.3.1"]
            [lein-doo "0.1.7"]]

  :min-lein-version "2.5.0"

  :clean-targets ^{:protect false}
  [:target-path
   [:cljsbuild :builds :test :compiler :output-dir]
   [:cljsbuild :builds :test :compiler :output-to]]

  :source-paths ["src"]
  :test-paths ["spec"]
  :resource-paths ["target/cljsbuild"]

  :cljsbuild
  {:builds {:test
            {:source-paths ["src" "spec"]
             :compiler {:output-to "target/test.js"
                        :optimizations :whitespace
                        :pretty-print true}}
            }
   :test-commands {"unit" ["phantomjs" "runners/speclj" "target/test.js"]}
   }

  :doo {:build "test"}

  :profiles {:dev {:dependencies [[speclj "3.3.1"]]}}
  )
