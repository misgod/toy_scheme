(defproject toy-scheme "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/tools.trace "0.7.9"]
                 [instaparse "1.4.8"]
                 [vvvvalvalval/scope-capture "0.1.4"]
                 ]

  :injections [(require 'sc.api)]
  :main ^:skip-aot toy-scheme.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
