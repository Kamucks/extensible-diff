(defproject extensible-diff "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [net.cgrand/xforms "0.19.2"]
                 [cheshire "5.10.0"]
                 [org.clojure/core.cache "0.8.2"]
                 [org.clojure/core.match "0.3.0"]
                 [org.clojure/core.unify "0.5.7"]
                 [uncomplicate/fluokitten "0.9.1"]
                 [org.clojure/algo.generic "0.1.3"]
                 [com.rpl/specter "1.1.1"]
                 [org.clojure/tools.logging "0.3.1"]
                 [prismatic/plumbing "0.5.5"]
                 [mvxcvi/puget "1.2.1"]
                 [org.clojure/tools.cli "1.0.194"]
                 [cloroutine "10"]
                 [com.velisco/tagged "0.5.0"]
                 [org.clojure/data.xml "0.0.8"]
                 [net.sf.cssbox/pdf2dom "1.2"]
                 [diffit "1.0.0"]]
  :main ^:skip-aot extensible-diff.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
