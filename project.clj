(defproject capr-nvnc "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [seesaw "1.5.0"]
                 [clojure-opennlp "0.5.0"]
                 [metosin/jsonista "0.3.1"]]
  :main ^:skip-aot capr-nvnc.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
