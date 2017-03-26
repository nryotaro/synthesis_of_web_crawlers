(defproject synthesis-crawlers "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha15"]
                 [org.jsoup/jsoup "1.10.2"]
                 [http-kit "2.2.0"]]

  :profiles {:dev {:plugins [[com.jakemccrary/lein-test-refresh "0.19.0"]]}})
