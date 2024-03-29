(defproject qwirkle "0.1.0-SNAPSHOT"
  :description "Implementation of the table-top tile-laying game Qwirkle"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [midje "1.6.3"]
                 [org.clojure/test.check "0.7.0"]]
  :profiles {:dev {:plugins [[lein-marginalia "0.8.0"]
                             [cider/cider-nrepl "0.8.2"]
                             [lein-cljfmt "0.1.10"]]}})
