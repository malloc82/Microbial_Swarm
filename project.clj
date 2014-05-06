(defproject swarm_project "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure      "1.6.0"]
                 [clj-time                 "0.7.0"]
                 [incanter/incanter-core   "1.5.6-SNAPSHOT"]
                 [incanter/incanter-charts "1.5.6-SNAPSHOT"]
                 [incanter/incanter-io     "1.5.6-SNAPSHOT"]
                 [net.mikera/core.matrix "0.23.0"]]
  :resource-paths ["resources/table.csv"
                   "resources/goog.csv"])
