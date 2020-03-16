(defproject crux-uikit "0.0.1"
  :description ""
  :url "https://github.com/crux-labs/crux-uikit"
  :license {:name "MIT"}
  :source-paths ["src"]
  :profiles {:uberjar {:aot :all}}
  :deploy-repositories [["releases" :clojars]
                        ["snapshots" :clojars]])
