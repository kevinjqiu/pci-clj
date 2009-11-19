(ns pci
    (:use clojure.set
          clojure.contrib.duck-streams
          pci.recommendations
          pci.feed)
    (:gen-class))

;(defn -main
;  []
;  (println (get-word-counts "http://feeds.feedburner.com/37signals/beMH")))

(def feeds (line-seq (reader "feedlist.txt")))

(println (agent-aggregate (take 5 feeds)))
