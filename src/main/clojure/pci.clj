(ns pci
    (:use clojure.set
          clojure.contrib.duck-streams
          clojure.inspector
          pci.recommendations
          pci.feed)
    (:gen-class))

;(defn -main
;  []
;  (println (get-word-counts "http://feeds.feedburner.com/37signals/beMH")))

(def feeds (line-seq (reader "feedlist.txt")))

(inspect (fetch-word-counts (take 5 feeds)))
