(ns pci
    (:use clojure.set
          clojure.contrib.duck-streams
          pci.recommendations
          pci.feed)
    (:gen-class))

;(defn -main
;  []
;  (println (get-word-counts "http://feeds.feedburner.com/37signals/beMH")))

(def feeds (take 20 (line-seq (reader "src/main/clojure/feedlist.txt"))))

(defn aggregate
  [feeds wc]
  (if (empty? feeds)
    wc
    (recur
      (rest feeds)
      (merge-with + wc (get-word-counts (first feeds))))))

(println (aggregate feeds {}))

