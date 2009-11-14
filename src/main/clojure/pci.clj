(ns pci
    (:use clojure.set pci.recommendations pci.feed)
    (:gen-class))

;(defn -main
;  []
;  (println (get-word-counts "http://feeds.feedburner.com/37signals/beMH")))

(println (get-word-counts "http://feeds.feedburner.com/37signals/beMH"))
