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

;; Sequential
(defn aggregate
  [feeds wc]
  (if (empty? feeds)
    wc
    (recur
      (rest feeds)
      (merge-with + wc (get-word-counts (first feeds))))))

;(println (aggregate feeds {}))

;; Use agents
(defn spawn-agents
  [feeds agents]
  (if (empty? feeds)
    agents
    (let [agt (agent {})
          feed (first feeds)]
      (recur
        (rest feeds)
        (conj
          agents
          (send agt (fn [agent-state] (get-word-counts feed))))))))

(println (let [agents (spawn-agents feeds '())]
  (apply await agents)
  ; Why the following doesn't work?
  ;(reduce #(merge-with + %) (map #(@%) agents))))
  (loop [rest-agents agents ret {}]
    (if (empty? rest-agents)
      ret
      (recur (rest rest-agents) (merge-with + ret @(first rest-agents)))))))





