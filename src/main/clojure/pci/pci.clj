(ns pci.pci
    (:use clojure.set pci.recommendations))


(defn main
    []
    (doseq [item (top-matches critics "Toby" 2 sim-pearson)]
            (println item)))
