(ns pci.pci
    (:use clojure.set pci.recommendations))


(defn main
    []
    (let [similarities (sort-by #(:score %) (for [person1 (key-set critics) person2 (disj (key-set critics) person1)]
                                            (struct-map similarity :person1 person1 :person2 person2 :score (sim-pearson critics person1 person2))))]
        (doseq [item similarities]
            (println item))))

