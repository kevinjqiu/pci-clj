(ns pci.bicluster
)

(defstruct bicluster :left :right :vec :id :distance)

(defn average-vector
  "return the average vector of the two bicluster"
  [cluster1 cluster2]
  (let [vec1 (:vec cluster1) vec2 (:vec cluster2)]
    (map #(/ (+ (%1 %2)) 2) vec1 vec2)))

(defn distance
  "return the distance of two biclusters
  given the distance calculating function"
  [cluster1 cluster2 distance-fn]
  (distance-fn (:vec cluster1) (:vec cluster2)))
