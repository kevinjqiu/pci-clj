(ns pci.commons
  (:use clojure.test))

(deftest bisect-smoke
  (is 
    (let [result (bisect [1 2 3 4 5 6] #(% < 3))] 
      (= [1 2] (first result)) 
      (= [3 4 5 6] (second result)))))
