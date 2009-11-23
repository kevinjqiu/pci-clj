(ns pci.util
  (:use clojure.contrib.math))

(defn square
  "Returns the square of the given number"
  [x]
  (* x x))


(defn almost?
  "Returns true if the difference between the two numbers
  is smaller than the precision"
  ([x y threshold]
    (< (abs (- x y)) threshold))
  ([x y]
    (almost? x y 0.00001)))

(defn pnr
  "Short for Print-n-Return"
  [x]
  (println x)
  x)

(defn pearson
  "Calculate the pearson similarity score for vector 1 and 2"
  [v1 v2]
  (let [length (count v1)
        sum1 (apply + v1) sum2 (apply + v2)
        sum1Sq (apply + (map * v1 v1)) sum2Sq (apply + (map * v2 v2))
        pSum (apply + (map * v1 v2))
        n (- pSum (/ (* sum1 sum2) length))
        density (sqrt (* (- sum1Sq (/ (square sum1) length)) (- sum2Sq (/ (square sum2) length))))]
    (if (zero? density) 0 (- 1.0 (/ n density)))))





