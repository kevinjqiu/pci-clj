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



