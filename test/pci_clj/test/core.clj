(ns pci-clj.test.core
  (:use [pci-clj.core] :reload)
  (:use [clojure.test]))

(deftest bisect-smoke
  (let [result (bisect [1 2 3 4 5 6] #(< % 3))]
      (is 
	        (= [1 2] (first result)) 
			      (= [3 4 5 6] (second result)))))

(deftest bisect-unordered-int
  (let [result (bisect [5 3 6 1 2 4] #(< % 3))]
      (is
	        (= [1 2] (first result))
			      (= [5 3 6 4] (second result)))))

(deftest bisect-unordered-int-unmatched
  (let [result (bisect [5 3 6 1 2 4] #(< % -3))]
      (is
	        (= [] (first result))
			      (= [5 3 6 1 2 4] (second result)))))

(deftest bisect-unordered-str
  (let [result (bisect ["abc" "def" "xyz" "blah"] #(= % "abc"))]
      (is
	        (= ["abc"] (first result))
			      (= ["def" "xyz" "blah"] (second result)))))


(deftest bisect-unordered-str-unmatched
  (let [result (bisect ["abc" "def" "xyz" "blah"] #(= % "java"))]
      (is
	        (= [] (first result))
			      (= ["abc" "def" "xyz" "blah"] (second result)))))

(deftest bisect-smoke
  (let [result (bisect [1 2 3 4 5 6] #(< % 3))]
      (is 
	        (= [1 2] (first result)) 
			      (= [3 4 5 6] (second result)))))

(deftest bisect-unordered-int
  (let [result (bisect [5 3 6 1 2 4] #(< % 3))]
      (is
	        (= [1 2] (first result))
			      (= [5 3 6 4] (second result)))))

(deftest bisect-unordered-int-unmatched
  (let [result (bisect [5 3 6 1 2 4] #(< % -3))]
      (is
	        (= [] (first result))
			      (= [5 3 6 1 2 4] (second result)))))

(deftest bisect-unordered-str
  (let [result (bisect ["abc" "def" "xyz" "blah"] #(= % "abc"))]
      (is
	        (= ["abc"] (first result))
			      (= ["def" "xyz" "blah"] (second result)))))


(deftest bisect-unordered-str-unmatched
  (let [result (bisect ["abc" "def" "xyz" "blah"] #(= % "java"))]
      (is
	        (= [] (first result))
			      (= ["abc" "def" "xyz" "blah"] (second result)))))

(deftest bisect-smoke
  (let [result (bisect [1 2 3 4 5 6] #(< % 3))]
      (is 
	        (= [1 2] (first result)) 
			      (= [3 4 5 6] (second result)))))

(deftest bisect-unordered-int
  (let [result (bisect [5 3 6 1 2 4] #(< % 3))]
      (is
	        (= [1 2] (first result))
			      (= [5 3 6 4] (second result)))))

(deftest bisect-unordered-int-unmatched
  (let [result (bisect [5 3 6 1 2 4] #(< % -3))]
      (is
	        (= [] (first result))
			      (= [5 3 6 1 2 4] (second result)))))

(deftest bisect-unordered-str
  (let [result (bisect ["abc" "def" "xyz" "blah"] #(= % "abc"))]
      (is
	        (= ["abc"] (first result))
			      (= ["def" "xyz" "blah"] (second result)))))


(deftest bisect-unordered-str-unmatched
  (let [result (bisect ["abc" "def" "xyz" "blah"] #(= % "java"))]
      (is
	        (= [] (first result))
			      (= ["abc" "def" "xyz" "blah"] (second result)))))

(deftest permutations-empty-coll
  (let [result (permutations [])]
    (is
      (= [] result))))

(deftest permutations-one-elem
  (let [result (permutations [1])]
    (is (= '(1 1) result))))

(deftest permutations-two-elems
  (let [result (permutations [1 2])]
    (is (= '((1 2) (2 1)) result))))

(deftest permutations-three-elems
  (let [result (permutations [1 2 3])]
    (is (= '((1 2) (1 3) (2 1) (2 3) (3 1) (3 2)) result))))

(deftest permutations-different-types
  (let [result (permutations [1 "a" 2.5])]
    (is (= '((1 "a") (1 2.5) ("a" 2.5) ("a" 1) (2.5 1) (2.5 "a") result)))))

