(ns pci-clj.test.decisiontree
  (:use [pci-clj.decisiontree] :reload)
  (:use [clojure.test]))

(def *test-data* [["slashdot" "USA" "yes" 18 "None"] 
        ["google" "France" "yes" 23 "Premium"] 
        ["digg" "USA" "yes" 24 "Basic"] 
        ["kiwitobes" "France" "yes" 23 "Basic"] 
        ["google" "UK" "no" 21 "Premium"] 
        ["(direct)" "New Zealand" "no" 12 "None"] 
        ["(direct)" "UK" "no" 21 "Basic"] 
        ["google" "USA" "no" 24 "Premium"] 
        ["slashdot" "France" "yes" 19 "None"] 
        ["digg" "USA" "no" 18 "None"] 
        ["google" "UK" "no" 18 "None"] 
        ["kiwitobes" "UK" "no" 19 "None"] 
        ["digg" "New Zealand" "yes" 12 "Basic"] 
        ["slashdot" "UK" "no" 21 "None"] 
        ["google" "UK" "yes" 18 "Basic"] 
        ["kiwitobes" "France" "yes" 19 "Basic"]])

(deftest divideset-number
  (is 
    (= 
      [[["google" "France" "yes" 23 "Premium"]
        ["digg" "USA" "yes" 24 "Basic"]
        ["kiwitobes" "France" "yes" 23 "Basic"]
        ["google" "UK" "no" 21 "Premium"]
        ["(direct)" "UK" "no" 21 "Basic"]
        ["google" "USA" "no" 24 "Premium"]
        ["slashdot" "UK" "no" 21 "None"]]
       [["slashdot" "USA" "yes" 18 "None"]
        ["(direct)" "New Zealand" "no" 12 "None"]
        ["slashdot" "France" "yes" 19 "None"]
        ["digg" "USA" "no" 18 "None"] 
        ["google" "UK" "no" 18 "None"] 
        ["kiwitobes" "UK" "no" 19 "None"] 
        ["digg" "New Zealand" "yes" 12 "Basic"] 
        ["google" "UK" "yes" 18 "Basic"] 
        ["kiwitobes" "France" "yes" 19 "Basic"]]
       ] 
      (divideset *test-data* 3 20))))

(deftest divideset-nominal
  (let [result (divideset *test-data* 2 "yes")
        truth-coll (first result)
        false-coll (second result)]
    (is
      (= (count truth-coll) 8))
    (is
      (= (count false-coll) 8))
    (is
      (= (count *test-data*) (+ (count truth-coll) (count false-coll))))
    (is
      (= (count (filter #(= (nth % 2) "no") truth-coll)) 0))
    (is
      (= (count (filter #(= (nth % 2) "yes") false-coll)) 0))))


(deftest uniquecounts-empty
  (is (= {} (uniquecounts []))))

(deftest uniquecounts-one-element
  (is (= {"basic" 1} (uniquecounts [["fact1" "fact2" "basic"]]))))

(deftest uniquecounts-multiple-elements
  (let [result (uniquecounts [["f1" "f2" "basic"]
                              ["f3" "f4" "premium"]
                              ["f5" "f6" "premium"]
                              ["f7" "f8" "none"]
                              ["f9" "fa" "basic"]])]
    (is (= 2 (get result "basic")))
    (is (= 2 (get result "premium")))
    (is (= 1 (get result "none")))))


(deftest gini-impurity-test
  (is (= 0.6328125 (gini-impurity *test-data*))))



