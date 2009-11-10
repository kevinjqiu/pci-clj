(ns pci.recommendations-test
  (:use clojure.test
        pci.recommendations
        pci.util))

(def prefs
  (hash-map
    "Lisa Rose"
    (hash-map
      "Lady in the Water" 2.5
      "Snakes on a Plane" 3.5
      "Just My Luck" 3.0
      "Superman Returns" 3.5
      "You, Me and Dupree" 2.5
      "The Night Listener" 3.0)
    "Gene Seymour"
    (hash-map
      "Lady in the Water" 3.0
      "Snakes on a Plane" 3.5
      "Just My Luck" 1.5
      "Superman Returns" 5.0
      "You, Me and Dupree" 3.5
      "The Night Listener" 3.0)
    "Michael Phillips"
    (hash-map
      "Lady in the Water" 2.5
      "Snakes on a Plane" 3.0
      "Superman Returns" 3.5
      "The Night Listener" 4.0)
    "Claudia Puig"
    (hash-map
      "Snakes on a Plane" 3.5
      "Just My Luck" 3.0
      "Superman Returns" 4.0
      "You, Me and Dupree" 2.5
      "The Night Listener" 4.5)
    "Mick LaSalle"
    (hash-map
      "Lady in the Water" 3.0
      "Snakes on a Plane" 4.0
      "Just My Luck" 2.0
      "Superman Returns" 3.0
      "You, Me and Dupree" 2.0
      "The Night Listener" 3.0)
    "Jack Matthews"
    (hash-map
      "Lady in the Water" 3.0
      "Snakes on a Plane" 4.0
      "Superman Returns" 5.0
      "You, Me and Dupree" 3.5
      "The Night Listener" 3.0)
    "Toby"
    (hash-map
      "Snakes on a Plane" 4.5
      "You, Me and Dupree" 1.0
      "The Night Listener" 4.0)))

;(def key-set @#'pci.recommendations/key-set)

(deftest test-key-set
  (is
    (= #{"Lisa Rose" "Gene Seymour" "Michael Phillips" "Claudia Puig" "Mick LaSalle" "Jack Matthews" "Toby"}
    (critics prefs))))

(deftest test-get-films
  (is
    (= #{"Lady in the Water" "Snakes on a Plane" "Just My Luck" "Superman Returns" "You, Me and Dupree" "The Night Listener"})
    (films prefs "Lisa Rose"))
  (is
    (= #{"Snakes on a Plane" "You, Me and Dupree" "The Night Listener"})
    (films prefs "Toby"))
  (is
    (= #{})
    (films prefs "Ebenezer")))

(deftest test-get-score
  (is
    (= 4.5 (score prefs "Toby" "Snakes on a Plane")))
  (is
    (= 3.0 (score prefs "Mick LaSalle" "Superman Returns"))))


(deftest test-shared-items
  (is
    (= #{"Lady in the Water" "Snakes on a Plane" "Just My Luck" "Superman Returns" "You, Me and Dupree" "The Night Listener"} (shared-items prefs "Lisa Rose" "Gene Seymour")))
  (is
    (= #{"Snakes on a Plane" "You, Me and Dupree" "The Night Listener"} (shared-items prefs "Lisa Rose" "Toby"))))

(deftest test-sim-distances
  (is
    (almost? 0.148148148 (sim-distance prefs "Lisa Rose" "Gene Seymour") 9)))



(run-tests)

