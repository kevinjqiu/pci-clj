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
      "Superman Returns" 4.0)))

(def critics @#'pci.recommendations/critics)
(def other-critics @#'pci.recommendations/other-critics)
(def films @#'pci.recommendations/films)
(def score @#'pci.recommendations/score)
(def shared-items @#'pci.recommendations/shared-items)
(def yet-to-score @#'pci.recommendations/yet-to-score)
(def collect @#'pci.recommendations/collect)

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

(deftest test-get-other-critics
  (is
    (= #{"Lisa Rose" "Gene Seymour" "Michael Phillips" "Claudia Puig" "Mick LaSalle" "Jack Matthews"} (other-critics prefs "Toby")))
  (is
    (= #{"Lisa Rose" "Gene Seymour" "Claudia Puig" "Mick LaSalle" "Jack Matthews" "Toby"} (other-critics prefs "Michael Phillips"))))

(deftest test-shared-items
  (is
    (= #{"Lady in the Water" "Snakes on a Plane" "Just My Luck" "Superman Returns" "You, Me and Dupree" "The Night Listener"} (shared-items prefs "Lisa Rose" "Gene Seymour")))
  (is
    (= #{"Snakes on a Plane" "You, Me and Dupree" "Superman Returns"} (shared-items prefs "Lisa Rose" "Toby"))))

(deftest test-yet-to-score
  (is
    (= #{"Lady in the Water" "Just My Luck" "The Night Listener"} (yet-to-score prefs "Lisa Rose" "Toby")))
  (is
    (= #{} (yet-to-score prefs "Toby" "Lisa Rose"))))


(deftest test-sim-distances
  (is
    (almost? 0.148148148 (sim-distance prefs "Lisa Rose" "Gene Seymour"))))

(deftest test-sim-pearson
  (is
    (almost? 0.396059017191 (sim-pearson prefs "Lisa Rose" "Gene Seymour")))
  (is
    (almost? 0.991240707 (sim-pearson prefs "Lisa Rose" "Toby")))
  (is
    (almost? 0.924473451 (sim-pearson prefs "Mick LaSalle" "Toby")))
  (is
    (almost? 0.893405147 (sim-pearson prefs "Claudia Puig" "Toby"))))

(deftest test-top-matches
  (let [tm (top-matches prefs "Toby" 3 sim-pearson)]
    (is (= 3 (count tm)))
    (let [m1 (first tm) m2 (second tm) m3 (last tm)]
      (is (almost? 0.991240707 (:score m1)))
      (is (= "Lisa Rose" (:person2 m1)))
      (is (almost? 0.924473451 (:score m2)))
      (is (= "Mick LaSalle" (:person2 m2)))
      (is (almost? 0.893405147 (:score m3)))
      (is (= "Claudia Puig" (:person2 m3))))))

(deftest test-collect
  (is
    (= (hash-map "Matthew" 1.5 "Mark" 1.0 "Luke" 2.0)
       (collect [["Matthew" 1.5] ["Mark" 1.0] ["Luke" 2.0]])))
  (is
    (= (hash-map "Matthew" 3.5 "Mark" 1.0 "Luke" 2.0)
       (collect [["Matthew" 1.5] ["Mark" 1.0] ["Luke" 2.0] ["Matthew" 2.0]]))))
