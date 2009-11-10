(ns pci.recommendations-test
  (:use clojure.test
        ;clojure.inspector
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
(def all-films @#'pci.recommendations/all-films)
(def recommendation-score @#'pci.recommendations/recommendation-score)

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
    (= #{"Lady in the Water" "Just My Luck" "The Night Listener"} (yet-to-score prefs "Toby")))
  (is
    (= #{} (yet-to-score prefs "Lisa Rose"))))


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
    (almost? 0.893405147 (sim-pearson prefs "Claudia Puig" "Toby")))
  (is
    (almost? 0.38 (sim-pearson prefs "Gene Seymour" "Toby") 0.01))
  (is
    (almost? 0.92 (sim-pearson prefs "Mick LaSalle" "Toby") 0.01))
  (is
    (almost? 0.66 (sim-pearson prefs "Jack Matthews" "Toby") 0.01)))

(deftest test-all-films
  (is
    (= #{"Lady in the Water" "Snakes on a Plane" "Just My Luck" "Superman Returns" "You, Me and Dupree" "The Night Listener"} (all-films prefs))))

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

(deftest test-get-recommendations-for-toby
  (let [rec (recommendations prefs "Toby" sim-pearson)
        item1 (first rec)
        item2 (second rec)
        item3 (first (rest (rest rec)))]
    (is (almost? 3.119201586 (last item1)))
    (is (= "The Night Listener" (first item1)))
    (is (almost? 3.002234730 (last item2)))
    (is (= "Lady in the Water" (first item2)))
    (is (almost? 2.530980737 (last item3)))
    (is (= "Just My Luck" (first item3)))))

(deftest test-recommendation-score
  (is (almost? 3.11 (recommendation-score prefs "Toby" "The Night Listener" sim-pearson) 0.01)))
