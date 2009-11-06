(ns pci.recommendations
  (:use clojure.set))

(def critics
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
      "You, Me and Dupree" 3.0
      "The Night Listener" 3.5)
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


(defstruct similarity :person1 :person2 :score)

(defn- key-set
  "Return the keyset of a map"
  [param]
  (set (keys param)))

(defn- get-score
  "Return the score of the specified item for the specified person"
  [prefs person item]
  (get (get prefs person) item))

(defn- shared-items
  [prefs person1 person2]
  (intersection
    (key-set (get prefs person1))
    (key-set (get prefs person2))))

(defn- sum-of-squares
  [prefs shared-items person1 person2]
  (apply +
    (for [item shared-items]
      (Math/pow (- (get-score prefs person1 item) (get-score prefs person2 item)) 2))))

(defn sim-distance
  "Returns a distance-based similarity score for p1 and p2"
  [prefs p1 p2]
  (let [si (shared-items prefs p1 p2)]
    (if (empty? si)
      0
      (let [sos (sum-of-squares prefs si p1 p2)]
        (/ 1 (+ 1 sos))))))

(defn sim-pearson
  "Returns the Pearson correlation coefficient for p1 and p2"
  [prefs p1 p2]
  (let [si (shared-items prefs p1 p2)
        size (count si)
        sum #(apply + (for [item si] (get-score prefs % item)))
        sum-sq #(apply + (for [item si] (Math/pow (get-score prefs % item) 2)))
        pSum (apply + (for [item si] (* (get-score prefs p1 item) (get-score prefs p2 item))))
        n (- pSum (/ (* (sum p1) (sum p2)) size))
        den #(- %1 (/ (Math/pow %2 2) size))]

    (if (empty? si)
      0
      (/ n (Math/sqrt (* (den (sum-sq p1) (sum p1)) (den (sum-sq p2) (sum p2))))))))
