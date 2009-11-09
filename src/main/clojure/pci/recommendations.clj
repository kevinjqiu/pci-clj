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

(defstruct recommendation :item :score)

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


(defn top-matches
  "Returns the best matches for person from the prefs dictionary.
  Number of results and similarity function are optional params"
  ([prefs, person, n, sim-fn]
    (take n (reverse (sort-by #(:score %) (for [other (disj (key-set prefs) person)]
    (struct-map similarity :person1 person :person2 other :score (sim-fn prefs person other)))))))
  ([prefs, person]
    (top-matches prefs, person, 5, sim-pearson)))

(defn- films-seen
  [prefs person]
  (key-set (get prefs person)))

(defn- yet-to-see
  "Returns the films yet to be seen by me"
  [prefs other me]
  (disj (films-seen prefs other) (films-seen prefs me)))

(defn-
  #^{:test (fn[]
              (assert (= (hash-map "Matthew" 1.5 "Mark" 1.0 "Luke" 2.0)
                         (collect [["Matthew" 1.5] ["Mark" 1.0] ["Luke" 2.0]])))
              (assert (= (hash-map "Matthew" 3.5 "Mark" 1.0 "Luke" 2.0)
                         (collect [["Matthew" 1.5] ["Mark" 1.0] ["Luke" 2.0] ["Matthew" 2.0]]))))}
  collect
  "Return a map of item:score from a list of (item,score)"
  [coll]
  (let [add-to-map (fn [the-map the-item]
                     (let [the-key (first the-item), the-val (last the-item)]
                       (if (contains? the-map the-key)
                         (assoc the-map the-key (+ (get the-map the-key) the-val))
                         (assoc the-map the-key the-val)))),
        helper (fn [ret-map lst]
                 (if (empty? lst)
                   ret-map
                   (recur (add-to-map ret-map (first lst)) (rest lst))))]
    (helper (hash-map) coll)))

(defn recommendation-list
  [prefs me sim-fn]
  (for [other (disj (key-set prefs) me)]
    (let [score (get (sim-fn prefs me other) me)
          films (yet-to-see prefs other me)]
      (for [film films] (list film (* ((get prefs other) film) score))))))

(defn get-recommendations
  "Gets recommendations for a person
  by using a weighted average of every other user's rankings"
  [prefs, person, sim-fn]
  nil)

