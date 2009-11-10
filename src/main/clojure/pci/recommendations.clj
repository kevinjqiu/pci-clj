(ns pci.recommendations
  (:use clojure.set
        pci.util))

(defstruct similarity :person1 :person2 :score)

(defstruct recommendation :item :score)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convenience methods for getting data out of the prefs map
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- critics
  "Return a set of critics in prefs"
  [prefs]
  (set (keys prefs)))

(defn- other-critics
  "Return a set of critics in prefs other than me"
  [prefs me]
  (disj (critics prefs) me))

(defn- films
  "Return a set of films reviewed by the critic"
  [prefs critic]
  (set (keys (get prefs critic))))

(defn- score
  "Return the score of the specified item for the specified person"
  ([prefs person item default]
    (let [ret (get (get prefs person) item)]
      (if (nil? ret) default ret)))
  ([prefs person item]
    (score prefs person item nil)))

(defn- shared-items
  "Returns a set of items"
  [prefs person1 person2]
  (intersection
    (critics (get prefs person1))
    (critics (get prefs person2))))

(defn- all-films
  "Return a set of all films in prefs"
  [prefs]
  (let [helper (fn [ret prefs]
                 (if (empty? prefs)
                   ret
                   (recur (union ret (set (keys (val (first prefs))))) (rest prefs))))]
    (helper (hash-set) prefs)))

(defn- yet-to-score
  "Returns the films yet to be scored by me"
  [prefs me]
  (difference (all-films prefs) (films prefs me)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Euclidean Distance Score
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- sum-of-square-diffs
  [prefs si p1 p2]
  (apply +
    (for [item si]
      (square (- (score prefs p1 item) (score prefs p2 item))))))

(defn sim-distance
  "Returns a distance-based similarity score for p1 and p2"
  [prefs p1 p2]
  (let [si (shared-items prefs p1 p2)]
    (if (empty? si)
      0
      (let [sos (sum-of-square-diffs prefs si p1 p2)]
        (/ 1 (+ 1 sos))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pearson Correlation Score
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sim-pearson
  "Returns the Pearson correlation coefficient for p1 and p2"
  [prefs p1 p2]
  (let [si (shared-items prefs p1 p2)
        size (count si)
        sum #(apply + (for [item si] (score prefs % item)))
        sum-sq #(apply + (for [item si] (square (score prefs % item))))
        pSum (apply + (for [item si] (* (score prefs p1 item) (score prefs p2 item))))
        n (- pSum (/ (* (sum p1) (sum p2)) size))
        den #(- %1 (/ (square %2) size))]

    (if (empty? si)
      0
      (/ n (Math/sqrt (* (den (sum-sq p1) (sum p1)) (den (sum-sq p2) (sum p2))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ranking critics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn top-matches
  "Returns the best matches for person from the prefs dictionary.
  Number of results and similarity function are optional params"
  ([prefs, person, n, sim-fn]
    (take n (reverse
              (sort-by #(:score %)
                (for [other (other-critics prefs person)]
                     (struct-map similarity :person1 person :person2 other :score (sim-fn prefs person other)))))))
  ([prefs, person]
    (top-matches prefs, person, 5, sim-pearson)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get recommendations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- scored-film?
  "Returns true if the person has scored the film"
  [prefs person film]
  (contains? (films prefs person) film))

(defn- recommendation-score
  "Return the recommendation score for the film"
  [prefs me film sim-fn]
  (/
    (apply + (map (fn [critic] (* (score prefs critic film 0) (sim-fn prefs critic me))) (other-critics prefs me)))
    (apply + (map (fn [critic] (sim-fn prefs critic me)) (filter #(scored-film? prefs % film) (other-critics prefs me))))))

(defn recommendations
  "Gets recommendations for a person
  by using a weighted average of every other user's rankings"
  [prefs, person, sim-fn]
  (reverse (sort-by (fn [item] (last item))
    (map #(list % (recommendation-score prefs person % sim-fn)) (yet-to-score prefs person)))))
