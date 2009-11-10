(ns pci.recommendations
  (:use clojure.set clojure.inspector pci.util))

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
  [prefs person item]
  (get (get prefs person) item))

(defn- shared-items
  "Returns a set of items"
  [prefs person1 person2]
  (intersection
    (critics (get prefs person1))
    (critics (get prefs person2))))

(defn- yet-to-score
  "Returns the films yet to be scored by me"
  [prefs other me]
  (difference (films prefs other) (films prefs me)))

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
(defn- collect
  "Return a map of film:(total,sim) from a list of (film,total,sim)"
  [coll]
  (let [add-to-map (fn [the-map the-item]
                     (let [film (first the-item) total (second the-item) sim (last the-item)]
                       (if (contains? the-map film)
                         (let [prev-total (first (get the-map film)) prev-sim (last (get the-map film))]
                           (assoc the-map film (list (+ total prev-total) (+ sim prev-sim))))
                         (assoc the-map film (list total sim)))))
        helper (fn [ret-map lst]
                 (if (empty? lst)
                   ret-map
                   (recur (add-to-map ret-map (first lst)) (rest lst))))]
    (helper (hash-map) coll)))

(defn- recommendation-list
  [prefs me sim-fn]
  (let [flatten (fn [ret lst] (if (empty? lst) ret (recur (concat ret (first lst)) (rest lst))))]
    (flatten '() (for [other (other-critics prefs me)]
      (let [sim (sim-fn prefs me other) films (yet-to-score prefs other me)]
        (for [film films] (list film (* ((get prefs other) film) sim) sim)))))))



(defn recommendations
  "Gets recommendations for a person
  by using a weighted average of every other user's rankings"
  [prefs, person, sim-fn]
  (let [calc-score (fn [ret-map rec-map]
                     (if (empty? rec-map)
                      ret-map
                      (let [item (first rec-map) k (key item) v (val item) total (first v) sim (last v)]
                        (recur (assoc ret-map k (/ total sim)) (rest rec-map)))))]
  (reverse (sort-by #(val %) (calc-score (hash-map) (collect (recommendation-list prefs person sim-fn)))))))
