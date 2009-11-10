(ns pci.recommendations
  (:use clojure.set pci.util))

(defstruct similarity :person1 :person2 :score)

(defstruct recommendation :item :score)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convenience methods for getting data out of the prefs map
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn critics
  "Return a set of critics in prefs"
  [prefs]
  (set (keys prefs)))

(defn other-critics
  "Return a set of critics in prefs other than me"
  [prefs me]
  (disj (critics prefs) me))

(defn films
  "Return a set of films reviewed by the critic"
  [prefs critic]
  (set (keys (get prefs critic))))

(defn score
  "Return the score of the specified item for the specified person"
  [prefs person item]
  (get (get prefs person) item))

(defn shared-items
  "Returns a set of items"
  [prefs person1 person2]
  (intersection
    (critics (get prefs person1))
    (critics (get prefs person2))))

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

(defn- films-seen
  [prefs person]
  (critics (get prefs person)))

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
  (for [other (disj (critics prefs) me)]
    (let [score (get (sim-fn prefs me other) me)
          films (yet-to-see prefs other me)]
      (for [film films] (list film (* ((get prefs other) film) score))))))

(defn get-recommendations
  "Gets recommendations for a person
  by using a weighted average of every other user's rankings"
  [prefs, person, sim-fn]
  nil)

