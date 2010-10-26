(ns pci-clj.decisiontree
  (:use [pci-clj.core]))

(defstruct tree-node :criteria :results :tb :fb)
(defstruct gain :value :criteria :sets)

(defn divideset [rows column target-value]
  "Divides a set on a specific column.
  If the column is numeric, the split is based on
  whether rows[column]>=target-value.
  If the column is a nominal value, the split is
  simply based on equality, e.g., rows[column]==target-value"
  (let [split-fn (if (number? target-value)
                   #(>= (nth % column) target-value)
                   #(= (nth % column) target-value))]
    (bisect rows split-fn)))

(defn uniquecounts [rows]
  "Return a map of results and their counts.
  The result is assumed to be the last column of a row"
  ; Read Clojure's doc on ``reduce`` method,
  ; if the coll is empty, f cannot accept arguments;
  ; if the coll has only 1 item, f won't be called.
  ; Therefore, there's the need to separate those two cases
  (let [len (count rows)]
    (cond (= len 0) {}
          (= len 1) {(last (first rows)) 1}
          ; originally, I had merge-with (fn [v1 v2] (+ v1 v2)),
          ; but it's simply the `+` operation...
          :else (reduce #(merge-with + %1 {(last %2) 1}) {} rows))))

(defn gini-impurity [rows]
  "The Gini Impurity of the collection of rows.
  The Gini Impurity is defined as the probability that 
  randomly placed item will be in the wrong category"
  (let [total (count rows)
        result-counts (uniquecounts rows)
        results (keys result-counts)
        perms (permutations results)
        prob-fn #(/ (get result-counts %) total)]
    (apply + (map #(* (prob-fn (first %)) (prob-fn (last %))) perms))))

(defn entropy [rows]
  "Entropy is the amount of disorder in a set - how mixed a set is"
  (let [result-counts (uniquecounts rows)
        results (keys result-counts)
        total (count rows)]
    (- (apply + (map (fn [result] (let [p (/ (get result-counts result) total)] (* p (log2 p)))) results)))))

(defn- values-in-column [rows col-idx]
  "Return a list of unique values in column ``col-idx``"
  (set (map #(nth % col-idx) rows)))

(defn- calculate-gain [current-score set1 set2 rows scoref]
  "Calculate the information gain of the subset over rows
  given the scoring function ``scoref``"
  ;(println "score" current-score)
  ;(println "set1" set1)
  ;(println "set2" set2)
  ;(println "rows" rows)
  (let [p (/ (count set1) (count rows))]
    (- current-score (* p (scoref set1)) (* (- 1 p) (scoref set2)))))

(defn- best-gain-in-column-values [col-idx col-values rows current-best-gain scoref current-score]
  (cond (= (count col-values) 0) current-best-gain
        :else (let [value (first col-values) 
                   dividedset (divideset rows col-idx value)
                   set1 (first dividedset)
                   set2 (last dividedset)
                   gain-val (calculate-gain current-score set1 set2 rows scoref)]
                ;(println "rows:" rows)
                ;(println "col-idx:" col-idx)
                (println "value:" value)
                (println "gain-val:" gain-val)
                (if (and (> gain-val (:value current-best-gain)) (not (empty? set1)) (not (empty? set2)))
                  (recur col-idx (rest col-values) rows (struct-map gain :value gain-val :criteria '(col-idx value) :sets '(set1 set2)) scoref current-score)
                  (recur col-idx (rest col-values) rows current-best-gain scoref current-score)))))

(defn- best-gain [col-idxs rows scoref current-best-gain current-score]
  "Calculate the best information gain."
  (cond (= (count col-idxs) 0) current-best-gain
        :else (let [col-idx (first col-idxs)
                   col-values (values-in-column rows col-idx)
                   best-gain (best-gain-in-column-values col-idx col-values rows current-best-gain scoref current-score)]
                (println "best-gain:" best-gain)
                (if (> (:value best-gain) (:value current-best-gain))
                  (recur (rest col-idxs) rows scoref best-gain current-score)
                  (recur (rest col-idxs) rows scoref current-best-gain current-score)))))

(defn buildtree [rows scoref]
  "Build the decision tree using the given data and scoring function"
  (cond 
    (= 0 (count rows)) 
      (struct-map tree-node)
    :else 
      (let [col-idxs (range 0 (dec (count (first rows))))
            current-score (scoref rows)
            current-best-gain (best-gain
                                col-idxs
                                rows
                                scoref
                                (struct-map gain :value current-score :criteria nil :sets nil)
                                current-score)]
        (if (> (:value current-best-gain) 0)
          (let [sets (:sets current-best-gain)
                true-branch (buildtree (first sets) scoref)
                false-branch (buildtree (last sets) scoref)]
            (struct-map tree-node :criteria (:criteria current-best-gain) :tb true-branch :fb false-branch))
          (struct-map tree-node :results (uniquecounts rows))))))

