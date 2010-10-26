(ns pci-clj.decisiontree
  (:use [pci-clj.core]))

(defstruct criterion :column :value)
(defstruct tree-node :criterion :results :tb :fb)
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
  "Return a map of value and its count at column ``col-idx``"
  (reduce #(merge-with + %1 {%2 1}) {} (map #(nth % col-idx) rows)))

(defn- calculate-gain [current-score set1 set2 rows scoref]
  "Calculate the information gain of the subset over rows
  given the scoring function ``scoref``"
  (let [p (/ (count set1) (count rows))]
    (- (- current-score (* p (scoref set1))) (* (- 1 p) (scoref set2)))))


(defn buildtree [rows scoref]
  "Build the decision tree using the given data and scoring function"
  (cond 
    (= 0 (count rows)) 
      (struct-map tree-node)
    :else 
      (let [current-gain (struct-map gain :value (scoref rows) :criteria nil :sets nil)
            column-count (dec (first rows))
            column-values-set (map #(values-in-column rows %) (range 0 column-count))]
        ())))





