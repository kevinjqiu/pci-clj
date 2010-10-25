(ns pci-clj.decisiontree
  (:use [pci-clj.core]))

(defstruct criterion :column :value)
(defstruct tree-node :criterion :results :tb :fb)

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





