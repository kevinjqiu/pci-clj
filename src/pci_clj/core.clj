(ns pci-clj.core)

(defmacro with-private-fns [[ns fns] & tests]
  "Refers private fns from ns and runs tests in context.
  courtesy of chouser"
  `(let ~(reduce #(conj %1 %2 `(ns-resolve '~ns '~%2)) [] fns)
    ~@tests))

(defn bisect [coll pred]
  "Bisects the collection based on the predicate.
  Returns a 2-tuple, whose first item is the collection of obj's
  with (true? pred obj) and the second (false? pred obj)"
  [(filter pred coll) (filter #(not (pred %)) coll)])

(defn permutations [coll]
  "Generate a collection of permutations of elements in coll.
  The coll must be ordered (if it has to be a set, use ordered-set"
  (cond (= (count coll) 0) '()
        (= (count coll) 1) (list (list (first coll) (first coll)))
        :else
          (for [e1 coll e2 (remove #(= % e1) coll)]
           (list e1 e2))))

(defn log2 [x]
  (/ (Math/log x) (Math/log 2)))

