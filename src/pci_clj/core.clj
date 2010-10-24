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

