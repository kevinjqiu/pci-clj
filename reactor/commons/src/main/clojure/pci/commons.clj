(ns pci.commons)

(defmacro with-private-fns [[ns fns] & tests]
  "Refers private fns from ns and runs tests in context.
   courtesy of chouser"
  `(let ~(reduce #(conj %1 %2 `(ns-resolve '~ns '~%2)) [] fns)
     ~@tests))

(defn bisect [coll pred]
  "Bisect the collection based on the predicate.
  Return a 2-tuple, whose first item is the collection of o's
  with (true? pred o), and the second (false? pred o)
  "
  [(filter coll pred) (filter coll #(not pred %))]
)
