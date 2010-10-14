(ns pci.commons)

(defmacro with-private-fns [[ns fns] & tests]
  "refers private fns from ns and runs tests in context.
   courtesy of chouser"
  `(let ~(reduce #(conj %1 %2 `(ns-resolve '~ns '~%2)) [] fns)
     ~@tests))

