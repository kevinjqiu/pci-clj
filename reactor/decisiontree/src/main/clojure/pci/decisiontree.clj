(ns pci.decisiontree
	:use (pci.commons))

(defstruct decision-node
  :criteria ; a tuple (col,value) which represents the criteria column[col]==value or column[col]>value if value is numeric 
  :results  ; 
  :tb       ; the true branch 
  :fb       ; the false branch
)

(defn- divide-set [data col-idx value]
  "divides the set of data on column `col-idx` given the `value`,
   return a tuple of two sets of `data` divided according to `value` on `col-idx`"
  (let [split-func (if (number? value)
                     #(>= % (nth col-idx data))
                     #(not (= % (nth col-idx data)))]
    (bisect data split-func))))
