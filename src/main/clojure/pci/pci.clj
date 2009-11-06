(ns pci.pci
	(:use clojure.set pci.recommendations))

(defn key-set
	"Return the keyset of a map"
	[param]
	(set (keys param)))

(defn shared-items
	[prefs person1 person2]
	(intersection
 		(key-set (get prefs person1))
		(key-set (get prefs person2))))

(defn sum-of-squares
	[prefs shared-items person1 person2]
	(apply + 
		(for [item shared-items] 
			(Math/pow (- (get (get prefs person1) item) (get (get prefs person2) item))	2))))

(defn sim-distance
	"Returns a distance-based similarity score for person1 and person2"
	[prefs person1 person2]
	(let [si (shared-items prefs person1 person2)]
		(if (empty? si) 
			0 
			(let [sos (sum-of-squares prefs si person1 person2)]
				(/ 1 (+ 1 (Math/sqrt sos)))))))

(defstruct similarity :person1 :person2 :score)

(defn main
	[]
	(let [similarities (sort-by #(:score %) (for [person1 (key-set critics) person2 (disj (key-set critics) person1)] 
											(struct-map similarity :person1 person1 :person2 person2 :score (sim-distance critics person1 person2))))]
		(doseq [item similarities]
			(println item))))
			
		
		
		
		

	
