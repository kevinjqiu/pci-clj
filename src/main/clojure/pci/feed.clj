(ns pci.feed
  (:use rome clojure.set clojure.inspector pci.bicluster pci.util)
  (:import (java.io FileNotFoundException)))


(defn- get-words
  [html-str]
  (let [word-list (.split #"[^A-Z^a-z]+" (.replaceAll html-str "<[^>]+>" ""))]
    (map (memfn toLowerCase) (filter #(not (empty? %)) word-list))))

(defn- count-words
  [wc words]
  (if (empty? words)
    wc
    (let [word (first words)]
      (recur (assoc wc word (inc (get wc word 0))) (rest words)))))

(defn- get-word-counts-helper
  [wc entries]
  (if (empty? entries)
    wc
    (let [entry (first entries)
          words (get-words
                  (str (.getTitle entry) " "
                       (if (nil? (.getDescription entry))
                         "" (.. entry getDescription getValue))))]
      (recur (count-words wc words) (rest entries)))))

(defn- get-word-counts
  "Returns the title and dictionary of word counts for an RSS feed"
  [url]
  (println "analyzing: " url)
  (try
    (get-word-counts-helper {} (.getEntries (parse-url url)))
    (catch Exception e (println (.toString e)))))

(defn- create-agent
  [feed]
  (send
    (agent [feed])
    (fn [state] (conj state (get-word-counts feed)))))

(defn- spawn-agents
  [feeds agents]
  (if (empty? feeds)
    agents
    (let [feed (first feeds)]
      (recur
        (rest feeds)
        (conj agents (create-agent feed))))))


(defn- fetch-word-counts
  "Fetch the word-count map for each of the feeds."
  [feeds]
  (let [agents (spawn-agents feeds '())]
    (apply await agents)
    (map deref agents)))

(defstruct feed-map :words :feed-map)

(defn- map-word-counts
  "feed-word-count is a map entry"
  [word-list feed-word-count retval]
  (let [feed (first feed-word-count)
        word-counts (last feed-word-count)]
    (assoc
      retval
      feed
      (map #(if (contains? word-counts %) (get word-counts %) 0) word-list))))

(defn- create-feed-map
  "feed-word-counts is a map of feed and its word count
  the return is a map of feed and a list of counts
  corresponding to word-list"
  [word-list feed-word-counts]
  (letfn [(helper [word-list interim-feed-word-counts retval]
            (if (empty? interim-feed-word-counts)
              retval
              (recur word-list (rest interim-feed-word-counts) (map-word-counts word-list (first interim-feed-word-counts) retval))))]
    (helper word-list feed-word-counts {})))

(defn- set-to-list
  "Is there a library function for this?"
  [my-set]
  (apply conj '() my-set))

(defn create-feed-map-struct
  [feeds]
  (let [feed-word-count (fetch-word-counts feeds)
        word-list
          (set-to-list
            (apply union (map #(set (keys (last %))) feed-word-count)))]
    (struct-map feed-map :words word-list :feed-map (create-feed-map word-list feed-word-count))))

(defn- create-init-clusters
  "create a list of initial clusters from feeds"
  [feed-map]
  (for [feed-entry (:feed-map feed-map)]
    (struct-map bicluster
      :left nil
      :right nil
      :id (key feed-entry)
      :vec (val feed-entry)
      :distance 0)))

(defn create-feed-cluster
  "create a feed cluster from a feed word map (of type struct-map feed-map)"
  [feed-map]
  (create-init-clusters feed-map))

(defn cluster-distances
  [cluster-list]
  (loop [i 0 cluster-distances []]
    (if (= i (count cluster-list))
      cluster-distances
      (recur
        (inc i)
        (conj
          cluster-distances
          (for [cluster1 (nth cluster-list i) cluster2 (rest (last (split-at i cluster-list)))]
            [(pearson (:vec cluster1) (:vec cluster2)) [cluster1 cluster2]]))))))
