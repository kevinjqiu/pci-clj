(ns pci.feed
  (:use rome)
  (:import (java.io FileNotFoundException)))


(defn- get-words
  [html-str]
  (let [word-list (.split #"[^A-Z^a-z]+" (.replaceAll html-str "<[^>]+>" ""))]
    (map (memfn toLowerCase) (filter #(not (empty? %)) word-list))))

(defn count-words
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

(defn get-word-counts
  "Returns the title and dictionary of word counts for an RSS feed"
  [url]
  (println "analyzing: " url)
  (try
    (get-word-counts-helper {} (.getEntries (parse-url url)))
    (catch FileNotFoundException e (println "FileNotFound:" (.getMessage e)))))
