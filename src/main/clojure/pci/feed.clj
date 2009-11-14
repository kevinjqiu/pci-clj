(ns pci.feed
  (:use rome))


(defn- get-words
  [html-str]
  (apply
    (memfn toLowerCase)
    (filter
      #(not (empty? %))
      (.split #"[^A-Z^a-z]+" (.replaceAll html-str #"<[^>]+>" "")))))

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
          words (get-words (str (.getTitle entry) " " (.. entry getDescription getValue)))]
      (recur (count-words wc words) (rest entries)))))

(defn get-word-counts
  "Returns the title and dictionary of word counts for an RSS feed"
  [url]
  (get-word-counts-helper {} (.getEntries (parse-url url))))
