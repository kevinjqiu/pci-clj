(ns rome
  (:import (java.net URL)
           (com.sun.syndication.feed.module Module)
           (com.sun.syndication.feed.synd SyndEntry SyndFeed)
           (com.sun.syndication.io SyndFeedInput XmlReader)))

(defn #^SyndFeed parse-url
  [#^String feed-url]
  (.build (SyndFeedInput.) (XmlReader. (URL. feed-url))))



