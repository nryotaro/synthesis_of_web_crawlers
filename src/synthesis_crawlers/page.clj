(ns synthesis-crawlers.page
  (:require [clojure.spec :as s]
            [clojure.string :refer [starts-with? ends-with?]]
            [org.httpkit.client :as http]
            [clojure.data :refer [diff]])
  (:import (org.jsoup Jsoup)
           (org.jsoup.select Elements)
           (org.jsoup.nodes Element)
           (info.debatty.java.stringsimilarity Jaccard)))

(defn get-page
  "gets the specified web page and return its body"
  [url]
  (:body @(http/get url)))

(s/fdef extract-links
        :args (s/cat :root-url #(not (ends-with? % "/")) :html string?))
(defn extract-links
  [root-url html]
  (let [root (Jsoup/parse html)]
    (map #(let [href (.attr % "href")] 
            (cond
              (starts-with? href "/")  (str root-url href)
              :else href)) 
         (filter #(.hasAttr % "href") (.getElementsByTag root "a")))))

(defn fetch-urls
  [urls root-url pattern crawled-pages]
  (when (seq urls)
    (let [url (first urls)
          matched-links  (filter some? 
                                 (map #(let [found (re-seq pattern %)] 
                                         (when found (first found))) 
                                      (extract-links root-url (get-page url))))
          uncrawled-links (filter #(not (crawled-pages %)) matched-links)]
      (lazy-seq 
        (cons url 
              (fetch-urls (rest (into urls uncrawled-links)) root-url pattern (conj crawled-pages url)))))))


