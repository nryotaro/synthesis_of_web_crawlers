(ns synthesis-crawlers.core
  (:require [clojure.spec :as s]
            [org.httpkit.client :as http]
            [clojure.data :refer [diff]])
  (:import (org.jsoup Jsoup)))

#_(s/def ::knowledge) 

(defn- diff? [a b]
  (let [a-b-c (diff a b)]
    (or (first a-b-c) (second a-b-c))))

(defn diff-knowledge? 
  [knowledge-a knowledge-b]
  (diff? knowledge-a knowledge-b))

(defn diff-extractors?
  [extractor-a extractor-b]
  (diff? extractor-a extractor-b))

(defn crawled?
  [extractor crawled-set]
  (crawled-set extractor))

(defn get-page
  "gets the specified web page and return its body"
  [url]
  #_(doseq [elem (.select (.get (Jsoup/connect url)) "#html")](println (.html elem) ))
  #_(println (.html (.select (.get (Jsoup/connect url)) "html > body > div[id=content]")))
  (:body @(http/get url)))
