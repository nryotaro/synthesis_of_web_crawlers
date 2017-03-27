(ns synthesis-crawlers.core
  (:require [clojure.spec :as s]
            [org.httpkit.client :as http]
            [clojure.data :refer [diff]])
  (:import (org.jsoup Jsoup)))

(defn- diff? [a b]
  (let [a-b-c (diff a b)] (or (first a-b-c) (second a-b-c))))

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
  (:body @(http/get url)))


(s/def ::data-extractor (s/map-of keyword? string?))
(s/def ::container-extractor string?)

(s/def ::text string?)
(s/def ::container-extractor string?)
(s/def ::attr-extractor (s/map-of keyword? string?))
(s/def ::extractors (s/map-of ::container-extractor ::attr-extractor))
(s/fdef extract
        :args (s/cat :text ::text :extractors ::extractors)
        :ret (s/map-of keyword? set?))
(defn extract
  [text extractors]
  (let [root (Jsoup/parse text)
        extracted-list (for [container-extractor (keys extractors)
                             container (.select root container-extractor)]; container-extractor: container-expr
                         ; coll: map entries[attr: expr]
                         (reduce #(assoc %1 (first %2) (.text (.select container (second %2)))) {} (get extractors container-extractor)))]
    (reduce #(assoc %1 (first %2) (if (set? (second %2)) (second %2) (set (list (second %2))))) 
            {} 
            (apply (partial merge-with #(set %&)) extracted-list))))

;; todo filter nil filter
(defn extract-knowledge
  [attrs pages extractors knowledge]
  (for [page pages]
    (Jsoup/parse (get-page page))))
(doseq [elem (.select (.get (Jsoup/connect url)) "#html")](println (.html elem) ))
(println (.html (.select (.get (Jsoup/connect url)) "html > body > div[id=content]")))
