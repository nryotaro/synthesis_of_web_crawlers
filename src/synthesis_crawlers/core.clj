(ns synthesis-crawlers.core
  (:require [clojure.spec :as s]
            [org.httpkit.client :as http]
            [clojure.data :refer [diff]])
  (:import (org.jsoup Jsoup)
           (org.jsoup.select Elements)))

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


(s/def ::attr-extractor (s/map-of keyword? string?))
(s/def ::container-extractor string?)
(s/def ::complete-attr-extractor (s/map-of keyword? string?))
(s/def ::text string?)
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

                           (reduce #(assoc %1 (first %2) (.text (.select container (second %2)))) 
                                   {} 
                                   (get extractors container-extractor))
                           )]
    (println extracted-list)
    (reduce #(assoc %1 (first %2) (if (set? (second %2)) (second %2) (set (list (second %2))))) 
            {} 
            (apply (partial merge-with #(set %&)) extracted-list))))

;; If container is empty, it is interpreted as a node descriptor that extracts the root of the page. 
;; If container is empty and f is undefined for every attribute, we say that the data extractor is empty.
(s/fdef drop-undef-attr-extractors
        :args (s/cat :extractors (s/map-of keyword? #(or (string? %) (nil? %)))))
(defn drop-undef-attr-extractors
  [attr-extractors]
  (select-keys attr-extractors (filter #(get attr-extractors %)  (keys attr-extractors))))

(s/fdef empty-extractor?
        :args (s/cat :extractor (s/map-of ::container-extractor 
                                          (s/map-of keyword? #(or (string? %) (nil? %))))))

(s/def ::empty-extractor (s/map-of #(= "" %) (s/map-of keyword? nil?)))
(defn empty-extractor?
  "returns a non nil value iff the specifiled extractor is empty"
  [extractor]
  (s/valid? ::empty-extractor extractor))

;; split-with

;; todo filter nil filter
#_(defn extract-knowledge
    [attrs pages extractors knowledge]
    (for [page pages]
      (Jsoup/parse (get-page page))))

#_(doseq [elem (.select (.get (Jsoup/connect url)) "#html")](println (.html elem) ))
#_(println (.html (.select (.get (Jsoup/connect url)) "html > body > div[id=content]")))
