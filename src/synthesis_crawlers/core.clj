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

(defn build-selector [attribute container-expr attr-expr]
  {attribute 
   (cond
     (empty? container-expr) attr-expr
     :else (str container-expr " > " attr-expr))})


;; If container is empty, it is interpreted as a node descriptor that extracts the root of the page. 
;; If container is empty and f is undefined for every attribute, we say that the data extractor is empty.
(s/fdef drop-undef-attr-extractors
        :args (s/cat :extractors (s/map-of keyword? #(or (string? %) (nil? %)))))
(defn drop-undef-attr-extractors
  [attr-extractors]
  (select-keys attr-extractors (filter #(get attr-extractors %)  (keys attr-extractors))))

(s/fdef build-selectors
        :ars (s/cat :extractors ::extractors))
(defn build-selectors
  [extractors]
  (let [built (for [[container attr-extractors] extractors
                    [attr attr-expr] (drop-undef-attr-extractors attr-extractors)]
                (build-selector attr container attr-expr))]
    (reduce #(assoc %1 (first %2) (conj (get %1 (first %2) #{}) (second %2))) 
            {} 
            (map first (map seq built)))))

(s/fdef extract
        :args (s/cat :text ::text :extractors ::extractors)
        :ret (s/map-of keyword? set?))
(defn extract
  [text extractors]
  (let [root (Jsoup/parse text)
        attr-selectors (build-selectors extractors)]
    (into {}
          (for [[attr exprs] attr-selectors]
            [attr (set (filter #(not (empty? %)) (map #(.text (.select root %)) exprs)))]))))


(s/fdef empty-extractor?
        :args (s/cat :extractor (s/map-of ::container-extractor 
                                          (s/map-of keyword? #(or (string? %) (nil? %))))))
(s/def ::empty-extractor (s/map-of #(= "" %) (s/map-of keyword? nil?)))
(defn empty-extractor?
  "returns a non nil value iff the specifiled extractor is empty"
  [extractor]
  (s/valid? ::empty-extractor extractor))

;; split-with
