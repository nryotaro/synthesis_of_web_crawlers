(ns synthesis-crawlers.core
  (:require [clojure.spec :as s]
            [clojure.data :refer [diff]]))

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
