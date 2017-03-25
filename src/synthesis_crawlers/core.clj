(ns synthesis-crawlers.core
  (:require [clojure.spec :as s]
            [clojure.data :refer [diff]]))

#_(s/def ::knowledge) 

(defn- diff? [a b]
  (let [a-b-c (diff a b)]
    (or (first a) (second b))))

(defn diff-knowledge? 
  [knowledge-a knowledge-b]
  (let [a-b-common (diff knowledge-a knowledge-b)]
    (or (first a-b-common) (second a-b-common))))

(defn diff-extractors?
  [extractor-a extractor-b]
  (let [a-b-common (diff extractor-a extractor-b)]
    (or (first a-b-common) (second a-b-common))))

