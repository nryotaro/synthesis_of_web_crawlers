(ns synthesis-crawlers.core
  (:require [clojure.spec :as s]
            [clojure.data :refer [diff]]))

#_(s/def ::knowledge) 

(defn diff-knowledge? 
  [knowledge-a knowledge-b]
  (diff knowledge-a knowledge-b))
