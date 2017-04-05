(ns synthesis-crawlers.core
  (:require [clojure.spec :as s]
            [synthesis-crawlers.page :as page]
            [clojure.string :refer [starts-with? ends-with?]]
            [org.httpkit.client :as http]
            [clojure.data :refer [diff]])
  (:import (org.jsoup Jsoup)
           (org.jsoup.select Elements)
           (org.jsoup.nodes Element)
           (info.debatty.java.stringsimilarity Jaccard)))

(s/def ::attribute keyword?)
(s/def ::attributes (s/coll-of ::attribute))
(s/def ::url-pattern #(instance? java.util.regex.Pattern %))
(s/def ::pages (s/map-of string? string?))
(s/def ::sites (s/map-of string? (s/keys :req-un [::url-pattern ::pages])))
(s/def ::knowledge (s/coll-of string?))
(s/def ::attr-knowledge (s/map-of  ::attribute ::knowledge))
(s/def ::attr-extractor (s/map-of keyword? (s/or :complete string? :empty nil?)))
(s/def ::container-extractor string?)
(s/def ::complete-attr-extractor (s/map-of keyword? string?))
(s/def ::extractor (s/map-of ::container-extractor ::attr-extractor))
(s/def ::site-extractor (s/map-of string? ::extractor))
(s/def ::text string?)
(s/def ::complete-extractor (s/map-of ::container-extractor
                                      ::complete-attr-extractor))
(s/def ::attributed-nodes-in-pages (s/map-of string? 
                                             (s/map-of keyword? 
                                                       (s/coll-of #(instance? Element %)))))

(s/fdef similarity :args (s/cat :a string? :b string?))
(defn similarity
  "returns true value iff the specified extractor is incomplete"

  [a b]
 (.similarity (Jaccard.) a b))

(defn- diff? [a b]
  (let [a-b-c (diff a b)] (or (first a-b-c) (second a-b-c))))

(defn diff-knowledge? 
  [knowledge-a knowledge-b]
  (diff? knowledge-a knowledge-b))

(defn diff-extractors?
  [extractor-a extractor-b]
  (diff? extractor-a extractor-b))

(s/fdef uncrawled-extractors
        :args (s/cat :site-extractors ::site-extractor 
                     :crawled-extractors ::site-extractor)
        :ret ::site-extractor)
(defn uncrawled-extractors
  "Returns uncrawled site extractors."
  [site-extractors crawled-extractors]
  (first (diff site-extractors crawled-extractors)))

(defn crawled?
  [extractor crawled-set]
  (crawled-set extractor))


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
        :ars (s/cat :extractors ::extractor))
(defn build-selectors
  [extractors]
  (let [built (for [[container attr-extractors] extractors
                    [attr attr-expr] (drop-undef-attr-extractors attr-extractors)]
                (build-selector attr container attr-expr))]
    (reduce #(assoc %1 (first %2) (conj (get %1 (first %2) #{}) (second %2))) 
            {} 
            (map first (map seq built)))))

(s/fdef extract
        :args (s/cat :text (s/or :text ::text :empty nil?) 
                     :extractors ::extractor)
        :ret (s/map-of keyword? set?))
(defn extract
  "tries extracting values with expressions"
  [text extractors]
  (when text
    (let [root (Jsoup/parse text)
          attr-selectors (build-selectors extractors)]
      (into {}
            (for [[attr exprs] attr-selectors]
              [attr (set (filter #(not (empty? %)) (map #(.text (.select root %)) exprs)))])))))

(s/fdef extract-knowledge
        :args (s/cat :sites ::sites 
                     :site-extractor ::site-extractor
                     :crawled-extractors ::site-extractor)
        :ret ::attr-knowledge)
(defn extract-knowledge
  "extracts knwoledge from the specified site"
  [sites site-extractor crawled-extractors]
  (let [extracted (for [[site extractor] (uncrawled-extractors site-extractor crawled-extractors)
                        text (vals (:pages (sites site)))] 
                    (extract text extractor))]
    (apply (partial merge-with into) extracted)))


(s/fdef empty-extractor?
        :args (s/cat :extractor (s/map-of ::container-extractor 
                                          (s/map-of keyword? #(or (string? %) (nil? %))))))
(s/def ::empty-extractor (s/map-of #(= "" %) (s/map-of keyword? nil?)))
(defn empty-extractor?
  "returns a non nil value iff the specifiled extractor is empty"
  [extractor]
  (s/valid? ::empty-extractor extractor))

;; split-with

(defn incomplete-extractors?
  [extractor]
  (not (s/valid? ::complete-extractor extractor)))

(s/fdef matched-knowledge
        :args (s/cat :text ::text :knowledge ::knowledge)
        :ret ::knowledge)
(defn matched-knowledge
  [text knowledge]
  (filter #(>= (similarity text %) 0.5 )
          knowledge))

(s/fdef find-attr-nodes
        :args (s/cat :nodes #(instance? Elements %) :knowledge ::knowledge)
        :ret #(instance? Elements %))
(defn find-attr-nodes 
  "returns the elements which contain text similar to the specified knowledge"
  [nodes knowledge]
  (filter #(not-empty (matched-knowledge (.text %) knowledge)) nodes))

(s/fdef reach?
        :args (s/cat :a #(instance? Element %) :b #(instance? Element %)))
(defn reach?
  "returns true iff b is reachable from a"
  [a b]
  (->> (filter #(= % b) (.getAllElements a)) empty? not))


(s/fdef find-nodes-in-page
  :args (s/cat :pages ::pages :attr-knowledge ::attr-knowledge))
(defn find-nodes-in-page
  [pages attr-knowledge]
  (reduce (fn [acc [url text]]
            (assoc acc 
                   url 
                   (reduce 
                     (fn [k [attr knowledge-set]] (assoc k 
                                                         attr 
                                                         (find-attr-nodes 
                                                           (.getAllElements 
                                                             (Jsoup/parse text)) 
                                                           knowledge-set))) 
                     {} 
                     attr-knowledge))) 
          {} 
          pages))

(defn reachable-elements
  [elem]
  (conj (into [] (.parents elem)) elem))

(s/fdef find-reachable-attrs
        :args (s/cat :attr-nodes (s/map-of string? 
                                           (s/map-of keyword? 
                                                     (s/coll-of #(instance? Element %))))))
(defn find-reachable-attrs 
  [attr-nodes] 
  (let [result (for [[url attr-nodes] attr-nodes
                     [attr nodes] attr-nodes
                     node (set (flatten (map reachable-elements nodes)))] 
                 [url [node attr]])]
    (reduce (fn [acc [url [node attr]]] 
              (let [node-attr (get acc url {})
                    attrs (get node-attr node #{})]
                (assoc acc url (assoc node-attr node (conj attrs attr)))))
            {} 
            result)))

(s/fdef find-reachable-attrs
        :args (s/cat :attr-nodes (s/map-of string? 
                                           (s/map-of keyword? 
                                                     (s/coll-of #(instance? Element %))))))

(defn find-support-nodes
  [url-attr-nodes]
  (let [result (for [[url attr-nodes] url-attr-nodes
                     [attr nodes] attr-nodes
                     node (set (flatten (map reachable-elements nodes)))]
                 [url [node nodes]])]
    (reduce (fn [acc [url [node nodes]]] 
              (let [node-nodes (get acc url {})
                    node-set (get node-nodes node #{})]
                (assoc acc url (assoc node-nodes node (into node-set nodes)))))
            {}
            result)))

(s/fdef find-best-attr-set
        :args (s/cat :attributed-nodes-in-pages ::attributed-nodes-in-pages))
(defn find-best-attr-set
  [attributed-nodes-in-pages]
  (zipmap (keys attributed-nodes-in-pages)
          (map (fn [attr-nodes] 
                 (set (filter some? (map (fn [[attr nodes]] 
                                           (when (seq nodes)
                                             attr)) 
                                         attr-nodes)))) 
               (vals attributed-nodes-in-pages))))

(defn find-container-node
  [node-attrs all-attrs] 
  (reduce (fn [acc e] 
            (if (reach? acc e) e) acc) 
          (keys (into {}
                      (filter (fn [[node attrs]] (= all-attrs attrs))
                              node-attrs)))))

(s/fdef find-container
        :args (s/cat :reachable-attr-nodes
                     (s/map-of string? (s/map-of #(instance? Element %)
                                                 (s/coll-of keyword?)))
                     :url-attrs
                     (s/map-of string? (s/coll-of keyword?))))
(defn find-container
  [reachable-attr-nodes url-attrs]
  (into {}
        (map #(identity [% (find-container-node (reachable-attr-nodes %) (url-attrs %))]) 
             (keys url-attrs))))


(s/fdef synthesis
        :args (s/cat :attributes 
                     ::attributes 
                     :sites ::sites 
                     :site-extractors ::site-extractor))
(defn synthesis
  [attributes sites site-extractors]
  (loop [attr-knowledge {}
         s-extractors site-extractors
         crawled-extractors {}]
    (let [new-knowledge (merge-with into 
                                    attr-knowledge 
                                    (extract-knowledge sites s-extractors crawled-extractors))]
      ;; new-knowledge ::attr-knowledge
      (for [[site container-extractor] (filter (fn [[site container-extractor]]
                                                 (incomplete-extractors? container-extractor)) 
                                               s-extractors)]
        (let [nodes-in-pages (find-nodes-in-page (:pages (sites site)) new-knowledge)
              reachable-attrs (find-reachable-attrs nodes-in-pages)
              support-nodes (find-support-nodes nodes-in-pages)
              find-support-nodes nodes-in-pages
              ]
          (println nodes-in-pages)
          ;(println reachable-attrs)
          ;(println support-nodes)
          )))))

