(ns synthesis-crawlers.core
  (:require [clojure.spec :as s]
            [synthesis-crawlers.page :as page]
            [clojure.string :refer [starts-with? ends-with? join split] :as string]
            [org.httpkit.client :as http]
            [clojure.data :refer [diff]])
  (:import (org.jsoup Jsoup)
           (org.jsoup.select Elements)
           (org.jsoup.nodes Element)
           (info.debatty.java.stringsimilarity Jaccard)))

(defn element?
  [e]
  (instance? Element e))

(s/def ::attribute keyword?)
(s/def ::attributes (s/coll-of ::attribute))
(s/def ::url-pattern #(instance? java.util.regex.Pattern %))
(s/def ::selector string?)

(s/def ::pages (s/map-of string? string?))
(s/def ::sites (s/map-of string? (s/keys :req-un [::pages])))
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
                                                       (s/coll-of string?))))

(s/def ::url string?)
(s/def ::selector string?)
(s/def ::selectors (s/coll-of string?))

(s/def ::reachable-attributes (s/map-of ::url (s/map-of ::selector ::attributes)))

(s/def ::tag string?)
(s/def ::class (s/coll-of string?))
(s/def ::id string?)
(s/def ::support double?)
(s/def ::expr (s/keys :req-un [::tag ::class ::id]))

(s/def ::crawled-extractors (s/map-of ::url (s/coll-of ::extractor)))

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
                     :crawled-extractors ::crawled-extractors))
(defn uncrawled-extractors
  "Returns uncrawled site extractors."
  [site-extractors crawled-extractors]
  (into {}
        (filter some? 
                (map (fn [[site extractor]] 
                       (cond
                         (nil? (crawled-extractors site)) [site extractor]
                         (not (some #(= extractor %) (crawled-extractors site))) [site extractor]
                         :else nil)) 
                     site-extractors))))

(defn crawled?
  [extractor crawled-set]
  (crawled-set extractor))

(s/fdef build-selector
  :args (s/cat :attribute ::attribute :container-expr ::container-extractor :attr-expr string?))
(defn build-selector [attribute container-expr attr-expr]
  {attribute 
   (cond
     (empty? container-expr) attr-expr
     (empty? attr-expr) container-expr
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
                     :site-extractor ::site-extractor))
(defn extract-knowledge
  "extracts knwoledge from the specified site"
  [sites site-extractor]
  (let [extracted (for [[site extractor] site-extractor 
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

(s/fdef filter-duplicated-nodes
  :args (s/cat :text string?))
(defn filter-duplicated-nodes
  "returns nodes which don't have inner nodes contain the same text"
  [text]
  (let [root (Jsoup/parse text)]
    (loop [nodes [root]
           done #{}]
      (if-not (seq nodes)
        done
        (let [node (first nodes)]
          (if (seq (filter #(= (.text node) (.text %)) (.children node)))
            (recur (into (rest nodes) (.children node)) done)
            (recur (into (rest nodes) (.children node)) (conj done node))))))))

(s/fdef reach?
        :args (s/cat :a element? :b element?))
(defn reach?
  "returns true iff b is reachable from a"
  [a b]
  (->> (filter #(.is b (.cssSelector %)) (.getAllElements a)) empty? not))

(s/fdef find-attr-nodes
        :args (s/cat :nodes #(or (instance? Elements %) element?) :knowledge ::knowledge))
(defn find-attr-nodes 
  "returns the elements which contain text similar to the specified knowledge"
  [nodes knowledge]
  (reduce 
    (fn [nodes node]
      (let [p (set (filter #(reach? % node) nodes))
            c (filter #(reach? node %) nodes)]
        (cond 
          ; nodes contain a node which is a parent of node
          (seq p) (do 
                    (conj (remove p nodes) node))
          ; node is a parent of one of nodes
          (seq c) nodes
          :else (conj nodes node))))
    [] 
    (filter #(not-empty (matched-knowledge (.text %) knowledge)) nodes)))

(s/fdef find-nodes-in-page
  :args (s/cat :pages ::pages :attr-knowledge ::attr-knowledge))
(defn find-nodes-in-page
  [pages attr-knowledge]
  (reduce (fn [acc [url text]]
            (assoc acc 
                   url 
                   (reduce 
                     (fn [k [attr knowledge-set]] 
                       (assoc k 
                              attr 
                              (set 
                                (map #(.cssSelector %) 
                                     (find-attr-nodes 
                                       (filter-duplicated-nodes text)
                                       knowledge-set))))) 
                     {} 
                     attr-knowledge))) 
          {} 
          pages))

(defn reachable-elements
  [elem]
  (conj (into [] (.parents elem)) elem))

(defn find-parents
  [selector text]
  (reachable-elements (first (.select (Jsoup/parse text) selector))))

(s/fdef find-reachable-attrs
        :args (s/cat :attr-nodes ::attributed-nodes-in-pages
                     :pages ::pages))
(defn find-reachable-attrs 
  [attr-nodes pages] 
  (let [result (for [[url attr-selectors] attr-nodes
                     [attr selectors] attr-selectors
                     selector (set (map #(.cssSelector %) 
                                    (flatten (map #(find-parents % (pages url)) selectors))))] 
                 [url [selector attr]])
        reachable-attrs (reduce (fn [acc [url [node attr]]] 
                                  (let [node-attr (get acc url {})
                                        attrs (get node-attr node #{})]
                                    (assoc acc url (assoc node-attr node (conj attrs attr)))))
                                {} 
                                result)]
    reachable-attrs))


;; nested nodes aren't supported <- ?
(s/fdef find-support-nodes
        :args (s/cat :attr-nodes ::attributed-nodes-in-pages
                     :pages ::pages))
(defn find-support-nodes
  [url-attr-selectors pages]
  (let [result (for [[url attr-selectors] url-attr-selectors
                     [attr selectors] attr-selectors
                     selector (set (map #(.cssSelector %) 
                                        (flatten 
                                          (map #(find-parents % (pages url)) selectors))))]
                 [url [selector selectors]])
        ;url-root 
        #_(zipmap (keys url-attr-nodes) 
                         (map #(.ownerDocument (first (second (first %)))) 
                                                (vals url-attr-nodes)))
        support-nodes (reduce (fn [acc [url [node nodes]]] 
                                (let [node-nodes (get acc url {})
                                      node-set (get node-nodes node #{})]
                                  (assoc acc 
                                         url 
                                         (assoc node-nodes node (into node-set nodes)))))
                              {}
                              result)]
    support-nodes
    #_(into {}
          (for [[url node-nodes] support-nodes]
            [url (zipmap (map #(first (.select (url-root url) %)) (keys node-nodes))
                         (for [nodes (vals node-nodes)](map #(first (.select (url-root url) %)) nodes)))]))))

(s/fdef count-support-nodes
       :args (s/cat :support-nodes (s/map-of string? (s/map-of string?
                                                               (s/coll-of string?)))))

(defn count-support-nodes
  [support-nodes]
  (into {} (for [[url node-nodes] support-nodes]
             [url (into {}  (for [[node nodes] node-nodes]
                              [node (count nodes)]))])))

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

(s/fdef find-container-node
        :args (s/cat :node-attrs (s/map-of ::selector ::attributes)
                     :all-attrs ::attributes
                     :text ::text))
(defn find-container-node
  [node-attrs all-attrs text] 
  (let [document (Jsoup/parse text)]
    (set 
      (map #(.cssSelector %)
           (reduce (fn [acc e] 
                     (let [removed (remove #(reach? % e) acc)]
                       (if-not (seq removed)
                         (conj (filter #(not (reach? % e)) acc) e)
                         acc)))
                   #{}
                   (map 
                     #(first (.select document %))
                     (keys (into {}
                                 (filter (fn [[node attrs]] (= all-attrs attrs))
                                         node-attrs)))))))))

(s/fdef find-container
        :args (s/cat :reachable-attr-nodes ::reachable-attributes
                     :url-attrs (s/map-of ::url ::attributes)
                     :pages ::pages))
(defn find-container
  [reachable-attr-nodes url-attrs pages]
  (into {}
        (map #(identity [% (find-container-node (reachable-attr-nodes %) (url-attrs %) (pages %))]) 
             (keys url-attrs))))


(s/fdef encode-node-path
        :args (s/cat :node element?))
(defn encode-node-path
  [node]
  (reverse 
    (reduce (fn [acc node]
              (conj acc {:tag (.tagName node) :class (set (.classNames node)) :id (.id node)})) 
            [] 
            (into [node] (.parents node)))))

(s/fdef decode-node-path
        :args (s/cat :encoded-node
                     (s/coll-of ::expr)))
(defn decode-node-path
  [encoded-node]
  (join " > " (map (fn [{:keys [tag class id]}]
                     (str tag 
                          (when (seq id) (str "#" id))
                          (when (seq class)
                            (str "." (join "." (sort (vec class)))))))
                   encoded-node)))


(s/fdef create-relative-path
        :args (s/alt :two (s/cat :prefix string? :node element?)
                     :one (s/cat :node element?)))
(defn create-relative-path
  ([prefix node] 
   (let [decoded (create-relative-path node)] 
     (cond 
       (= prefix decoded) "" 
       (seq prefix) (string/replace decoded (re-pattern (str prefix " > ")) "")
       :else decoded
       )))
  ([node] (decode-node-path (encode-node-path node))))

(s/fdef generate-container-cand-exprs
        :args (s/cat :container-cand-nodes (s/map-of ::url ::selectors)
                     :support-node-num (s/map-of ::url (s/map-of ::selector pos-int?))
                     :pages ::pages))
(defn generate-container-cand-exprs
  [container-cand-nodes support-node-num pages]
  (into {} 
        (for [[url container-cands] container-cand-nodes
              document [(Jsoup/parse (pages url))]
              cand container-cands]
          (do 
            [(create-relative-path (first (.select document cand))) ((support-node-num url) cand)]))))

(s/fdef parse-css-clause :args (s/cat :clause string?))
(defn parse-css-clause
  [clause] 
  (let [tag (re-seq #"^[^#\.]+" clause)
        classes (re-seq #"\.([^\.]+)" clause)
        id (re-seq  #"#([^\.\s]+)" clause)]
    (hash-map :tag (if (seq tag) (first tag) "")
              :id (if (seq id) (->> id first second) "")
              :class (if (seq classes) (set (map second classes)) #{}))))

(s/fdef parse-css-selector
        :args (s/cat :selector string?))
(defn parse-css-selector
  [selector]
  (map #(parse-css-clause %) 
       (split selector #"\s+>\s+")))


(s/fdef agree?
        :args (s/cat :a (s/coll-of ::expr)
                     :b (s/coll-of ::expr)))
(defn agree?
  "returns true iff a satisfies with b"
  [a b]
  (if (< (count a) (count b)) 
    false
    (reduce 
      #(if %1 %2 false)
      true
      (map (fn [{atag :tag aclass :class aid :id} 
                {btag :tag bclass :class bid :id}]
             (and (or (nil? (seq bid)) (= aid bid))
                  (or (nil? (seq btag)) (= atag btag))
                  (nil? (second (diff aclass bclass))))) 
           a b))))

(s/fdef compare-instruction
        :args (s/cat :a-inst ::expr :b-inst ::expr))
(defn compare-instruction
  [a-inst b-inst]
  (letfn [(full-filled? [{:keys [tag class id]}] 
            (and (seq tag) (seq class) (seq id)))]
    (cond 
      (agree? [a-inst] [b-inst]) -1
      (agree? [b-inst] [a-inst]) 1
      (and (full-filled? a-inst) (full-filled? b-inst)) (- (count (:class b-inst)) 
                                                           (count (:class a-inst)))
      (and (seq (:id a-inst)) (not (seq (:id b-inst)))) -1
      (and (seq (:id b-inst)) (not (seq (:id a-inst)))) 1
      :else (- (+ (if (seq (:tag b-inst)) 1 0) (count (:class b-inst)))
               (+ (if (seq (:tag a-inst)) 1 0) (count (:class a-inst)))))))


(s/fdef select-uni-inst
        :args (s/cat :inst-support
                     (s/map-of ::expr pos-int?)
                     :threshold double?
                     :total-support pos-int?))
(defn select-uni-inst
  [inst-support threshold total-support]
  (reduce 
    (fn [acc e]
      (if-not (seq acc)
        (let [counted (apply + (vals (filter (fn [[inst _]] (agree? [inst] [e])) 
                                             inst-support)))]
          (if (> counted (* total-support threshold))
            e))
        acc))
    nil
    (sort compare-instruction (keys inst-support))))

(s/fdef unify-exprs
        :args (s/cat :exprs-with-supports 
                     (s/map-of (s/coll-of ::expr) pos-int?)
                     :threshold double?))
(defn unify-exprs
  [exprs-with-supports threshold]
  (when (seq exprs-with-supports)
    (let [total-support (apply + (vals exprs-with-supports))
          longest  (apply max (map count (keys exprs-with-supports))
                          #_(map (fn [{:keys [expr]}](count expr)) 
                                 exprs-with-supports))]
      (loop [iter 0
             still-agreed exprs-with-supports
             agreed-path []]
        (if (>= iter longest)
          agreed-path
          (let [agreed (filter (fn [[path _]] (agree? path agreed-path)) still-agreed)
                instructions (apply (partial merge-with +)
                                    (map (fn [[path support]]
                                           (hash-map (nth path iter) support))
                                         (filter (fn [[k _]](> (count k) iter))
                                                 agreed)
                                         ))]
            (if (empty? instructions)
              agreed-path
              (let [inst (select-uni-inst instructions threshold total-support)]
                (recur (inc iter) 
                       agreed 
                       (if inst (conj agreed-path inst) agreed-path))))))))))

(s/fdef flatten-attributed-nodes
        :args (s/cat :nodes-in-page (s/map-of ::url (s/map-of ::attribute ::selectors ))
                     :pages ::pages))
(defn flatten-attributed-nodes
  [nodes-in-page pages]
  (let [result (for [[url attr-selectors] nodes-in-page]
                 (let [document (Jsoup/parse (pages url))]
                   (zipmap (keys attr-selectors)
                           (map #(set 
                                   (map (fn [selector] (first (.select document selector))) %)) 
                                (vals attr-selectors)))))]
    (apply (partial merge-with into) result)))

(s/fdef generate-attr-exprs
  :args (s/cat :container-descriptions (s/coll-of ::expr)
               :nodes-in-pages ::attributed-nodes-in-pages
               :pages ::pages
               :threshold double?))
(defn generate-attr-exprs
  [container-descriptions nodes-in-pages pages threshold]
  (let [attr-nodes (flatten-attributed-nodes nodes-in-pages pages)]
    (into {} 
          (for [attr (keys attr-nodes)]
            (let [nodes (map #(parse-css-selector 
                                  (create-relative-path 
                                    (decode-node-path container-descriptions) %))
                             (attr-nodes attr))]
              [attr (decode-node-path 
                      (unify-exprs (zipmap nodes (repeat (count nodes) 1)) threshold))])))))

(s/fdef generate-extractors
        :args (s/cat :pages ::pages 
                     :attributed-knowledge ::attr-knowledge
                     :threshold double?))
(defn generate-extractors
  [pages attributed-knowledge threshold]
  (let [nodes-in-pages (find-nodes-in-page pages attributed-knowledge)
        reachable-attrs (find-reachable-attrs nodes-in-pages pages)
        support-nodes (count-support-nodes (find-support-nodes nodes-in-pages pages))
        container-cand-nodes (find-container reachable-attrs 
                                             (find-best-attr-set 
                                               nodes-in-pages) 
                                             pages)
        container-cand-exprs (generate-container-cand-exprs 
                               container-cand-nodes support-nodes pages)
        container-expr (unify-exprs 
                         (zipmap (map parse-css-selector (keys container-cand-exprs)) 
                                 (vals container-cand-exprs))
                         threshold)
        attr-exprs (generate-attr-exprs container-expr nodes-in-pages pages threshold)]
    {(decode-node-path container-expr) attr-exprs}))

(s/fdef check-crawled-extractors
  :args (s/cat :crawled-extractors ::crawled-extractors
               :extractors ::site-extractor))
(defn check-crawled-extractors
  [crawled-extractors extractors]
  (merge crawled-extractors
         (into 
           {}
           (for [[site exts] extractors]
             (cond
               (crawled-extractors site) [site (conj (crawled-extractors site) exts)]
               :else [site (conj #{} exts)])))))

(s/fdef synthesis
        :args (s/cat :attributes 
                     ::attributes 
                     :sites ::sites 
                     :site-extractors ::site-extractor
                     :threshold double?))
(defn synthesis
  [attributes sites site-extractors threshold]
  (loop [attr-knowledge {}
         s-extractors site-extractors
         crawled-extractors {}]
    (let [extractors (uncrawled-extractors s-extractors crawled-extractors)
          new-knowledge (merge-with into 
                                    attr-knowledge 
                                    (extract-knowledge sites extractors))
          updated-extractors (into 
                               {}
                               (for [[site _] (filter (fn [[site container-extractor]]
                                                        (incomplete-extractors? container-extractor)) 
                                                      s-extractors)]
                                 [site (generate-extractors (:pages (sites site)) new-knowledge threshold)]))
          resulted-extractors (merge s-extractors updated-extractors)]
      (if (not (and (= attr-knowledge new-knowledge) (= s-extractors resulted-extractors)))
        (recur new-knowledge resulted-extractors (check-crawled-extractors crawled-extractors extractors))
        resulted-extractors))))

