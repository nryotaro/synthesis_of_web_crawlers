(ns synthesis-crawlers.core-test
  (:require [clojure.test :refer :all]
            [clojure.spec :as s]
            [synthesis-crawlers.page :as page]
            [clojure.spec.test :as stest]
            [clojure.string :refer [starts-with?]]
            [synthesis-crawlers.core :refer :all])
  (:import (org.jsoup Jsoup)
           (org.jsoup.nodes Element)))

(stest/instrument)

(deftest knowledge-difference-test
  (testing "returns some if there is no difference between two knowlege otherwise returns nil"
    (is (diff-knowledge? {:title #{"wagahai-ha-neko-de-aru"}} {:title #{""}}))
    (is (not (diff-knowledge? {:title #{"wagahai-ha-neko-de-aru"}} {:title #{"wagahai-ha-neko-de-aru"}})))))

(deftest extractors-difference-test
  (testing "return some if there is no difference between two extractor sets otherwise returns nil"
    (is (not (diff-extractors? {"site-url" ["container-expr" {:attr "partial-expr"}]}
                               {"site-url" ["container-expr" {:attr "partial-expr"}]})) "extractors-difference-test-1")
    (is (diff-extractors? {"site-url" ["container-expr" {:attr "partial-expr"}]
                           "site-url2" ["container-expr2" {:attr "partial-expr"}]}
                          {"site-url" ["container-expr" {:attr "partial-expr"}]}) "extractors-difference-test-2")))

(deftest crawled-test
  (testing "returns something if the specified extractor is used"
    (is (crawled? {"site-url" ["container-expr" {:attr "partial-expr"}]} 
                  #{{"site-url" ["container-expr" {:attr "partial-expr"}]}}))))

(deftest build-selector-test
  (testing "builds a selector"
    (is (= (build-selector :a "html" "body")
           {:a "html > body"}))
    (is (= (build-selector :a "" "body")
           {:a "body"}))))

(deftest build-selectors-test
  (testing "converts the specified extractors to the selector map"
    (is (= (build-selectors {"html" {:a "body" :b "head"} "foo" {:bar "bar" :a "hi"}})
           {:a #{"foo > hi" "html > body"}, 
            :b #{"html > head"}, 
            :bar #{"foo > bar"}}))
    (is (= (build-selectors {"html" {:a "body" :b nil}})
           {:a #{"html > body"}}))))

(deftest extract-instance-test
  (testing "tries extracting values with expressions"
    (let [text (slurp "dev-resources/index.html")]
      (is (= (extract text
                      {"html > body > div[id=wrapper] > div[id=main] > article[class=post] > header > div[class=title]" {:title "h2 > a" :date "time[class=published]"}}) 
             {:title #{"Installing Atom packages on Windows behind a proxy"}
              :date #{"August 14, 2016"}}))
      (is (= (extract text
                      {"" {:title "html > body > div[id=wrapper] > div[id=main] > article[class=post] > header > div[class=title] > h2 > a" 
                           :date "html > body > div[id=wrapper] > div[id=main] > article[class=post] > header > div[class=title] > time[class=published]"}
                       "foo" {:date "bar"}}) 
             {:title #{"Installing Atom packages on Windows behind a proxy"}
              :date #{"August 14, 2016"}})))))


(deftest filter-undefined-extractor-test
  (testing "drops the undefined expressions of attributes"
    (is (= (drop-undef-attr-extractors {:title nil :date ""})
           {:date ""}))))

(deftest empty-extractor?-test
  (testing "returns a non nil value iff the specifiled extractor is empty")
  (is (= (empty-extractor? {"" {:a ""}})
         false))
  (is (= (empty-extractor? {}) 
         true))
  (is (= (empty-extractor? {"" {:a nil}}) 
         true)))

(deftest calc-similarity-test
  (testing "returns text similarity coefficient"
    (is (= (similarity "asds" "asds")
           1.0))))

(deftest incomplete-extractors?-test
  (testing "returns true value iff the specified extractor is incomplete"
    (is (incomplete-extractors? {"html" {:a "body" :b nil}}))
    (is (incomplete-extractors? {"" {:a nil :b nil}}))
    (is (not (incomplete-extractors? {"html" {:a "body" :b "header"}})))))


(deftest match-knowledge-test
  (testing "returns the subset of knowledge which are similar to the specified text"
    (is (= (matched-knowledge "foobar" #{"foobar"}) 
           ["foobar"]))
    (is (= (matched-knowledge "aa" #{"foobar"}) 
           []))))

(deftest filter-duplicated-nodes-test
  (testing "returns nodes which don't have innder nodes contain the same text"
    (let [text "<html><body><div><span>abcde</span></div></body></html>"]
      (is (= (set (map #(.tagName %) (filter-duplicated-nodes text))) 
             #{"span" "head"})))))


(deftest find-attr-nodes-test
  (testing "returns the elements which contain text similar to the specified knowledge"
    (let [text (slurp "dev-resources/synthesis_crawlers/find_textnodes.html")]
      (is (= (map #(.text %)
                  (find-attr-nodes (.getAllElements (Jsoup/parse text))
                                   #{"sed do eiusmod tempor"})) 
             ["sed do eiusmod tempor"])))))

(deftest reach?-test
  (testing "returns true iff a contains d"
    (let [text (Jsoup/parse "<html><body><div><a>hoge</a></div></body></html>")]
      (is (= (reach? (first (.select text "div")) (first (.select text "div > a"))) 
             true))
      (is (= (reach? (first (.select text "div > a")) (first (.select text "div")))
             false)))))

(deftest uncrawled-extractors-test
  (testing "returns uncrawled site extractors"
    (is (= (uncrawled-extractors {"site" {"container" {:attr "containee"}}
                                  "site2" {"container2" {:attr "containee2"}}}
                                 {"site" {"container" {:attr "containee"}}})
           {"site2" {"container2" {:attr "containee2"}}}))))


(deftest extract-knowledge-test
  (testing "extracts knwoledge from the specified site"
    (is (= (extract-knowledge {"http://www.economist.com" {:url-pattern #"^http://www\.economist\.com/blogs/.+$" :pages {"http://www.economist.com/blogs/1" "<html><body><div><span>All in the golden afternoon</span></div></body></html>"
                                                                                                                         "http://www.economist.com/blogs/2" ""
                                                                                                                         }}}
                              {"http://www.economist.com" {"html > body > div" {:title "span"}}}
                              {})
           {:title #{"All in the golden afternoon"}}))))

(deftest find-nodes-in-page-test
  (testing "Finds nodes in a page."
    (let [text "<html><body><div>hoge</div><span>bar foo</span></body></html>"
          nodes (Jsoup/parse text)
          result (find-nodes-in-page {"http://foobar.com" text 
                                      "http://piyo.com" "<html></html>"} 
                                 {:title #{"hoge" "piyo"}})]
      (is (s/valid? (s/map-of string? 
                              (s/map-of keyword? 
                                        (s/coll-of #(instance? Element %)))) 
                    result))
      (is (= (count (:title (result "http://foobar.com"))) 1))
      (is (= (result "http://piyo.com") {:title []}))))
  (testing "test case2"
    (is (= (find-nodes-in-page 
             {"http://example.com/1" 
              (slurp "dev-resources/synthesis_crawlers/generate-extractor/sample1.html")}
             {:food #{"bacon" "batter" "black beans"}
              :date #{"2016-10-02"}}) "to be implemented"))))

#_(deftest find-nodes-test
  (testing "finds nodes in the specified page."
    (is (= (find-nodes 
             {"http://example.com/1" (slurp "dev-resources/synthesis_crawlers/generate-extractor/sample1.html")}
             {:food #{"bacon" "batter" "black beans"}
              :date #{"2016-10-02"}}) "to be implemented"))))

(deftest find-reachable-attrs-test
  (testing "finds nodes which can be candidates of conainters"
    (let [text (Jsoup/parse 
                 (slurp "dev-resources/synthesis_crawlers/find-reachable-attrs.html"))
         inner-div (first (.select text "html > body > div > div")) 
         html (first (.select text "html")) 
         body (first (.select text "html > body")) 
         span (first (.select text "html > body > div > span")) 
         outer-div (first (.select text "html > body > div"))
         result (find-reachable-attrs 
                  {"http://foo.com" {:title [inner-div] :date [span]}})]
      (is (= result
             {"http://foo.com" {inner-div #{:title}
                                outer-div #{:date :title}
                                html #{:date :title}
                                body #{:date :title}
                                span #{:date}}})))))

(deftest find-support-nodes-test
  (testing "returns nodes which can be reachable from the specified nodes, 
           and contain attributes"
    (let [text (Jsoup/parse (slurp "dev-resources/synthesis_crawlers/find-support-nodes.html"))
          inner-div (first (.select text "html > body > div > div")) 
          html (first (.select text "html")) 
          body (first (.select text "html > body")) 
          span (first (.select text "html > body > div > span")) 
          outer-div (first (.select text "html > body > div"))
          result (find-support-nodes
                   {"http://foo.com" {:title [inner-div] :date [span]}})]
      (is (= result
             {"http://foo.com" {inner-div #{inner-div}
                                outer-div #{inner-div span}
                                html #{inner-div span}
                                body #{inner-div span}
                                span #{span}}}))
      (is (= (count-support-nodes result) 
             {"http://foo.com" {inner-div 1
                                outer-div 2
                                html 2
                                body 2
                                span 1}}))
      )))

(deftest reachable-elements-test
  (testing "return reachable elements"
    (let [elem (first (.select (Jsoup/parse "<html><body><div></div></body></html>") "html > body > div"))]
      (is (= (count (reachable-elements elem)) 3)))))

(deftest find-best-attr-set-test
  (testing "returns attributes found in pages"
    (let [text (Jsoup/parse (slurp "dev-resources/synthesis_crawlers/find-reachable-attrs.html"))
          inner-div (first (.select text "html > body > div > div")) 
          html (first (.select text "html")) 
          body (first (.select text "html > body")) 
          span (first (.select text "html > body > div > span")) 
          outer-div (first (.select text "html > body > div"))
          result (find-best-attr-set
                   {"http://foo.com" {:title [inner-div] :date [span] :body []}})]
      (is (= result
             {"http://foo.com" #{:title :date}})))))

(deftest finds-container-test
  (testing "finds the container node in each page"
    (let [text (Jsoup/parse (slurp "dev-resources/synthesis_crawlers/find-reachable-attrs.html"))
          inner-div (first (.select text "html > body > div > div")) 
          html (first (.select text "html")) 
          body (first (.select text "html > body")) 
          span (first (.select text "html > body > div > span")) 
          outer-div (first (.select text "html > body > div"))
          result (find-best-attr-set
                   {"http://foo.com" {:title [inner-div] :date [span] :body []}})]
      (is (= (find-container-node 
               {inner-div #{:title}
                outer-div #{:date :title}
                html #{:date :title}
                body #{:date :title}
                span #{:date}}
               #{:title :date})
             #{outer-div}))
      (is (= (find-container 
               {"http://foo.com" {inner-div #{:title}
                                  outer-div #{:date :title}
                                  html #{:date :title}
                                  body #{:date :title}
                                  span #{:date}}}
               {"http://foo.com" #{:title :date}})
             {"http://foo.com" #{outer-div}})))))

(deftest encode-node-path-test
  (testing "builds node path"
    (let [node (first 
                 (.select 
                   (Jsoup/parse 
                     "<html><body><div class=\"hoge\"><span id=\"sp\"></span></div></html>") "html > body > div > span#sp"))]
      (is (= (encode-node-path node)
             [{:tag "html", :class #{}, :id ""} 
              {:tag "body", :class #{}, :id ""} 
              {:tag "div", :class #{"hoge"}, :id ""} 
              {:tag "span", :class #{}, :id "sp"}])))))

(deftest decode-node-path-test
  (testing "decodes node path"
    (is (= (decode-node-path [{:tag "html", :class #{}, :id ""} 
                              {:tag "body", :class #{}, :id ""} 
                              {:tag "div", :class #{"hoge" "piyo"}, :id "bar"} 
                              {:tag "span", :class #{}, :id "sp"}])
           "html > body > div#bar.hoge.piyo > span#sp"))))

(deftest create-relative-path-test
  (testing "returns a css selector which specifies e"
    (let [node (first (.select (Jsoup/parse "<html><body><div class=\"hoge\"><span id=\"sp\"></span></div></html>") "html > body > div > span#sp"))]
      (is (= (create-relative-path node) "html > body > div.hoge > span#sp"))
      (is (= (create-relative-path "html > body > div.hoge" node) "span#sp"))
      )))

(deftest generate-container-cand-exprs-test
  (testing "generates the expressions of the specified containers"
    (let [text (Jsoup/parse (slurp "dev-resources/synthesis_crawlers/find-reachable-attrs.html"))
          inner-div (first (.select text "html > body > div > div")) 
          html (first (.select text "html")) 
          body (first (.select text "html > body")) 
          span (first (.select text "html > body > div > span")) 
          outer-div (first (.select text "html > body > div"))]
      (is (= (generate-container-cand-exprs
               {"http://foo.com" #{outer-div}}
               {"http://foo.com" {inner-div 1
                                  outer-div 2
                                  html 2
                                  body 2
                                  span 1}})
             {"html > body > div" 2})))))

(deftest parse-css-selctor-test
  (testing "parses the specified css selector"
    (is (= (parse-css-clause "html") {:tag "html" :class #{} :id ""}))
    (is (= (parse-css-clause "div#bar.hoge.piyo") {:tag "div" :class #{"hoge" "piyo"} :id "bar"}))
    (is (=  (parse-css-selector
              "html > body > div#bar.hoge.piyo > span#sp")
           [{:tag "html", :class #{}, :id ""} 
            {:tag "body", :class #{}, :id ""} 
            {:tag "div", :class #{"hoge" "piyo"}, :id "bar"} 
            {:tag "span", :class #{}, :id "sp"}]))))

(deftest agree?-test
  (testing "returns true iff a satisfies with b"
    (let [t1 (parse-css-selector "html > body > div#id")
          t2 (parse-css-selector "html > body > div")]
      (is (= (agree? t1 [])
             true))
      (is (= (agree? t1 t2)
             true))
      )
    (is (= (agree? [] [])
           true))))


(deftest sort-instructions-test
  (testing "sorts the specified instructions"
    (is (< (compare-instruction {:tag "html" :class #{} :id "#id"}
                                {:tag "html" :class #{} :id ""})
           0))
    (is (> (compare-instruction {:tag "html" :class #{} :id "#id"}
                                {:tag "html" :class #{"a"} :id "a"})
           0))
    (is (> (compare-instruction {:tag "html" :class #{} :id "#id"}
                                {:tag "html" :class #{"a"} :id "a"})
           0))
    (is (> (compare-instruction {:tag "html" :class #{"a"} :id "a"}
                                {:tag "html" :class #{"a" "b"} :id "id"})
           0))
    ))

(deftest select-uni-inst-test
  (testing "selects unifiled instruction"
    (is (= (select-uni-inst
             {{:tag "div" :class #{} :id ""} 1
              {:tag "div" :class #{} :id "foo"} 1}
             0.5
             2)
           {:tag "div" :class #{} :id ""}))))

(deftest unify-test
  (testing "unify exprs"
    (let [s1 (parse-css-selector "html > body > div.foo > div")
          s2 (parse-css-selector "html > body > div.foo > div.piyo")
          s3 (parse-css-selector "html > body > div.foo")
          ]
      (is (= (decode-node-path (unify-exprs {s1 2 s2 1} 0.6))
             "html > body > div.foo > div"))
      (is (= (decode-node-path (unify-exprs {s1 1 s3 1} 0.6))
             "html > body > div.foo")))))

(deftest generate-attr-exprs-test
  (testing "generates attribute descriptors"
    (let [text (Jsoup/parse (slurp "dev-resources/synthesis_crawlers/generate-attr-exprs.html"))
          inner-div (first (.select text "html > body > div > div")) 
          span (first (.select text "html > body > div > span")) 
          html (first (.select text "html")) 
          body (first (.select text "html > body")) 
          outer-div (first (.select text "html > body > div"))]
      (is (= (generate-attr-exprs (parse-css-selector "html > body > div") 
                                  {"http://foo.com" {:title #{inner-div} :date #{span}}} 
                                  0.5) 
             {:title "div" :date "span"})))))

(deftest generate-extractor-test
  (testing "generates extractors from the speicfied texts and words"
    (is (= (generate-extractors
             {"http://example.com/1" (slurp "dev-resources/synthesis_crawlers/generate-extractor/sample1.html")}
             {:food #{"bacon" "batter" "black beans"}
              :date #{"2016-10-02"}}
             0.5)
           "to be implemented"))))

#_(deftest a-test
    (testing ""
      (is (= nil 
             "to be implemented"))))

#_(deftest synthesis-test
  (testing "tests synthesis"
    (is (= (synthesis #{:title} 
                      {"http://www.economist.com" {:url-pattern #"^http://www\.economist\.com/blogs/.+$" 
                                                   :pages {"http://www.economist.com/blogs/1" "<html><body><span>hello world</span></body></html>"}}
                       "http://www.newsweek.com" {:url-pattern #"^http://www\.newsweek\.com/.+$"
                                                  :pages {"http://www.newsweek.com/1" "<html><body><div>hello world1</div></body></html>"}}}
                      {"http://www.economist.com" {"html > body" {:title "span"}}
                       "http://www.newsweek.com" {"" {:title nil}}}
                      0.5)
           nil))))
