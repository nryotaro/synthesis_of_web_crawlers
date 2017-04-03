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
             true)))))

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
      (is (= (result "http://piyo.com") {:title []}))
      )))

(deftest find-container-nodes-test
  (testing "finds nodes which can be candidates of conainters"
    (is (= (find-container-nodes 
             {"http://foo.com" "<html></html>"}
             {"http://foo.com" {:title [(first (.getAllElements (Jsoup/parse "")))]}})
           nil))))

#_(deftest a-test
    (testing ""
      (is (= nil 
             nil))))

#_(deftest synthesis-test
  (testing "tests synthesis"
    (is (= (synthesis #{:title} 
                      {"http://www.economist.com" {:url-pattern #"^http://www\.economist\.com/blogs/.+$" 
                                                   :pages {"http://www.economist.com/blogs/1" "<html><body><span>hello world</span></body></html>"}}
                       "http://www.newsweek.com" {:url-pattern #"^http://www\.newsweek\.com/.+$"
                                                  :pages {"http://www.newsweek.com/1" "<html><body><div>hello world1</div></body></html>"}}}
                      {"http://www.economist.com" {"html > body" {:title "span"}}
                       "http://www.newsweek.com" {"" {:title nil}}})
           nil))))
