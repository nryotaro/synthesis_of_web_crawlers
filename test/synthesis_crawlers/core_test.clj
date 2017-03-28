(ns synthesis-crawlers.core-test
  (:require [clojure.test :refer :all]
            [clojure.spec :as s]
            [clojure.spec.test :as stest]
            [clojure.string :refer [starts-with?]]
            [synthesis-crawlers.core :refer :all]))

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
    (is (crawled? {"site-url" ["container-expr" {:attr "partial-expr"}]} #{{"site-url" ["container-expr" {:attr "partial-expr"}]}}))))

#_(deftest get-page-test
    (testing "gets the specified web page and return its body"
      (is (starts-with? (get-page "http://www.http-kit.org/") "<!DOC"))))

(deftest build-selector-test
  (testing "builds a selector"
    (is (= (build-selector :a "html" "body")
           {:a "html > body"}))
    (is (= (build-selector :a "" "body")
           {:a "body"}))))

(deftest build-selectors-test
  (testing "converts the specified extractors to the selector map"
    (is (= (build-selectors {"html" {:a "body"}})
           {:a "html > body"}))))

#_(deftest extract-instance-test
  (testing "tries extracting values with expressions"
    (let [text (slurp "dev-resources/index.html")]
      #_(is (= (extract text
                      {"html > body > div[id=wrapper] > div[id=main] > article[class=post] > header > div[class=title]" {:title "h2 > a" :date "time[class=published]"}}) 
             {:title #{"Installing Atom packages on Windows behind a proxy"}
              :date #{"August 14, 2016"}}))
      (is (= (extract text
                      {"" {:title "html > body > div[id=wrapper] > div[id=main] > article[class=post] > header > div[class=title] > h2 > a" 
                           :date "html > body > div[id=wrapper] > div[id=main] > article[class=post] > header > div[class=title] > time[class=published]"}}) 
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

  #_(testing "extrtacts knowledge"
    (is (= (extract-instance #{:title :price} 
                             #{"http://www.barnesandnoble.com/w/living-clojure-carin-meier/1120914833?ean=9781491909041"}
                             #{["container-expr" {:title "title-expr" :price "price-expr"}]}
                             #{}) 
           {:complete nil :knowledge nil :incomplete nil})))
