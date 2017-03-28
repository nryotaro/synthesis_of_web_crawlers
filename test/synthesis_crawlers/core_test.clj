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

(deftest extract-instance-test
  (testing "tries extracting values with expressions"
    (is (= (extract (slurp "dev-resources/index.html") 
                    {"html > body > div[id=wrapper] > div[id=main] > article[class=post] > header > div[class=title]" {:title "h2 > a" :date "time[class=published]"}}) 
           {:title #{"Installing Atom packages on Windows behind a proxy"}
            :date #{"August 14, 2016"}})))

(deftest filter-undefined-extractor-test
  (testing "drops the undefined expressions of attributes"
    (is (= (drop-undef-attr-extractors {:title nil :date ""})
           {:date ""}))))

  #_(testing "extrtacts knowledge"
    (is (= (extract-instance #{:title :price} 
                             #{"http://www.barnesandnoble.com/w/living-clojure-carin-meier/1120914833?ean=9781491909041"}
                             #{["container-expr" {:title "title-expr" :price "price-expr"}]}
                             #{}) 
           {:complete nil :knowledge nil :incomplete nil}))))
