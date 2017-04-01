(ns synthesis-crawlers.page-test
  (:require [clojure.test :refer :all]
            [clojure.spec :as s]
            [clojure.string :refer [starts-with? ends-with?]]
            [clojure.spec.test :as stest]
            [synthesis-crawlers.page :refer :all]))

(stest/instrument)

(deftest get-page-test
    (testing "Gets the specified web page and return its body. 
              If the specified page is not found, it will return nil."
      (is (starts-with? (get-page "http://www.http-kit.org/") "<!DOC"))
      (is (= (get-page "https://hoasdfasdfa/") nil))))

(deftest extract-links-test
  (testing "returns all the links in the specified url"
    (is (= (extract-links "http://nryotaro.org" (slurp "dev-resources/synthesis_crawlers/extract_links.html"))
           '("https://foobar/hoge" "http://nryotaro.org/news")))))

(deftest fetch-web-pages-test
  (testing "fetches web pages"
    (is (= (take 1 (fetch-urls ["http://www.economist.com" ]
                               "http://www.economist.com"
                               #"^http://www\.economist\.com/blogs/.+$" 
                               #{})) 
           '("http://www.economist.com")))))
