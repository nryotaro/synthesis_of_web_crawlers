(ns synthesis-crawlers.core-test
  (:require [clojure.test :refer :all]
            [synthesis-crawlers.core :refer :all]))


(deftest knowledge-test
  (testing "find difference between two knowlege"
    (is (diff-knowledge? {:title #{"wagahai-ha-neko-de-aru"}} {:title #{:title {}}}))))
