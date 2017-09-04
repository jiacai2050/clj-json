(ns clj-json.core-test
  (:require [clojure.test :refer :all]
            [clj-json.core :refer :all]))

(deftest test-parse-string
  (testing "normal string"
    (is (= (:parsed (parse-value "\"abc\""))
           "abc")))
  (testing "escape with \""
    (is (= (:parsed (parse-value "\"ab\\\"cd\""))
           "ab\"cd"))))
