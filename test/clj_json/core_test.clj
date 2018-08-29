(ns clj-json.core-test
  (:require [clojure.test :refer :all]
            [the.parsatron :refer [run]]
            [clj-json.core :refer :all]))


(deftest test-number
  (are [expect input] (= expect (run (json-number) input))
    123 "123"
    123 " 123 "
    ))

(deftest test-string
  (are [expect input] (= expect (run (json-string) input))
    "" "\"\""
    "ab" "\"ab\""
    "ab" "  \"ab\" "
    "ab\"cd" "\"ab\\\"cd\""
    "你好" "\"\\u4f60\\u597d\""
    "\\abc" "\"\\\\abc\""
    ))

(deftest test-array
  (are [expect input] (= expect (run (json-array) input))
    ["ab" "cd"] "[\"ab\",\"cd\"]"
    ["ab" "cd"] "[   \"ab\"   ,     \"cd\" ]"
    [true false nil] "[ true, false , null  ]"
    [["ab"] "cd"] "[  [ \"ab\" ]  ,     \"cd\" ]"
    ))

(deftest test-object
  (are [expect input] (= expect (run (json-object) input))
    {"a" "b"} "{\"a\":\"b\"}"
    {"a" "b"} " { \"a\" : \"b\" }"
    {"a" "b" "c" "d"} " {\"a\":\"b\",\"c\":\"d\"}"
    {"a" "b" "c" "d"} " { \"a\" : \"b\" , \"c\" : \"d\" }"
    ))
