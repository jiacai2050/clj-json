(ns clj-json.core
  (:refer-clojure :exclude [char])
  (:require [the.parsatron :refer :all]))

(declare json-value)

(defparser blank []
  (many (token #{\newline \ \tab})))

(defparser json-number []
  (let->> [_ (blank)
           nums (many1 (digit))
           _ (blank)]
    (always (read-string (apply str nums)))))

(defparser json-string []
  (let->> [chars (between (>> (blank) (char \"))
                          (>> (char \") (blank))
                          (many (either (let->> [_ (lookahead (>> (char \\) (char \")))
                                                 _ (times 2 (any-char))]
                                          (always \"))
                                        (token #(not= \" %)))))]
    (always (apply str chars))))

(defparser json-array []
  (let [element (let->> [_ (blank)
                         v (json-value)
                         _ (blank)]
                  (always v))]
    (let->> [_ (>> (blank) (char \[))
             v element
             vs (many (let->> [_ (char \,) 
                               v element]
                        (always v)))
             _ (>> (char \]) (blank))]
      (always (apply vector v vs)))))

(defparser json-object []
  (let [tuple (let->> [_ (blank)
                       k (json-string)
                       _ (>> (blank) (char \:) (blank))
                       v (json-value)
                       _ (blank)]
                (always {k v}))]
    (let->> [_ (>> (blank) (char \{))
             first-tuple tuple 
             rest-tuple (many (let->> [_ (char \,) 
                                       another-tuple tuple
                                       _ (blank)]
                                (always another-tuple)))
             _ (>> (char \}) (blank))]
      (always (merge first-tuple
                     (into {} rest-tuple))))))

(defparser json-literals []
  (let->> [literal (choice (string "true")
                           (string "false")
                           (string "null"))]
    (always (case literal
              "true" true
              "false" false
              nil))))

(defparser json-value []
  (choice (json-number)
          (json-string)
          (json-literals)
          (json-array)
          (json-object)))
