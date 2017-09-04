(ns clj-json.core)

(defn- blank? [c]
  (#{\space \tab \return \newline} c))

(defn- string? [c]
  (= \" c))

(defn- number? [c]
  (Character/isDigit c))

(defn- object? [c]
  (= \{ c))

(defn- array? [c]
  (= \[ c))

(defrecord Holder [raw-input parsed err])

(defn parse-string [holder]
  (loop [prev nil
         raw-input (:raw-input holder)
         acc (StringBuffer.)]
    (if-let [c (first raw-input)]
      (let [remaining (subs raw-input 1)]
        (if (= \" c)
          (condp = prev
            nil (recur c remaining acc)
            \\ (recur c remaining (.. acc
                                      (deleteCharAt (dec (count acc)))
                                      (append c)))
            (assoc holder
                   :raw-input remaining
                   :parsed (str acc)))
          (recur c remaining (.append acc c))))
      (assoc holder :err "EOF while parse string"))))

(defn parse-value [raw-input]
  (let [holder (Holder. raw-input nil nil)]
    (let [c (first raw-input)]
      (cond
        (string? c) (parse-string holder)
        :else raw-input))))
