(ns day11
  (:require
   [clojure.string :as str]
   [clojure.walk :as w]))

(def input-data (as-> "resources/puzzle-inputs/day11.txt" input
                  (slurp input)
                  (str/split-lines input)
                  (mapv #(str/split % #":") input)
                  (mapv (fn [[a b]] (vector a (str/split (str/trim b) #"\s"))) input)
                  (w/postwalk #(if (= % "out") 1 %) input)))

(def sample-input (->> "resources/puzzle-inputs/day11-sample.txt"
                       (slurp)
                       (str/split-lines)
                       (mapv #(str/split % #":"))
                       (mapv (fn [[a b]] (vector a (str/split (str/trim b) #"\s"))))
                       (w/postwalk #(if (= % "out") 1 %))))

(defn extract-values-from-start [data-source starting-key] ; needs to be transposed
  (loop [initial-values (get-in data-source starting-key)
         all-values-out? (= (set initial-values) #{1})]  ; use contains and distinct
    (if all-values-out?
      (count initial-values)
      (let [new-values (vec (flatten (for [val initial-values]
                                       (if (= val 1)
                                         1
                                         (get-in data-source [val])))))
            all-values-out-new? (= (set new-values) #{1})]
        (recur new-values all-values-out-new?)))))

;; p1 solution
(as-> input-data input
  (apply (partial mapv vector) input)
  (zipmap (get-in input [0]) (get-in input [1])) ; get hashmap of values to keys
  (extract-values-from-start input ["you"]))
