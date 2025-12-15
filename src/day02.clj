(ns day02
  (:require 
   [clojure.string :as str]))

(defn repeating-subsequence-compsed-string? [regex]
  (fn [x] (let [str-x (str x)]
              (= str-x
                 (first (re-find (re-pattern regex) str-x))))))

(defn calculate-total [input f]
  (->> input
       (map #(filter f %))
       (map #(apply + %))
       (reduce +)))

(def input-prep (as-> "resources/puzzle-inputs/day02.txt" input
                  (slurp input)
                  (str/split input #",")
                  (map #(str/split % #"-") input)
                  (map #(mapv (fn [x] (Long/parseLong x)) %) input)
                  (map (fn [[a b]] (range a (inc b))) input)))

(defn -main []
  (println (calculate-total input-prep (repeating-subsequence-compsed-string? "^(.+)\\1$")))
  (println (calculate-total input-prep (repeating-subsequence-compsed-string? "^(.+)\\1+$"))))

(-main)
