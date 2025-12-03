(ns day2)
(require '[clojure.string :as str])

(defn repeating-subsequence-compsed-string? [part]
  (let [regex (if (= part "p1")
                #"^(.+)\1$"
                #"^(.+)\1+$")]
    (fn [x] (let [str-x (str x)]
              (= str-x
                 (first (re-find regex str-x)))))))

(defn calculate-total [input f]
  (->> input
       (map #(filter f %))
       (map #(apply + %))
       (reduce +)))

(def input-prep (as-> "resources/puzzle-inputs/day2.txt" input
                  (slurp input)
                  (str/split input #",")
                  (map #(str/split % #"-") input)
                  (map #(mapv (fn [x] (Long/parseLong x)) %) input)
                  (map (fn [[a b]] (range a (inc b))) input)))

;; p1 solution
(calculate-total input-prep (repeating-subsequence-compsed-string? "p1"))

;; p2 solution
(calculate-total input-prep (repeating-subsequence-compsed-string? "p2"))