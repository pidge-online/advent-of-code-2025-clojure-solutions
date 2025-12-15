(ns day05
  (:require
   [clojure.string :as str]))

(def input-ranges (as-> "resources/puzzle-inputs/day05-ranges.txt" input
                    (slurp input)
                    (str/split input #"\n")
                    (mapv #(str/split % #"-") input)
                    (mapv #(mapv (fn [x] (Long/parseLong x)) %) input)
                    (vec (sort input)))) ;; drop if cuases issues during res

(def input-ids (as-> "resources/puzzle-inputs/day05-ids.txt" input
                 (slurp input)
                 (str/split input #"\n")
                 (mapv #(Long/parseLong %) input)
                 (sort input)))

(defn ids-in-ranges
  ([ids ranges] (ids-in-ranges ids ranges 0))
  ([ids ranges acc]
   (cond
     (empty? ids) acc
     (empty? ranges) (recur (rest ids) input-ranges acc)
     :else (let [[lower-range-bound upper-range-bound] (first ranges)
                 id                                    (first ids)]
             (if (and
                  (> id lower-range-bound)
                  (< id upper-range-bound))
               (recur (rest ids) input-ranges (+ acc 1))
               (recur ids (rest ranges) acc))))))

(defn adjust-range-bounds-for-overlap [ranges]
  (loop [remaining-ranges (rest ranges)
         [cur-start cur-end] (first ranges)
         merged-ranges []]
    (if (empty? remaining-ranges)
      (conj merged-ranges [cur-start cur-end])
      (let [[start end] (first remaining-ranges)]
        (if (<= start (inc cur-end))
          (recur (rest remaining-ranges) [cur-start (max cur-end end)] merged-ranges)
          (recur (rest remaining-ranges) [start end] (conj merged-ranges [cur-start cur-end])))))))

(def part1 (ids-in-ranges input-ids input-ranges))

(def part2 (->> (adjust-range-bounds-for-overlap input-ranges)
                (mapv (fn [[a b]] (+ (- b a) 1)))
                (reduce +)))

(defn -main []
  (println part1)
  (println part2))

(-main)
