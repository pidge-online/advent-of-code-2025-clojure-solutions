(ns day03
  (:require
   [clojure.string :as str]))

(def input-data (->> (slurp "resources/puzzle-inputs/day03.txt")
                  (str/split-lines)
                  (map #(str/split % #""))))

(defn get-joltage-for-n-digits [n input-data]
  (loop [remaining-digits          n
         string                    input-data
         current-joltage           ""
         sorted-int-set            (apply sorted-set-by #(compare %2 %1) string)]
    (if (= 0 remaining-digits)
      (Long/parseLong current-joltage)
      (let [largest-int               (str (first sorted-int-set))
            remaining-ints-post-split (second (str/split (str/join "" string) (re-pattern largest-int) 2))]
        (if (>= (count remaining-ints-post-split) (dec remaining-digits))
          (recur
           (dec remaining-digits)
           (str/split  remaining-ints-post-split #"")
           (str current-joltage largest-int)
           (apply sorted-set-by #(compare %2 %1) remaining-ints-post-split))
          (recur
           remaining-digits
           string
           current-joltage
           (rest sorted-int-set)))))))

(defn sum-total-joltages [n] (->> input-data
                                  (map #(get-joltage-for-n-digits n %))
                                  (apply +)))

(def part1 (sum-total-joltages 2))

(def part2 (sum-total-joltages 12))

(defn -main []
  (println part1)
  (println part2))

(-main)
