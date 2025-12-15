(ns day06
  (:require 
   [clojure.string :as str]))

(defn transpose [data]
  (apply (partial mapv vector) data))

(defn perfom-math [[args operation]]
  (apply (resolve (symbol operation)) args))

(defn strings-to-ints-and-symbol
  ([[a b c d operation]] (strings-to-ints-and-symbol [a b c d operation] identity))
  ([[a b c d operation] f]
   (vector (mapv #(Integer/parseInt %) (f [a b c d])) (symbol operation))))

(def input-data (as-> "resources/puzzle-inputs/day06.txt" input
                  (slurp input)
                  (str/split input #"\n")
                  (mapv #(str/split % #"\s+") input)
                  (transpose input))) ; transpose nested vectors

(def part1 (->> input-data
                (mapv #(strings-to-ints-and-symbol %))
                (mapv #(perfom-math %))
                (reduce +)))

(def part2 "part 2 TBD")

(defn -main []
  (println part1)
  (println part2))

(-main)
