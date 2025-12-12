(ns day12
  (:require
   [clojure.string :as str]
   [clojure.walk :as w]))

(def shapes (as-> "resources/puzzle-inputs/day12-shapes.txt" shapes
              (slurp shapes)
              (str/split shapes #"\n\n")
              (mapv #(str/split % #":") shapes)
              (mapv (fn [[_ shape]] shape) shapes)
              (mapv #(str/split (str/trim %) #"\n") shapes)
              (w/postwalk #(if (string? %) (str/replace % #"\.|#" {"." "0" "#" "1"}) %) shapes)
              (w/postwalk #(if (string? %) (mapv (fn [i] (Integer/parseInt i)) (str/split % #"")) %) shapes)
              ))

(def combinations (->> "resources/puzzle-inputs/day12-combinations.txt" 
                    (slurp)
                    (str/split-lines)
                    (mapv #(str/split % #":"))))

(def grids (->> combinations 
                (mapv (fn [[size _]] size))
                (mapv #(str/split % #"x"))
                (w/postwalk #(if (string? %) (Integer/parseInt %) %))))

(def arrangements (->> combinations
                       (mapv (fn [[_ arrangements]] arrangements))
                       (mapv #(str/split (str/trim %) #" ")) 
                       (w/postwalk #(if (string? %) (Integer/parseInt %) %))))

(def total-tile-comsumptions-per-shape
   (mapv #(reduce + (flatten %)) shapes))

; p1 solution
(reduce + (for [x (range 0 1000)]
         (if (>= (apply * (get grids x)) (reduce + (mapv * (get arrangements x) total-tile-comsumptions-per-shape)))
           1
           0))) ; produces answer, doesn't consider edge cases but works
