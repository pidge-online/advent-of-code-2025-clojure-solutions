(ns day3)
(require '[clojure.string :as str])

(def input-prep (as-> "resources/puzzle-inputs/day3.txt" input
                   (slurp input)
                   (str/split input #"\n")
                   (map #(str/split % #"") input)
                   ))

;; p1 solution
(as-> input-prep input
  (map (fn [x] (let [sorted-batteries (reverse (sort x))
                     largest-joltage (first sorted-batteries)]
                 (if (apply distinct? (take 2 sorted-batteries))
                   (let [substring-to-validate (str/split (str/join "" x) (re-pattern largest-joltage))] 
                     (if (= (count substring-to-validate) 1) 
                       (Integer/parseInt (str
                                          (first (reverse (sort (last substring-to-validate))))
                                          largest-joltage))
                       (Integer/parseInt (str
                                          largest-joltage
                                          (first (reverse (sort (last substring-to-validate)))))))) 
                     (* 11 (Integer/parseInt largest-joltage)))))
       input)
  (apply + input))

