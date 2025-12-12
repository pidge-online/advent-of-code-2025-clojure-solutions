(ns day03
  (:require [clojure.string :as str]))

(def input-prep (as-> "resources/puzzle-inputs/day03.txt" input
                   (slurp input)
                   (str/split input #"\n")
                   (map #(str/split % #"") input)))

(defn two-battery-joltage [input]
  (let [sorted-batteries (reverse (sort input))
                largest-joltage (first sorted-batteries)]
            (if (apply distinct? (take 2 sorted-batteries))
              (let [substring-to-validate (str/split (str/join "" input) (re-pattern largest-joltage))
                    other-digit                      (first (reverse (sort (last substring-to-validate))))]
                (if (= (count substring-to-validate) 1)
                  (Integer/parseInt (str
                                     other-digit
                                     largest-joltage))
                  (Integer/parseInt (str
                                     largest-joltage
                                     other-digit))))
              (* 11 (Integer/parseInt largest-joltage)))))

;; p1 solution
(as-> input-prep input
  (map two-battery-joltage input)
  (apply + input))

