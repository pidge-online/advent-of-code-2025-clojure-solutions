(ns day04
  (:require 
   [clojure.string :as str]))

;; data loading and prep
(def input-data (as-> "resources/puzzle-inputs/day04.txt" input
                  (slurp input)
                  (str/split input #"\n")
                  (mapv #(str/replace % #".|@" {"." "0" "@" "1"}) input)
                  (mapv #(str/split % #"") input)
                  (mapv #(mapv (fn [i] (Integer/parseInt i)) %) input))) 

(def rows-of-rolls (count input-data)) ; needed to iter across

(defn roll-existence-co-ord-check [[y x] n grid] ; rotating round from north clockwise to check adjacent rolls
  (let [found-value (case n
                      0 (get-in grid [(dec y) x])
                      1 (get-in grid [(dec y) (inc x)])
                      2 (get-in grid [y  (inc x)])
                      3 (get-in grid [(inc y)  (inc x)])
                      4 (get-in grid [(inc y) x])
                      5 (get-in grid [(inc y) (dec x)])
                      6 (get-in grid [y (dec x)])
                      7 (get-in grid [(dec y) (dec x)]))]
    (if (some? found-value)
      found-value
      0)))

(defn accessible-toilet-roll? [y x grid] 
  (loop [adjactent-reachable-total 0
         items-checked 0]
    (if (or
         (= adjactent-reachable-total 4)
         (= items-checked 8))
      (if (zero? (quot adjactent-reachable-total 4)) ; if (= 0 (quot adj 4)) then less than 4 rolls nearby
        true
        false)
      (let [new-adjactent-reachable-total (+ adjactent-reachable-total (roll-existence-co-ord-check [y x] items-checked grid))
            new-items-checked (inc items-checked)]
        (recur new-adjactent-reachable-total new-items-checked)))))

(defn accessible-roll-state [grid]
  (vec (for [y (range 0 rows-of-rolls)]
         (vec (for [x (range 0 rows-of-rolls)]
                (if (and
                     (pos? (get-in grid [y x]))
                     (accessible-toilet-roll? y x grid)) 1 0)))))) 

(defn update-roll-state [input-state new-state]
  (vec (for [y (range 0 rows-of-rolls)]
         (vec (for [x (range 0 rows-of-rolls)]
                (- (get-in input-state [y x]) (get-in new-state [y x])))))))

(defn count-total-accessible-rolls [grid]
  (reduce + (flatten grid)))

(defn calculate-final-role-state
  ([input] (calculate-final-role-state input (count-total-accessible-rolls input) 0))
  ([input count acc]
   (let [accessible-roll-state (accessible-roll-state input)
         accessible-count      (count-total-accessible-rolls accessible-roll-state)
         next-role-state       (update-roll-state input accessible-roll-state)
         new-count             (count-total-accessible-rolls next-role-state)]
     (if (= count new-count)
       next-role-state
       (recur next-role-state new-count (+ acc accessible-count))))))

(def part1 (-> input-data
               (accessible-roll-state)
               (count-total-accessible-rolls)))

(def part2 (- (count-total-accessible-rolls input-data)
              (count-total-accessible-rolls (calculate-final-role-state input-data))))

(defn -main []
  (println part1)
  (println part2))

(-main)
