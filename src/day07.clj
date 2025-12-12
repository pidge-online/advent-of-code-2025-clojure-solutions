(ns day07
  (:require [clojure.string :as str]))

(def input-data (->> "resources/puzzle-inputs/day07.txt"
                     (slurp)
                     (str/split-lines)))

(defn create-splits-and-timelines [input-str]
  (let [lines (mapv #(vec (.getBytes %)) input-str)
        width (count (first lines))
        center (quot width 2)
        initial-timelines (assoc (vec (repeat width 0)) center 1)]
    (loop [splits 0
           timelines initial-timelines
           y 0
           rows (subvec lines 2)]
      (if (empty? rows)
        [splits (reduce + timelines)]
        (let [row (first rows)
              range-x (range (- center y) (inc (+ center y)) 2)
              [splits' timelines']
              (reduce (fn [[s t] x]
                        (let [count (t x)]
                          (if (and (> count 0) (= (nth row x) (byte \^)))
                            [(inc s)
                             (-> t
                                 (assoc x 0)
                                 (update (dec x) + count)
                                 (update (inc x) + count))]
                            [s t])))
                      [splits timelines]
                      range-x)]
          (recur splits' timelines' (inc y) (subvec rows 2)))))))

;; [p1 p2 solution]

(create-splits-and-timelines input-data)
