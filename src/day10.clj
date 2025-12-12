(ns advent-of-code-2025.day10
  (:require
   [clojure.core.reducers :as r]
   [clojure.math.combinatorics :as combo]
   [clojure.string :as str]
   [clojure.walk :as w]))

(defn binary-vector-addition [v u]
  (mapv #(if (= %1 %2)
           0
           1) v u))

(def input-data (->> "resources/puzzle-inputs/day10.txt"
                     (slurp)
                     (str/split-lines)
                     (mapv #(re-matches #"(\[.*\]) (\(.*\))\ (\{.*\})" %))))

(def input-indicator-light-diagrams
  (->> input-data
       (mapv #(get % 1))
       (mapv #(str/replace % #"\[|\]" {"[" "" "]" ""}))
       (mapv #(str/split % #""))
       (mapv #(mapv (fn [char] (str/replace char #".|#" {"." "0" "#" "1"})) %))
       (mapv #(mapv (fn [char] (Integer/parseInt char)) %))))

(def input-button-wiring-schematics
  (->> input-data
       (mapv #(get % 2))
       (mapv #(str/replace % #"\(|\)" {"(" "" ")" ""}))
       (mapv #(str/split % #" "))
       (mapv #(mapv (fn [s] (str/split s #",")) %))
       (w/postwalk #(if (string? %) (Integer/parseInt %) %))))

(def binary-button-wirings
  (let [zero-base-to-size (mapv #(vec (repeat % 0)) (mapv #(count %) input-indicator-light-diagrams))]
    (vec (for [x (range 0 (count input-button-wiring-schematics))]
           (let [buttons        (nth input-button-wiring-schematics x)
                 zeroed-buttons (nth zero-base-to-size x)
                 button-count   (count buttons)]
             (vec (sort (for [y              (range 0 button-count)]
                          (reduce #(assoc %1 %2 1) zeroed-buttons (get buttons y))))))))))

(def input-indicator-joltage-requirements
  (->> input-data
       (mapv #(get % 3))
       (mapv #(str/replace % #"\{|\}" {"{" "" "}" ""}))
       (mapv #(str/split % #","))
       (mapv #(mapv (fn [char] (Integer/parseInt char)) %))))

; p1 solution
(r/fold + (fn [cnt sub-coll]
            (+ cnt (count sub-coll))) (for [length (range 0 (count input-data))]
                                        (some
                                         #(when (= (nth input-indicator-light-diagrams length)
                                                   (reduce binary-vector-addition %)) %)
                                         (rest (combo/subsets (nth binary-button-wirings length))))))
