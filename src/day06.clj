(ns day06
   (:require
    [clojure.string :as str]
    [clojure.walk :as w]))

(defn transpose [data]
  (apply (partial mapv vector) data))

(defn sum-from-symbol-and-ints [[operation args]]
  (apply (resolve (symbol operation)) args))

(defn zeroed-cephalid-string-vec-to-standard-int-vec [v]
  (mapv #(apply str %) (vec (for [x (range 0 (count (first v)))]
                              (vec (for [y (range 0 (count v))]
                                     (get-in v [y x])))))))

(defn extract-exact-length-digit-inputs [s length-counts]
  (loop [parsed       []
         unparsed     s
         split-counts length-counts]
    (if (empty? split-counts)
      parsed
      (let [[next-int remainder-to-parse] (mapv #(apply str %) (split-at (first split-counts) unparsed))]
        (recur (conj parsed next-int) (rest remainder-to-parse) (rest split-counts))))))

(defn attach-operations-to-string-list-then-sum-operation-results [operations v] (->> v
                                                                           (w/postwalk #(if (string? %) (Integer/parseInt %) %))
                                                                           (mapv #(conj [%1] %2) operations)
                                                                           (mapv #(sum-from-symbol-and-ints %))
                                                                           (reduce +)))

(defn extract-int-rows [v] (->> (take 4 v)
                                (transpose)))

(def raw-single-string-digit-vector (-> (slurp "resources/puzzle-inputs/day06.txt")
                                        (str/split-lines)))

(def input-data (->> raw-single-string-digit-vector
                     (mapv #(str/split % #"\s+"))
                     (w/postwalk #(if (vector? %) (vec (remove empty? %)) %))))

(def operation-list (->> (last input-data)
                         (transpose)
                         (first)
                         (mapv #(str %))))

(def largest-digit-length-each-sum (->> (extract-int-rows input-data)
                                        (mapv #(apply max (mapv (fn [s] (count s)) %)))))

(def part1 (->> (extract-int-rows input-data)
                (attach-operations-to-string-list-then-sum-operation-results operation-list)))

(def part2 (->> raw-single-string-digit-vector
                (mapv #(extract-exact-length-digit-inputs % largest-digit-length-each-sum))
                (extract-int-rows)
                (mapv #(zeroed-cephalid-string-vec-to-standard-int-vec %))
                (mapv #(mapv (fn [s] (str/replace s #"\s" "")) %))
                (attach-operations-to-string-list-then-sum-operation-results operation-list)))

(defn -main []
  (println part1)
  (println part2))

(-main)
