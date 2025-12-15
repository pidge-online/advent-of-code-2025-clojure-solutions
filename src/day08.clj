(ns day08
  (:require
   [clojure.math :as math]
   [clojure.string :as str]))

(def input-data (as-> "resources/puzzle-inputs/day08.txt" input
                  (slurp input)
                  (str/split input #"\n")
                  (mapv #(vec (str/split % #",")) input)
                  (mapv #(mapv (fn [x] (Integer/parseInt x)) %) input)))

(defn sq [x] (math/pow x 2))

(defn squared-dist [[x1 y1 z1] [x2 y2 z2]]
  (+ (sq (- x1 x2))
     (sq (- y1 y2))
     (sq (- z1 z2))))

(defn all-pairs [xs]
  (let [v (vec xs)
        n (count v)]
    (for [i (range n)
          j (range (inc i) n)]
      [(v i) (v j)])))

(defn sorted-pairs [xs] (->> xs
                             (all-pairs)
                             (sort-by #(apply squared-dist %))))

(defn uf-init [xs]
  (zipmap xs xs))

(defn uf-find [uf x]
  (if (= (uf x) x)
    x
    (recur uf (uf x))))

(defn uf-union [uf a b]
  (let [ra (uf-find uf a)
        rb (uf-find uf b)]
    (if (= ra rb)
      uf
      (assoc uf ra rb))))

(defn uf-cluster-map [uf]
  (->> (keys uf)
       (group-by #(uf-find uf %))))

(def part1 
  (let [input input-data
        pairs (sorted-pairs input)
        uf0   (uf-init input)]
    (->> pairs
         (reductions (fn [uf [a b]]
                       (uf-union uf a b))
                     uf0)
         (map (fn [uf]
                (->> (uf-cluster-map uf)
                     vals
                     (map count)
                     (sort >)
                     (take 3)
                     (apply *))))
         (take 1000)
         last)))

(def part2 
  (let [input input-data
        pairs (sorted-pairs input)
        uf0   (uf-init input)
        index (->> pairs
                   (reductions (fn [uf [a b]]
                                 (uf-union uf a b))
                               uf0)
                   (take-while #(> (count (uf-cluster-map %)) 1))
                   count)
        [[x1 _ _] [x2 _ _]] (nth pairs (dec index))]
    (* x1 x2)))

(defn -main []
  (println part1)
  (println part2))

(-main)
