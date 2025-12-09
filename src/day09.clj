(ns day09)
(require '[clojure.string :as str])
(require '[clojure.math :as math])


(def input-data (as-> "resources/puzzle-inputs/day09.txt" input
                  (slurp input)
                  (str/split input #"\n")
                  (mapv #(vec (str/split % #",")) input)
                  (mapv #(mapv (fn [x] (Integer/parseInt x)) %) input)))

(defn calculate-area [[x y] [x' y']]
  (abs (*
        (+ (abs (- y' y)) 1)
        (+ (abs (- x' x)) 1))))

(defn compare-areas [input-tiles]
  (loop [first-tile (first input-tiles)
         tile-number 1
         current-max-area (calculate-area first-tile (nth input-tiles 1))]
    (if (= (inc tile-number) (count input-tiles))
      current-max-area
      (let [next-tile-number (inc tile-number)
            new-area (calculate-area first-tile (nth input-tiles next-tile-number))]
        (if (> new-area current-max-area)
          (recur first-tile next-tile-number new-area)
          (recur first-tile next-tile-number current-max-area))))))

(defn iterate-through-tiles [input-tiles]
  (let [inital-max (compare-areas input-tiles)
        total-tiles (count input-tiles)]
    (loop [n 0
           max-area inital-max]
      (if (= (+ n 2) total-tiles)
        max-area
        (let [new-area (compare-areas (subvec input-tiles (inc n) total-tiles))
              new-max-area (max max-area new-area)]
          (recur (inc n) new-max-area))))))

; p1 solution
(iterate-through-tiles input-data)

(defn generate-co-ord-comparisons [[x y] [x' y']]
  (let [xprime-leading-comparison [(if (> x' x)
                                     >=
                                     <=)
                                   (if (> y y')
                                     >=
                                     <=)]
        yprime-leading-comparison (reverse xprime-leading-comparison)]
    [xprime-leading-comparison yprime-leading-comparison]))

(defn produce-in-bounds-status-list-per-co-ord [input-tiles [x y] [x' y']]
  (let [[[x'-dir y-dir] [x-dir y'-dir]] (generate-co-ord-comparisons [x y] [x' y'])]
    (vec (for [r (range 0 (count input-tiles))]
           (let [[a b] (get input-tiles r)]
             [[a b]
              (if (and
                   (x'-dir a x')
                   (y-dir b y))
                true
                false)
              (if (and
                   (x-dir a x)
                   (y'-dir b y'))
                true
                false)])))))

(defn verify-in-bounds-co-ord? [nested-status-list]
  (and (not (nil? (some #{[false true]} nested-status-list)))
       (not (nil? (some #{[true false]} nested-status-list)))))

(defn compare-areas-in-bounds [input-tiles original-tiles]
  (loop [[x y] (first input-tiles)
         tile-number 1
         current-max-area (calculate-area [x y] (nth input-tiles 1))]
    (if (= (inc tile-number) (count input-tiles))
      current-max-area
      (let [next-tile-number (inc tile-number)
            [x' y'] (nth input-tiles next-tile-number)
            new-area (if (verify-in-bounds-co-ord? (produce-in-bounds-status-list-per-co-ord original-tiles [x y] [x' y']))
                       (calculate-area [x y] [x' y'])
                       0)]
        (if (> new-area current-max-area)
          (recur [x y] next-tile-number new-area)
          (recur [x y] next-tile-number current-max-area))))))

(defn iterate-through-in-bounds-tiles [input-tiles]
  (let [inital-max (compare-areas-in-bounds input-tiles input-tiles)
        total-tiles (count input-tiles)]
    (loop [n 0
           max-area inital-max]
      (if (= (+ n 2) total-tiles)
        max-area
        (let [new-area (compare-areas-in-bounds (subvec input-tiles (inc n) total-tiles) input-tiles)
              new-max-area (max max-area new-area)]
          (recur (inc n) new-max-area))))))


; p2 solution
(iterate-through-in-bounds-tiles input-data)

(def sample-data (as-> "7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3" input
                   (str/split input #"\n")
                   (mapv #(vec (str/split % #",")) input)
                   (mapv #(mapv (fn [x] (Integer/parseInt x)) %) input)))

(assert (= 24 (iterate-through-in-bounds-tiles sample-data))) ; nil means assertion is true, otherwise throws assertion error

;; current too large value co-ords [95990 63461] [4061 37808]
;; ([[3836 64177] true false])
;; ([[96087 37113] false true])
;; not accounted for concave nature of polygon
;; need to find maximum spanning rectangle area inside concave polygon algorithm

