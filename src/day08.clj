(ns day08)
(require '[clojure.string :as str])
(require '[clojure.math :as math])

(def input-data (as-> "resources/puzzle-inputs/day08.txt" input
                  (slurp input)
                  (str/split input #"\n")
                  (mapv #(vec (str/split % #",")) input)
                  (mapv #(mapv (fn [x] (Integer/parseInt x)) %) input)))

input-data

(defn calculate-distance [[x y z] [x' y' z']]
  (math/pow
   (+ (math/pow (- x x') 2) (math/pow (- y y') 2) (math/pow (- z z') 2))
   0.5))

(defn take-three-largest-circuits [circuit-set]
  (vec (take 3 (reverse (sort-by count circuit-set)))))

(defn multiplied-size-of-largest-circuits [[c1 c2 c3]]
  (* (count c1) (count c2) (count c3)))

;; compare vs rest of list

;; socket in vec once complete

;; find neighbour in vec once done

(defn compare-junction-box-distances [input-boxes junction-box-to-compare box-number]
  (loop [remaining-boxes           (vec (concat (subvec input-boxes 0 box-number) (subvec input-boxes (inc box-number) 1000))); (vec (concat (subvec input-boxes 0 box-number) (subvec input-boxes (inc box-number) 1000)))
         current-closest-box       [0 0 0]
         current-shortest-distance 1000000000]
    (if (empty? remaining-boxes)
      (vec (sort [junction-box-to-compare current-closest-box]))
      (let [new-box (first remaining-boxes)
            new-distance (calculate-distance junction-box-to-compare new-box)]
        (if (< new-distance current-shortest-distance)
          (recur (rest remaining-boxes) new-box new-distance)
          (recur (rest remaining-boxes) current-closest-box current-shortest-distance))))))

(defn append-junction-box-to-coupling [current-couplings box]
  (update current-couplings (count current-couplings) (fn [_] box)))

(defn update-extant-box-couplings [current-couplings [a b]]
  (mapv #(cond
           (and (some #{a} %) (some #{b} %)) %
           (some #{a} %)                     (append-junction-box-to-coupling % b)
           (some #{b} %)                     (append-junction-box-to-coupling % a)
           :else %)                          current-couplings))

(defn update-or-append-new-coupling [current-couplings [a b]]
  (let [single-flattened-couplings (apply concat current-couplings)]
    (if (or (not (nil? (some #{a} single-flattened-couplings)))
            (not (nil? (some #{b} single-flattened-couplings))))
      (update-extant-box-couplings current-couplings [a b])
      (append-junction-box-to-coupling current-couplings [a b]))))

(defn iterate-boxes [box-amount]
  (if (< box-amount 0)
    []
    (update-or-append-new-coupling (iterate-boxes (dec box-amount)) (compare-junction-box-distances input-data (nth input-data box-amount) box-amount))))

(take-three-largest-circuits (iterate-boxes 999))

(multiplied-size-of-largest-circuits (take-three-largest-circuits (iterate-boxes 999)))

(count (first (take-three-largest-circuits (iterate-boxes 999))))

(iterate-boxes 999)