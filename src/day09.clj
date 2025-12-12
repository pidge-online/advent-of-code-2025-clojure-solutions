(ns day09
  (:require '[clojure.string :as str]))

(defn calculate-area [[x y] [x' y']]
  (abs (*
        (+ (abs (- y' y)) 1)
        (+ (abs (- x' x)) 1))))

(defn generate-co-ord-comparisons [[x y] [x' y']] ; producing reversed comparison pairs for determining presence of other validating grid values
  (let [xprime-leading-comparison [(if (> x' x)
                                     [>= <=]
                                     [<= >=])
                                   (if (> y y')
                                     [>= <=]
                                     [<= >=])]]
    [[(get (first xprime-leading-comparison) 0) (get (last xprime-leading-comparison) 0)] 
     [(get (first xprime-leading-comparison) 1) (get (last xprime-leading-comparison) 1)]]))

(defn produce-in-bounds-status-list-per-co-ord [input-tiles [x y] [x' y']]
  (let [[[x'-dir y-dir] [x-dir y'-dir]] (generate-co-ord-comparisons [x y] [x' y'])]
    (vec (for [r (range 0 (count input-tiles))]
           (let [[a b] (get input-tiles r)]
             [(if (and
                   (x'-dir a x')
                   (y-dir b y))
                true
                false)
              (if (and
                   (x-dir a x)
                   (y'-dir b y'))
                true
                false)])))))

(defn verify-bounded-by-other-co-ords? [nested-status-list]
  (and (not (nil? (some #{[false true]} nested-status-list)))
       (not (nil? (some #{[true false]} nested-status-list)))))

(defn invalid-by-concave-path-passthrough? [[x y] [x' y'] [[a b] [a' b']]]
  (let [lx (max x x') ; l - largest
        sx (min x x') ; s - smallest
        ly (max y y')
        sy (min y y')
        la (max a a')
        sa (min a a')
        lb (max b b')
        sb (min b b')]
    (if (or
         (and (> lx la sx) (> lx sa sx) (or (> lb sy sb) (> lb ly sb))) ; does the pair of points cross through a horizontal rectangle bound
         (and (> ly lb sy) (> ly sb sy) (or (> la sx sa) (> la lx sa))))  ; does the pair of points cross through a vertical rectangle bound
      true
      false)))

(defn verify-coordinates-not-passed-through? [[x y] [x' y'] step-pathing]
  (loop [steps step-pathing
         out-of-bounds-detected? (invalid-by-concave-path-passthrough? [x y] [x' y'] (first steps))]
    (cond
      out-of-bounds-detected?      false 
      (= 1 (count steps))          true
      :else                        (let [remaining-steps (rest steps)
                                         next-out-of-bounds? (invalid-by-concave-path-passthrough? [x y] [x' y'] (first remaining-steps))]
                                     (recur remaining-steps next-out-of-bounds?)))))

(defn verify-and-calculate-new-area [original-tiles [x y] [x' y'] step-pathing]
  (if (and 
       (verify-bounded-by-other-co-ords? (produce-in-bounds-status-list-per-co-ord original-tiles [x y] [x' y']))
       (verify-coordinates-not-passed-through? [x y] [x' y'] step-pathing)
       true)
    (calculate-area [x y] [x' y'])
    0))

(defn compare-areas
  ([input-tiles] (compare-areas input-tiles false false))
  ([input-tiles original-tiles step-pathing]
   (loop [[x y] (first input-tiles)
          tile-number 1
          current-max-area (calculate-area [x y] (nth input-tiles 1))]
     (if (= (inc tile-number) (count input-tiles))
       current-max-area
       (let [next-tile-number (inc tile-number)
             [x' y']          (nth input-tiles next-tile-number)
             new-area         (if step-pathing
                                (verify-and-calculate-new-area original-tiles [x y] [x' y'] step-pathing)
                                (calculate-area [x y] (nth input-tiles next-tile-number)))]
         (if (> new-area current-max-area)
           (recur [x y] next-tile-number new-area)
           (recur [x y] next-tile-number current-max-area)))))))

(defn iterate-through-tiles ([input-tiles] (iterate-through-tiles input-tiles false)) 
  ([input-tiles step-pathing]
  (let [inital-max (if step-pathing
                     (compare-areas input-tiles input-tiles step-pathing)
                     (compare-areas input-tiles))
        total-tiles (count input-tiles)]
    (loop [n 0
           max-area inital-max]
      (if (= (+ n 2) total-tiles)
        max-area
        (let [new-area (if step-pathing
                         (compare-areas (subvec input-tiles (inc n) total-tiles) input-tiles step-pathing)
                         (compare-areas (subvec input-tiles (inc n) total-tiles)))
              new-max-area (max max-area new-area)]
          (recur (inc n) new-max-area)))))))

(def input-data (->> "resources/puzzle-inputs/day09.txt"
                  (slurp)
                  (str/split-lines)
                  (mapv #(vec (str/split % #",")))
                  (mapv #(mapv (fn [x] (Integer/parseInt x)) %))))

; p1 solution
(iterate-through-tiles input-data)

; p2 solution
(def tile-pathing-steps (vec (map vec (partition 2 1 input-data))))
(iterate-through-tiles input-data tile-pathing-steps)


