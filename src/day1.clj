(ns day1)
(require '[clojure.string :as str])

(defn end-tick-increment [count value]
  (if (zero? value)
    (inc count)
    count))

(defn any-tick-increment [count rotation value]
  (+ count
     (abs (quot rotation 100))
     (if (neg? rotation)
       (if (and
            (not (zero? value))
            (>= (mod (abs rotation) 100) value))
         1
         0)
       (if (<= 100 (+ (mod rotation 100) value))
         1
         0))))

(defn tick-up
  ([input f] (tick-up input f 50 0))
  ([input f acc count]
   (let [rotation  (first input)
         res       (mod (+ acc rotation) 100)
         new-count (cond
                     (= f end-tick-increment) (end-tick-increment count res)
                     (= f any-tick-increment) (any-tick-increment count rotation acc))]
     (if (empty? (rest input))
       new-count
       (tick-up (rest input) f res new-count)))))

(def input-prep (as-> "resources/puzzle-inputs/day1.txt" input
                  (slurp input)
                  (str/replace input #"L|R" {"L" "-" "R" ""})
                  (str/split input #"\n")
                  (map Integer/parseInt input)))

;; p1 solution (stack overflow errors when not using lein as REPL)
(tick-up input-prep end-tick-increment)

;; p2 solution (stack overflow errors when not using lein as REPL)
(tick-up input-prep any-tick-increment)