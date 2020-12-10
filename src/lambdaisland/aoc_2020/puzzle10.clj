(ns lambdaisland.aoc-2020.puzzle10
  (:require [lambdaisland.aoc-2020.util :as util]))

(set! *unchecked-math* :warn-on-boxed)

(def demo-input [16 10 15 5 1 11 7 19 6 12 4])
(def demo-input2 [28 33 18 42 31 14 46 20 48 47 24 23 49 45 19 38 39 11 1 32 25 35 8 17 7 9 4 2 34 10 3])

(def real-input (util/num-resource-seq 10))

(defn part1 [input]
  (let [device-joltage (+ 3 (apply max input))
        full-input (conj input 0 device-joltage)
        {ones 1 threes 3 :as diffs} (->> full-input
                                         sort
                                         (partition 2 1)
                                         (map #(- (second %) (first %)))
                                         frequencies)]
    (* ones threes)))

(part1 demo-input)
;; => 35

(part1 demo-input2)
;; => 220

(time
 (part1 real-input))
;; => 2263

(defn remove-index [v ^long idx]
  (into (subvec v 0 idx) (subvec v (inc idx) (count v))))

(defn can-be-removed? [v ^long idx]
  (let [left (nth v (dec idx))
        right (nth v (inc idx))]
    (and left right (<= (- (long right) (long left)) 3))))

(defn single-pass [full-input]
  (for [idx (range (count full-input))
        :when (can-be-removed? full-input idx)]
    (remove-index full-input idx)))

#_(defn part2 [input]
    (let [full-input (vec (sort (conj input 0 (+ 3 (apply max input)))))]
      (loop [input     [full-input]
             result    1]
        (let [this-pass (mapcat single-pass input)]
          #_(clojure.pprint/pprint this-pass)
          (if (seq this-pass)
            (recur this-pass (+ result (count this-pass)))
            result)))))

(defn add-boundaries [input]
  (vec (sort (conj input 0 (+ 3 (long (apply max input)))))))

(comment
  (part2 demo-input)
  ;; => 16
  (part2 demo-input2))

(defn combos ^long [input ^long start]
  (loop [cnt 1
         idx start]
    (cond
      (<= (count input) 2)
      cnt

      (<= (dec (count input)) idx)
      cnt

      (can-be-removed? input idx)
      (let [removed (remove-index input idx)]
        (recur (+ cnt (combos removed idx))
               (inc idx)))

      :else
      (recur cnt (inc idx)))))

(defn part2 [input]
  (combos (vec (sort (add-boundaries input))) 0))

(comment
  (part2 demo-input)
  ;; => 8
  (time (part2 demo-input2))
  ;; => 19208
  (time (part2 real-input))
  )

(defn divide [input]
  (let [split-points (remove #(can-be-removed? input %) (range 1 (dec (count input))))]
    (if (seq split-points)
      (let [split-idx (long (nth split-points (quot (count split-points) 2)))
            left (take (inc split-idx) input)
            right (drop split-idx input)]
        [left right])
      (combos (vec input) 1))))

(defn conquer [input]
  (let [parts (divide input)]
    (if (number? parts)
      parts
      (apply * (map conquer parts)))))

(time
 (let [input (add-boundaries real-input)]
   (conquer input)))
;; => 396857386627072
