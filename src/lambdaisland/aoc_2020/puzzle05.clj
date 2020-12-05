(ns lambdaisland.aoc-2020.puzzle05
  (:require [clojure.java.io :as io]))

(def chars {\B 1 \F 0 \L 0 \R 1})

(def real-input (line-seq (io/reader (io/resource "puzzle_input_05.txt"))))

2r1000110
;; => 70
2r111
;; => 7

(defn bits->num [bits]
  (reduce (fn [num bit]
            (-> num
                (bit-shift-left 1)
                (bit-or bit)))
          0
          (map chars bits)))

(defn seat-id [code]
  (let [[row col] (map bits->num (partition-all 7 code))]
    (+ (* row 8) col)))

(def seat-ids (map seat-id real-input))

(apply max seat-ids)
;; => 908

(some (fn [[l h]]
        (when (= h (+ 2 l))
          (inc l)))
      (partition-all 2 1 (sort seat-ids)))
;; => 619
