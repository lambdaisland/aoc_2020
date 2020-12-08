(ns lambdaisland.aoc-2020.puzzle05
  (:require [clojure.java.io :as io]))

(def chars {\B 1 \F 0 \L 0 \R 1})

(def real-input (line-seq (io/reader (io/resource "puzzle_input_05.txt"))))

2r1000110
;; => 70
2r111
;; => 7

(defn bits->num [bits]
  (reduce #(+ (* %1 2) %2) 0 (map chars bits)))

(def seat-ids (map bits->num real-input))

(apply max seat-ids)
;; => 908

(some (fn [[l h]]
        (when (= h (+ 2 l))
          (inc l)))
      (partition-all 2 1 (sort seat-ids)))
;; => 619

(def bits [0 1 0 1 0 1])

(reduce #(bit-or (bit-shift-left %1 1) %2) 0 bits)
