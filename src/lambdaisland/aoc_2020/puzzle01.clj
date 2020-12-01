(ns lambdaisland.aoc-2020.puzzle01
  (:require [clojure.java.io :as io]))

(def demo-input [1721
                 979
                 366
                 299
                 675
                 1456])

(def input
  (map #(Long/parseLong %)
       (line-seq (io/reader (io/resource "puzzle_input_01.txt")))))

(set
 (for [x input
       y input
       :when (= 2020 (+ x y))]
   (* x y)))
;; => #{73371}
;; => #{514579}

(set
 (for [x input
       y input
       z input
       :when (= 2020 (+ x y z))]
   (* x y z)))
;; => #{127642310}
