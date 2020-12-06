(ns pranav-puzzle05
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def demo-input "BFFFBBFRRR
FFFBBBFRRR
BBFFBBFRLL")

(defn floor-int [n]
  (int (Math/floor n)))

(defn dig [char [n1 n2]]
  (if (#{\L \F} char)
    [n1 (+ n1 (quot (- n2 n1) 2))]
    [(- n2 (quot (- n2 n1) 2)) n2]))

(defn get-n [str init]
  (reduce #(dig %2 %1) init str)
  #_(loop [[c & cs] str
           i init]
      (if c
        (recur cs (dig c i))
        (first i))))

(defn get-seat [str]
  (let [[rs cs] (partition-all 7 str)
        ri [0 127]
        ci [0 7]]
    (+ (* 8 (get-n rs ri)) (get-n cs ci))))

(get-seat "BBFFBBFRLL")

(apply max (map get-seat (str/split demo-input #"\n")))
;; => 820

(def input (line-seq (io/reader (io/resource "puzzle_input_05.txt"))))

(apply max (map get-seat input))
;; => 908

;; ; Part 2

(def sorted-nums (sort (map get-seat input)))

(- (reduce +
           (range
            (first sorted-nums)
            (inc (last sorted-nums))))
   (reduce + sorted-nums))
;; => 619
