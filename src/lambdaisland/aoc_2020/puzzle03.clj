(ns src.lambdaisland.aoc-2020.puzzle03
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def demo-input "..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#")

(def real-input (slurp (io/resource "puzzle_input_03.txt")))

(defn input->map [input]
  (mapv (fn [row]
          (mapv {\# true \. false} row))
        (str/split input #"\n")))

(defn tree? [m x y]
  (let [width (count (first m))]
    (get-in m [y (mod x width)])))

(defn sled [[down-x down-y] [my-map x y trees]]
  (let [x (+ x down-x)
        y (+ y down-y)
        tree? (tree? my-map x y)]
    (cond
      (nil? tree?)
      (reduced trees)

      (true? tree?)
      [my-map x y (inc trees)]

      :else
      [my-map x y trees])))


(defn sled-down [slope input]
  @(first
    (drop-while
     (complement reduced?)
     (iterate (partial sled slope) [(input->map input) 0 0 0]))))

(sled-down [3 1] demo-input)
;; => 7

(sled-down [3 1] real-input)
;; => 164

(def slopes [[1 1]
             [3 1]
             [5 1]
             [7 1]
             [1 2]])

(time apply * (for [s slopes]
                (sled-down s real-input))))
;; => 5007658656
