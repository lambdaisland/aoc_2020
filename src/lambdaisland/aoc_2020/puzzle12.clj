(ns lambdaisland.aoc-2020.puzzle12
  (:require [clojure.string :as str]
            [lambdaisland.aoc-2020.util :as util]))

(def demo-input "F10
N3
F7
R90
F11")

(def pattern #"(\w)(\d+)")

(defn parse-demo [d]
  (map (comp
        (fn [[i n]]
          [i (util/parse-long n)])
        #(next (re-find pattern %)))
       (str/split-lines d)))

(def demo-parsed (parse-demo demo-input))

(def real-parsed (map (fn [[i n]]
                        [i (util/parse-long n)])
                      (util/re-resource-seq 12 #"(\w)(\d+)")))

demo-parsed
real-parsed

;; (require 'clojure.set)
;; (clojure.set/map-invert
;;  {"N" "E" "E" "S" "S" "W" "W" "N"})

(defn handle-instruction [[x y d :as state] [i n]]
  (case i
    "N" [(+ x n) y d]
    "E" [x (+ y n) d]
    "S" [(- x n) y d]
    "W" [x (- y n) d]
    "R" (case n
          90  [x y ({"N" "E" "E" "S" "S" "W" "W" "N"} d)]
          180 [x y ({"N" "S" "S" "N" "E" "W" "W" "E"} d)]
          270 [x y ({"E" "N", "S" "E", "W" "S", "N" "W"} d)])
    "L" (case n
          90  [x y ({"E" "N", "S" "E", "W" "S", "N" "W"} d)]
          180 [x y ({"N" "S" "S" "N" "E" "W" "W" "E"} d)]
          270 [x y ({"N" "E" "E" "S" "S" "W" "W" "N"} d)])
    "F" (handle-instruction state [d n])))

(def start [0 0 "E"])

(defn manhatten [[x y]]
  (+ (Math/abs x) (Math/abs y)))

(manhatten
 (reduce handle-instruction start demo-parsed))
;; => 25
;; => [-8 17 "S"]

(manhatten
 (reduce handle-instruction start real-parsed))
;; => 1589

;; [0 0] [1 10]
;; F10 [10 100] [1 10]
;; N3  [10 100] [4 10]
;; F7  [38 170] [4 10]
;; R90 [38 170] [-10 4]
;; F11 [(+ 38 (* 11 -10)) (+ 170 (* 11 4))];; => [-72 214]

"N" [(+ x n) y d]
"E" [x (+ y n) d]
"S" [(- x n) y d]
"W" [x (- y n) d]

(defn handle2 [[[x y] [wx wy] :as state] [i n]]
  (case i
    "N" [[x y] [(+ wx n) wy]]
    "E" [[x y] [wx (+ wy n)]]
    "S" [[x y] [(- wx n) wy]]
    "W" [[x y] [wx (- wy n)]]
    "R" (case n
          90  [[x y] [(- wy) wx]]
          180 [[x y] [(- wx) (- wy)]]
          270 [[x y] [wy (- wx)]])
    "L" (case n
          90  [[x y] [wy (- wx)]]
          180 [[x y] [(- wx) (- wy)]]
          270 [[x y] [(- wy) wx]])
    "F" [[(+ x (* wx n)) (+ y (* wy n))] [wx wy]]))

(defn debug [f]
  (fn [state [i n]]
    (let [res (f state [i n])]
      (println (format "%-4s %s %s" (str i n) (pr-str (vec (reverse (first res)))) (pr-str (vec (reverse (second res))))))
      res)))

(def wp-start [1 10])

(reduce handle2 [[0 0] wp-start] demo-parsed)
;; => [[-72 214] [-10 4]]
;; => [[-1062 844] [-100 4]]

(manhatten (first (reduce handle2 [[0 0] wp-start] demo-parsed)))
(manhatten (first (reduce handle2 [[0 0] wp-start] (take 20 real-parsed))))
;; => 1724
;; => 23960
;; => 26988

(time (manhatten (first (reduce handle2 [[0 0] wp-start] (take 20 real-parsed)))))

(def test-input "E5
R90
F2
W10
L180
N7
F4
R270
S3
L90
R180
W8
L270
F5")

(manhatten (first (reduce (debug handle2) [[0 0] wp-start]
                          (parse-demo test-input))))
;; => 216

ff
;;      ship  waypoint
;; E5   [0 0]   [15 1]
;; R90  [0 0]   [1 -15]
;; F2   [2 -30] [1 -15]
;; W10  [2 -30] [-9 -15]
;; L180 [2 -30] [9 15]
;; N7   [2 -30] [9 22]
;; F4   [38 58] [9 22]
;; R270 [38 58] [-22 9]
;; S3   [38 58] [-22 6]
;; L90  [38 58] [-6 -22]
;; R180 [38 58] [6 22]
;; W8   [38 58] [-2 22]
;; L270 [38 58] [22 2]
;; F5   [148 68] [22 2]
