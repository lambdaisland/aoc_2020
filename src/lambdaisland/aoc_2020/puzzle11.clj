(ns lambdaisland.aoc-2020.puzzle11
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def demo-input "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL")

(defn parse-input [input]
  (mapv #(mapv '{\. _ \L L} %) (str/split-lines input)))

(def demo-layout (parse-input demo-input))
(def real-layout (parse-input (slurp (io/resource "puzzle_input_11.txt"))))

(def max-x (dec (count (first demo-layout))))
(def max-y (dec (count demo-layout)))

(defn getxy [grid x y]
  (.nth ^clojure.lang.PersistentVector (.nth ^clojure.lang.PersistentVector grid x) y))

(defn getxy* [^clojure.lang.PersistentVector grid ^clojure.lang.PersistentVector xy]
  (.nth ^clojure.lang.PersistentVector (.nth grid (.nth xy 0)) (.nth xy 1)))

(defn neighbours [x y max-x max-y]
  (for [x' (range (max 0 (dec x)) (inc (min max-x (inc x))))
        y' (range (max 0 (dec y)) (inc (min max-y (inc y))))
        :when (not (and (= x x') (= y y')))]
    [x' y']))

(defn pos-score
  #_([layout x y]
     (pos-score x y max-x max-y))
  ([layout x y max-x max-y]
   (reduce (fn [score coords]
             (+ score (if (= 'O (getxy* layout coords)) 1 0)))
           0
           (neighbours x y max-x max-y))))

(defn generation
  #_([layout]
     (generation layout max-x max-y))
  ([layout max-x max-y]
   (mapv (fn [x]
           (mapv (fn [y]
                   (case (getxy layout x y)
                     _ '_
                     L (if (= 0 (pos-score layout x y max-x max-y)) 'O 'L)
                     O (if (<= 4 (pos-score layout x y max-x max-y)) 'L 'O)))
                 (range (inc max-y))))
         (range (inc max-x)))))

(defn compute-max-x [layout]
  (dec (count layout)))

(defn compute-max-y [layout]
  (dec (count (first layout))))

(defn find-fix-point
  ([layout]
   (find-fix-point layout generation))
  ([layout generation]
   (let [max-x (compute-max-x layout)
         max-y (compute-max-y layout)]
     (loop [layout layout
            layout' (generation layout max-x max-y)]
       (if (= layout' layout)
         layout
         (recur layout' (generation layout' max-x max-y)))))))

(comment
  (pos-score (generation layout) 9 2)
  (generation (generation layout)))

(defn result1
  ([layout]
   (result1 layout generation))
  ([layout generation]
   (transduce (comp cat (map '{_ 0, L 0, O 1})) + (find-fix-point layout generation))))

(comment
  (generation demo-layout max-x max-y)

  (result1 demo-layout)
  (time (result1 real-layout)))
;; => 2552

(defn layout-size [l]
  [(count l)
   (count (first l))])

(defn spit-layout [f l]
  (spit f (str/join "\n" (map #(apply str (map '{_ \. L \L O \#} %)) l))))

(comment
  (spit-layout "/tmp/layout.txt" real-layout #_(generation real-layout
                                                           (compute-max-x real-layout)
                                                           (compute-max-y real-layout)))

  (layout-size (generation real-layout
                           (compute-max-x real-layout)
                           (compute-max-y real-layout))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 2

(def directions (for [x (range -1 2)
                      y (range -1 2)
                      :when (not= [x y] [0 0])]
                  [x y]))

(defn seat-in-direction [layout x y max-x max-y [dx dy]]
  (loop [x (+ x dx)
         y (+ y dy)]
    (if (or (< x 0) (< y 0) (< max-x x) (< max-y y))
      0
      (let [seat (getxy layout x y)]
        (case seat
          L 0
          O 1
          _ (recur (+ x dx)
                   (+ y dy)))))))

(defn compute-seat [layout x y max-x max-y]
  (reduce
   (fn [acc dir]
     (let [acc (+ acc (seat-in-direction layout x y max-x max-y dir))]
       (if (= acc 5)
         (reduced 5)
         acc)))
   0
   directions))

(defn generation2
  #_([layout]
     (generation layout max-x max-y))
  ([layout max-x max-y]
   (mapv (fn [x]
           (mapv (fn [y]
                   (let [sym (getxy layout x y)]
                     (if (= '_ sym)
                       '_
                       (case (compute-seat layout x y max-x max-y)
                         5 'L
                         0 'O
                         sym))))
                 (range (inc max-y))))
         (range (inc max-x)))))

(comment
  (time (result1 real-layout generation2))
  ;; => 2197
  (generation2 demo-layout 9 9)


  )
