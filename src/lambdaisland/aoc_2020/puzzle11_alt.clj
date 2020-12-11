(ns lambdaisland.aoc-2020.puzzle11-alt
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(set! *unchecked-math* :warn-on-boxed)
(set! *warn-on-reflection* true)

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


;; The 8 cardinal directions, as [dx dy] pairs
(def directions (for [x (range -1 2)
                      y (range -1 2)
                      :when (not= [x y] [0 0])]
                  [x y]))

(defn empty-grid [dimx dimy]
  (vec (repeat dimx (vec (repeat dimy '_)))))

;; Optimized (?) lookup in nested vector
(defn getxy ^long [grid x y]
  (.nth ^clojure.lang.PersistentVector (.nth ^clojure.lang.PersistentVector grid x) y))

(defn agetxy [^"[[J" arr ^long x ^long y]
  (java.lang.reflect.Array/get (java.lang.reflect.Array/get arr x) y))

(defn setxy [grid x y v]
  (assoc-in grid [x y] v))

(defn asetxy [^"[[J" arr ^long x ^long y ^long v]
  (aset-long (java.lang.reflect.Array/get arr x) y v))

(defn cart* [rx ry]
  (for [x rx y ry] [x y]))

(def starting-positions
  (memoize
   (fn [^long dx ^long dy ^long dimx ^long dimy]
     (concat
      (case dx
        -1 (cart* [(dec dimx)] (range dimy))
        0 nil
        1 (cart* [0] (range dimy)))
      (case dy
        -1 (cart* (range dimx) [(dec dimy)])
        0 nil
        1 (cart* (range dimx) [0]))))))

(defn array-grid ^"[[J" [^long dimx ^long dimy]
  (into-array (repeatedly dimx #(long-array dimy 0))))

(defn precompute [grid [^long dx ^long dy] ^long dimx ^long dimy]
  (let [result (array-grid dimx dimy)]
    (doseq [[^long x ^long y] (starting-positions dx dy dimx dimy)]
      (loop [last-seen 0
             x x
             y y]
        (when (and (< -1 x dimx) (< -1 y dimy))
          (asetxy result x y last-seen)
          (case (getxy grid x y)
            -1 (recur last-seen (+ x dx) (+ y dy))
            0  (recur 0 (+ x dx) (+ y dy))
            1  (recur 1 (+ x dx) (+ y dy))))))
    (mapv vec result)))

(defn dimx [grid]
  (count grid))

(defn dimy [grid]
  (count (first grid)))

#_(defn neighbours [x y dimx dimy]
    (for [x' (range (max 0 (dec x)) (min (inc dimx) (+ x 2)))
          y' (range (max 0 (dec y)) (min (inc dimy) (+ y 2)))
          :when (not (and (= x x') (= y y')))]
      [x' y']))

(defn score ^long [layers x y]
  (transduce
   (map #(getxy % x y))
   +
   layers))

(defn syms->nums [grid]
  (mapv #(mapv '{_ -1 L 0 O 1} %) grid))

(defn nums->syms [grid]
  (mapv #(mapv '{-1 _ 0 L 1 O} %) grid))

(defn generation [dimx dimy idxs grid]
  (let [layers (map #(precompute grid % dimx dimy) directions)]
    (mapv (fn [x]
            (mapv (fn [y]
                    (case (getxy grid x y)
                      -1 -1
                      0  (if (= 0 (score layers x y)) 1 0)
                      1  (if (<= 5 (score layers x y)) 0 1)))
                  (range dimy)))
          (range dimx))))

(defn occupied-count [grid]
  (transduce (comp cat (map {-1 0 0 0 1 1})) + grid))


;; Part 2

(time
 (let [grid (syms->nums real-layout)
       dimx (dimx grid)
       dimy (dimy grid)
       idxs (cart* (range dimx) (range dimy))
       gen (partial generation dimx dimy idxs)]
   (loop [grid1 grid
          grid2 (gen grid)]
     (if (= grid1 grid2)
       (occupied-count grid1)
       (recur grid2 (gen grid2))))))
;; => 2197
;; "Elapsed time: 1503.329406 msecs"


(comment
  (precompute (mapv #(mapv '{_ -1 L 0 O 1} %)
                    '[[_ _ _ _ _ _ _ _ _ _]
                      [_ _ O _ _ _ _ _ _ _]
                      [_ _ _ _ _ _ _ _ _ _]
                      [_ _ _ _ L _ _ _ _ _]
                      [_ _ _ _ _ _ _ _ _ _]
                      [_ _ _ _ O _ _ _ _ _]
                      [_ _ O _ _ _ _ _ _ _]
                      [_ _ _ _ _ _ _ _ _ _]
                      [_ _ L _ _ _ _ _ _ _]
                      [_ _ _ _ L _ _ _ _ _]])
              [1 0]
              10 10
              )

  (let [dim 1000
        arr (array-grid dim dim)]
    (time
     (doseq [x (range dim)
             y (range dim)]
       (agetxy arr x y))))
  ;; This takes ~150ms

  (let [dim 1000
        arr (mapv vec (array-grid dim dim))]
    (time
     (doseq [x (range dim)
             y (range dim)]
       (getxy arr x y)))))
;; This only take ~75ms
