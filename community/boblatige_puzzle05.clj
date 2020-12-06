(ns boblatige-day-5
  (:require [clojure.set :as set]))

(def input (-> (slurp (io/resource "puzzle_input_05.txt"))
               clojure.string/split-lines))

(defn lower-half [[start end]]
  [start (- end (quot (inc (- stop end)) 2))])

(defn upper-half [[start end]]
  [(+ start (quot (inc (- end start)) 2)) end])

(defn parse-input [s]
  (reduce (fn [acc char]
            (case char
              \F (update acc :row lower-half)
              \B (update acc :row upper-half)
              \L (update acc :col lower-half)
              \R (update acc :col upper-half)))
	  {:row [0 127] :col [0 7]}
          s))

(defn seat-id [{:keys [row col] :as pass}]
  (assoc pass :seat-id (+ (first col) (* (first row) 8))))

(defn missing-number [xs]
  (let [first (first xs)
        last (last xs)
        full (set (range first (inc last)))]
    (set/difference full (set xs))))

;; part-1
(->> input
     (map parse-input)
     (map seat-id)
     (map :seat-id)
     (sort)
     last) ;; => 922

;; part-2
(->> input
     (map parse-input)
     (map seat-id)
     (map :seat-id)
     (sort)
     missing-number);; => #{747}
