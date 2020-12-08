(ns lambdaisland.aoc-2020.puzzle07
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
;; faded blue bags contain no other bags.

(def demo-input (line-seq (io/reader (io/resource "puzzle_input_07_demo.txt"))))
(def demo-input2 (line-seq (io/reader (io/resource "puzzle_input_07_demo2.txt"))))
(def real-input (line-seq (io/reader (io/resource "puzzle_input_07.txt"))))

(defn parse-entry [s]
  (let [[bag & deps] (str/split s #"\s?(contain|,)\s?")
        color (re-find #"\w+ \w+" bag)]
    [color (keep (comp next (partial re-find #"(\d+) (\w+ \w+)" )) deps)]))

(map parse-entry demo-input)

(defn color-graph [entries]
  (reduce (fn [m [bag deps]]
            (reduce (fn [m [num col]]
                      (update m col conj bag))
                    m deps))
          {}
          entries))

(defn add-valid [result graph color]
  (into result (get graph color)))

(defn valid-outermost [graph start]
  (loop [result (add-valid #{} graph start)]
    (let [result2 (reduce (fn [res color]
                            (add-valid res graph color))
                          result result)]
      (if (= result result2)
        result
        (recur result2)))))

(count (valid-outermost (color-graph (map parse-entry demo-input)) "shiny gold"))

(count (valid-outermost (color-graph (map parse-entry real-input)) "shiny gold"))
;; => 332

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; part 2

(defn nesting-graph [entries]
  (reduce (fn [m [bag deps]]
            (reduce (fn [m [num col]]
                      (update m bag conj [(Long/parseLong num) col]))
                    m deps))
          {}
          entries))

(def graph (nesting-graph (map parse-entry real-input)))

(defn color-count [graph color]
  (let [entries (get graph color)]
    (if (seq entries)
      (reduce
       (fn [cnt [num color]]
         (+ cnt (* num (color-count graph color))))
       1
       entries)
      1)))

(dec (color-count graph "shiny gold"))
;; => 10875
