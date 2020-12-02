(ns lambdaisland.aoc-2020.puzzle02
  (:require [clojure.java.io :as io]))

(def sample-input "1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc")

(defn parse-long [l]
  (Long/parseLong l))

(defn parse-line [s]
  (let [[_ min max char pwd] (re-find #"(\d+)-(\d+) (.): (.*)" s)]
    [(parse-long min) (parse-long max) (first char) pwd]))

(defn entry-ok? [[min max char pwd]]
  (<= min (get (frequencies pwd) char 0) max))

(def input
  (map parse-line (line-seq (io/reader (io/resource "puzzle_input_02.txt")))))

(count (filter entry-ok? input))
;; => 603

(defn entry-ok2? [[min max char pwd]]
  (let [ok1 (= (nth pwd (dec min)) char)
        ok2 (= (nth pwd (dec max)) char)]
    (not= ok1 ok2)))

(count (filter entry-ok2? input))
;; => 404
