(ns lambdaisland.aoc-2020.puzzle06
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]))

(def groups (str/split (slurp (io/resource "puzzle_input_06.txt")) #"\R\R"))

(reduce + (map (comp count set #(str/replace % "\n" "")) groups))
;; => 7110

(reduce + (map (comp count distinct #(remove #{\newline} %)) groups))

(def group (first groups))

(transduce (map
            (fn [group]
              (count (apply set/intersection (map set (str/split-lines group))))))
           +
           groups)
;; => 3628
