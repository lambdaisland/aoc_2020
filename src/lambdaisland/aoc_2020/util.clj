(ns lambdaisland.aoc-2020.util
  (:require [clojure.java.io :as io]))

(defn parse-long [l]
  (Long/parseLong l))

(defn puzzle-input [day]
  (io/resource (format "puzzle_input_%02d.txt" day)))

(defn num-resource-seq [day]
  (with-open [rdr (io/reader (puzzle-input day))]
    (doall (map parse-long (line-seq rdr)))))

(defn re-resource-seq [day re]
  (with-open [rdr (io/reader (puzzle-input day))]
    (doall (map #(next (re-find re %)) (line-seq rdr)))))
