(ns lambdaisland.aoc-2020.puzzle08
  (:require [clojure.java.io :as io]))

(def test-input "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6")

(defn parse-input [input]
  (vec (for [[_ op arg] (re-seq #"(\w{3}) ([+-]\d+)" input)]
         [(keyword op) (read-string arg)])))

(defn run-vm [program]
  (let [init-ctx {:pc 0
                  :acc 0
                  :seen? #{}}]
    (loop [{:keys [pc acc seen?] :as ctx} init-ctx]
      (if (seen? pc)
        acc
        (let [[op arg] (get program pc)]
          (case op
            :nop
            (recur (-> ctx
                       (update :pc inc)
                       (update :seen? conj pc)))
            :acc
            (recur (-> ctx
                       (update :acc + arg)
                       (update :pc inc)
                       (update :seen? conj pc)))
            :jmp
            (recur (-> ctx
                       (update :pc + arg)
                       (update :seen? conj pc)))))))))

(run-vm (parse-input (slurp (io/resource "puzzle_input_08.txt"))))
;; => 2051

(defn run-vm2 [program]
  (let [init-ctx {:pc 0
                  :acc 0
                  :seen? #{}}]
    (loop [{:keys [pc acc seen?] :as ctx} init-ctx]
      (cond
        (seen? pc)
        :infinte-loop!

        (= pc (count program))
        acc

        :else
        (let [[op arg] (get program pc)]
          (case op
            :nop
            (recur (-> ctx
                       (update :pc inc)
                       (update :seen? conj pc)))
            :acc
            (recur (-> ctx
                       (update :acc + arg)
                       (update :pc inc)
                       (update :seen? conj pc)))
            :jmp
            (recur (-> ctx
                       (update :pc + arg)
                       (update :seen? conj pc)))))))))

(let [program (parse-input (slurp (io/resource "puzzle_input_08.txt")))]
  (for [i (range (count program))
        :when (#{:nop :jmp} (get-in program [i 0]))
        :let [program (update-in program [i 0] {:jmp :nop, :nop :jmp})
              result (run-vm2 program)]
        :when (not= :infinte-loop! result)]
    result))
