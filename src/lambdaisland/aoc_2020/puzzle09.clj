(ns lambdaisland.aoc-2020.puzzle09
  (:require [lambdaisland.aoc-2020.util :as util]))

;; Font: Iosevka Fixed SS14

;; Theme: Tomorrow night
;; (I also use/have used Tomorrow day, Solarized light, Solarized dark, Leuven)

;; Editor: Emacs + Corgi (github.com/plexus/corgi) = evil, CIDER, paredit

(def demo-input [35 20 15 25 47 40 62 55 65 95 102 117 150 182 127 219 299 277 309 576])

(def input (util/num-resource-seq 9))

(defn valid? [window num]
  (some #{num} (for [x window y window :when (< x y)] (+ x y))))

(defn xmas-cypher [input preamble-size]
  (some identity
        (for [part (partition (inc preamble-size) 1 input)
              :let [window (butlast part)
                    num (last part)]]
          (when-not (valid? window num)
            num))))

(xmas-cypher demo-input 5)
;; => 127

(xmas-cypher input 25)
;; => 26796446

(defn xmas-cypher2 [target input]
  (loop [nums input]
    (let [res (loop [cnt 1]
                (let [rng (take cnt nums)
                      sum (apply + rng)]
                  (cond
                    (= sum target)
                    (+ (apply min rng) (apply max rng))

                    (< sum target)
                    (recur (inc cnt))

                    #_(< target sum))))]
      (if res
        res
        (recur (next nums))))))

(xmas-cypher2 127 demo-input)

(time (xmas-cypher2 26796446 input))
;; => 3353494
