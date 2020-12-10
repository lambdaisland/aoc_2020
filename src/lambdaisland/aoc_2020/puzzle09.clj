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

(comment
  (xmas-cypher2 127 demo-input)

  (time (xmas-cypher2 26796446 input))
;; => 3353494

  (time
   (let [target 26796446
         ranges (mapcat #(take-while identity (iterate butlast %))
                        (take-while identity (iterate next input)))]
     (some #(when (= target (apply + %))
              (+ (apply min %) (apply max %)))
           ranges)
     )))


(time
 (let [input demo-input
       target 127]
   (->> (range 2 (count input))
        (mapcat (fn [i] (partition i 1 input)))
        (some #(when (= target (apply + %))
                 (+ (apply min %) (apply max %)))))
   ))

;; => 14360655


;; @BobLaTige
;;  What would be the advantages other than semantics to use loop / recur
;;  instead of recursing by calling the fn from within the fn ?
(comment
  (defn part-2 [window invalid-num nums]
    (let [poss-set (->> (partition window 1 nums)
                        (map #(vector (apply + %) %) )
                        (into {}))
          weakness (poss-set invalid-num)]
      (if weakness
        (+ (apply min weakness) (apply max weakness))
        (recur (inc window) invalid-num nums))))

  (part-2 2 invalid-num (parse-input input)))
;; => 1962331


(defn decrement-to-zero [i]
  (if (= 0 i)
    i
    (decrement-to-zero (dec i))))
