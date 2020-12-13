(ns lambdaisland.aoc-2020.puzzle13
  (:require [lambdaisland.aoc-2020.util :as util]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def demo-input "939
7,13,x,x,59,x,31,19")

(def demo-parsed (map util/parse-long (re-seq #"\d+" demo-input)))

(def real-parsed (map util/parse-long (re-seq #"\d+" (slurp (util/puzzle-input 13)))))

(let [[ts & busses] real-parsed]
  (apply *
         (first
          (sort-by second
                   (for [bus busses]
                     [bus (- bus (mod ts bus))])))))
;; => 2238
;; => 295

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 2

(defn parse2 [input]
  (->> input
       (re-seq #"[\dx]+")
       (next)
       (map read-string)
       (map-indexed (fn [i n]
                      (when (not= 'x n)
                        [i n])))
       (filter some?)))

(def demo-parsed2 (parse2 demo-input))

(def real-parsed2 (parse2 (slurp (util/puzzle-input 13))))

(defn bus-cycle [[^long idx ^long bus]]
  (iterate #(+ bus ^long %) (- idx)))

(comment
  (take 10 (drop-while #(< % 1068781) (bus-cycle [0 7])))
  (take 10 (drop-while #(< % 1068781) (bus-cycle [1 13]))))

#_
(let [seqs (map bus-cycle (reverse (sort-by second demo-parsed2)))]
  (loop [[s & seqs] seqs]
    (let [[n & ns] s]
      (let [rest-seqs (map (fn [s] (drop-while #(< % n) s)) seqs)]
        (if (apply = n (map first rest-seqs))
          n
          (recur (cons ns rest-seqs)))))))

;; => 1068781


;; Original attempt, incrementally filter the sequence... way too slow
#_(defn narrow-cycle [bc [^long idx ^long bus]]
    (filter #(= 0 (mod (+ ^long % idx) bus)) bc))

;; Transducers couldn't save us either
#_(defn narrow-cycle-xform [[^long idx ^long bus]]
    (filter #(= 0 (mod (+ ^long % idx) bus))))
#_(time
   (let [[s & ss] (take 8 (sort-by second real-parsed2))
         xform (apply comp (map narrow-cycle-xform ss))]
     (transduce xform (fn ([acc] acc) ([acc n] (reduced n))) nil (bus-cycle s))))

;; Much better, find the first point where the pattern repeats, then use that to
;; start a new sequence
(defn narrow-cycle [bc [^long idx ^long bus]]
  (let [;; This is basically our first attempt, just do a filter over the
        ;; bus-cycle
        new-cycle (filter #(= 0 (mod (+ ^long % idx) bus)) bc)
        ;; But once you have that new cycle, you know that it'll skip ahead by
        ;; fixed increments
        [n1 n2] (take 2 new-cycle)
        diff (- ^long n2 ^long n1)]
    ;; So we basically re-create it as a single iteration sequence
    (iterate #(+ ^long % diff) n1)))

(time
 (first
  (let [[s & ss] (sort-by second real-parsed2)]
    (reduce narrow-cycle (bus-cycle s) ss))))
;; => 560214575859998
;; => "Elapsed time: 0.43833 msecs"


;; (first (narrow-cycle (bus-cycle [1 5]) [2 9]))

;; (filter (set (take 35 (bus-cycle [1 5])))
;;         (take 35 (bus-cycle [2 30])))
;; => (34 79 124 169)
;; => (34)
;; [(take 10 (bus-cycle [0 3]))
;;  (take 10 (bus-cycle [1 5]))]

;; (+ (* 5 7)
;;    (- (* 3 7) 1))

;; (+ (* 3 5)
;;    (- (* 2 5) 1))
