(ns concat-overflow)

;; loop / recur

;; (concat ... )

;; StackOverflowError

(def long-seq (reduce concat (map #(list %) (range 100000))))

(first long-seq)

(defn my-concat [x y]
  (lazy-seq
   (if-let [s (seq x)]
     (cons (first s) (my-concat (rest s) y))
     y)))
