(ns zeng-xinhui-day5)

;; 202005

(let [f #(clojure.string/replace %3 % %2)
      g #(if (= (inc %2) %1) %2 (reduced (inc %2)))
      input (->> (slurp "resources/puzzle_input_05.txt")
                 (f "B" "1") (f "F" "0") (f "R" "1") (f "L" "0")
                 (re-seq #"\w+")
                 (map #(Integer/parseInt % 2))
                 sort
                 reverse)]
  [(first input) (reduce g input)])

;; [864 739]
