(ns lambdaisland.aoc-2020.puzzle14
  (:require [lambdaisland.aoc-2020.util :as util]
            [clojure.walk :as walk]))

(def demo-input "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0")

(def demo-input2 "mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1")

(def pattern #"(\w+)(\[(\d+)\])? = (.*)")

(defn parse-bitmask [mask bit]
  (->> (str/escape mask {\X bit})
       (str "2r")
       read-string))

(defn parse-instructions [input]
  (for [[op _ addr val] input]
    (case op
      "mask" [:mask (parse-bitmask val 1) (parse-bitmask val 0)]
      "mem" [:mem
             (read-string addr)
             (read-string val)])))

(def demo (parse-instructions (util/re-str-seq pattern demo-input)))
(def real (parse-instructions (util/re-resource-seq 14 pattern)))

(defn apply-mask [num zero one]
  (bit-and (bit-or num zero) one))

(defn part1 [input]
  (-> (reduce
       (fn [{:keys [mask0 mask1] :as mem} [op v1 v2]]
         (case op
           :mask
           (assoc mem :mask0 v1 :mask1 v2)
           :mem
           (assoc mem v1 (apply-mask v2 mask0 mask1))))
       {}
       input)
      (dissoc :mask0 :mask1)
      vals
      (->> (reduce +))))

(part1 real)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; part 2

;; FAILED

(defn parse-pair [mask]
  [(parse-bitmask mask 1)
   (parse-bitmask mask 0)])

(defn set-bit [[and-mask or-mask] pos bit]
  (case bit
    0 [(bit-and and-mask (bit-xor Long/MAX_VALUE pos)) or-mask]
    1 [and-mask (bit-or or-mask pos)]))

(defn binstr [n]
  (Long/toBinaryString n))

(defn str-tree [nums]
  (walk/postwalk #(if (number? %) (binstr %) %) nums))

;; and   or
[["111" "001"]
 ["101" "001"]
 ["111" "000"]]

;; => [[7 1] [7 1] [7 0]]

(parse-bitmask "XX1" 0)


(defn expand-masks [mask]
  (first (reduce
          (fn [[results pos] char]
            (case char
              \0 [results (* pos 2)]
              \1 [results (* pos 2)]
              \X [(mapcat (fn [pair]
                            #_(prn [:-> pair pos])
                            [(set-bit pair pos 0)
                             (set-bit pair pos 1)])
                          results)
                  (* pos 2)]))
          [[(parse-pair mask)] 1]
          (reverse mask))))

(defn parse-instructions2 [input]
  (for [[op _ addr val] input]
    (case op
      "mask" [:masks (expand-masks val)]
      "mem" [:mem
             (read-string addr)
             (read-string val)])))

(def demo2 (parse-instructions2 (util/re-str-seq pattern demo-input2)))
(def real2 (parse-instructions2 (util/re-resource-seq 14 pattern)))

(defn part2 [input]
  (-> (reduce
       (fn [{:keys [masks] :as mem} [op v1 v2]]
         (case op
           :masks
           (assoc mem :masks v1)
           :mem
           (reduce (fn [mem mask]
                     (assoc mem (apply apply-mask v1 mask) v2))
                   mem
                   masks)))
       {}
       input)
      (dissoc :masks)
      vals
      (->> (reduce +))))

(part2 demo2)
