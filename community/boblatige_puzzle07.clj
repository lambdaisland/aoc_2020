(ns boblatige-puzzle07
  (:require [clojure.string :as st]
            [clojure.java.io :as io]))

(def input (slurp (io/resource "puzzle_input_07.txt")))

(def example-input "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.")

(def example-input-2 "shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags.")

(defn parse-rule
  "['dark violet' [{:name :amount} ...]"
  [raw-rule-line]
  (let [[bag-name contains] (st/split raw-rule-line #" contain ")
        [_ parsed-bag-name] (re-find #"(.*) bag" bag-name)
        contains (when (not= "no other bags." contains)
                   (->> (st/split contains #", ")
                        (map (fn [sub-strings]
                               (let [[_ amount name] (re-find #"^(\d*) (.*) bag" sub-strings)]
                                 {:name name
                                  :amount (Integer/parseInt amount)})))))]
    [parsed-bag-name contains]))

(parse-rule "dark green bags contain 2 dark blue bags.")
;; => ["dark green" ({:name "dark blue", :amount 2})]

(def parsed (->> (st/split-lines input)
                 (map parse-rule)
                 (into {})))

(def contain-tree (fn [bag-name]
                    (map (fn hold [bag]
                           [bag
                            (when-let [contain (seq (get parsed (:name bag)))]
                              (map hold contain))])
                         (get parsed bag-name))))

(def can-hold (fn [bags bag-name]
                (->> bags
                     (filter (fn [[_ can-hold]]
                               (some (fn check [bag]
                                       (or (= bag-name (:name bag))
                                           (some check (get parsed (:name bag)))))
                                     can-hold)))
                     count)))

(def drill (fn drill [[{:keys [amount]} xs]]
             (+ amount
                (* amount (apply + (when (seq xs) (map drill xs)))))))

{:part-2 (apply + (map drill (contain-tree "shiny gold")))
 :part-1 (can-hold parsed "shiny gold")}
;; => {:part-2 24867, :part-1 148}

;; => (({:name "dark olive", :amount 1}
;;      (({:name "faded blue", :amount 3})
;;       ({:name "dotted black", :amount 4})))
;;     ({:name "vibrant plum", :amount 2}
;;      (({:name "faded blue", :amount 5})
;;       ({:name "dotted black", :amount 6}))))
