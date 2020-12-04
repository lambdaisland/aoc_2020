(ns lambdaisland.aoc-2020.puzzle04
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def demo-input "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in")

(def real-input (slurp (io/resource "puzzle_input_04.txt")))

(defn parse-long [l]
  (Long/parseLong l))

(defn parse-entry [entry]
  (into {} (map (comp vec next)) (re-seq #"(\w{3}):(\S+)" entry)))

(defn valid? [m]
  (= (count (dissoc m "cid")) 7))

(->> (str/split demo-input #"\R\R")
     (map parse-entry)
     (filter valid?)
     count)

(def fields ["byr"
             "iyr"
             "eyr"
             "hgt"
             "hcl"
             "ecl"
             "pid"
             "cid"])

(count fields)

(->> (str/split real-input #"\R\R")
     (map parse-entry)
     (filter valid?)
     count)
;; => 228

;; byr (Birth Year) - four digits; at least 1920 and at most 2002.
;; iyr (Issue Year) - four digits; at least 2010 and at most 2020.
;; eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
;; hgt (Height) - a number followed by either cm or in:
;; If cm, the number must be at least 150 and at most 193.
;; If in, the number must be at least 59 and at most 76.
;; hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
;; ecl (Eye Color) - exactly one of:
;; pid (Passport ID) - a nine-digit number, including leading zeroes.
;; cid (Country ID) - ignored, missing or not.

(defn valid2? [{:strs [byr iyr eyr hgt hcl ecl pid cid]}]
  (and
   byr (<= 1920 (parse-long byr) 2002)
   iyr (<= 2010 (parse-long iyr) 2020)
   eyr (<= 2020 (parse-long eyr) 2030)
   hgt (let [[_ num unit] (re-find #"(\d+)(in|cm)" hgt)]
         (case unit
           "cm" (<= 150 (parse-long num) 193)
           "in" (<= 59 (parse-long num) 76)
           false))
   hcl (re-find #"^#[0-9a-f]{6}$" hcl)
   ecl (#{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} ecl)
   pid (re-find #"^\d{9}$" pid)))

(->> (str/split real-input #"\R\R")
     (map parse-entry)
     (filter valid2?)
     count)
;; => 175
