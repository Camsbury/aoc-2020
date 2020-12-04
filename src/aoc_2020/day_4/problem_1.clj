(ns aoc-2020.day-4.problem-1
  (:require
   [clojure.java.io :as io]
   [clojure.string  :as str]
   [clojure.set  :as set]
   [clojure.walk  :as walk]
   [aoc-2020.utils :as utils]))

(def needed-codes
  #{"byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"})

;; byr (Birth Year) - four digits; at least 1920 and at most 2002.
;; iyr (Issue Year) - four digits; at least 2010 and at most 2020.
;; eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
;; hgt (Height) - a number followed by either cm or in:
;; If cm, the number must be at least 150 and at most 193.
;; If in, the number must be at least 59 and at most 76.
;; hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
;; ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
;; pid (Passport ID) - a nine-digit number, including leading zeroes.
;; cid (Country ID) - ignored, missing or not.



(defn extract-codes [passport]
  (->> #"\s"
       str/split
       (-> passport)
       (map #(subs % 0 3))
       set))

(defn valid-height? [hgt]
  (let [cm-height
        (some->> hgt
                 (re-matches #"([0-9]+)cm")
                 second
                 (Integer/parseInt))
        in-height
        (some->> hgt
                 (re-matches #"([0-9]+)in")
                 second
                 (Integer/parseInt))]
    (or
     (and cm-height
          (<= 150 cm-height 193))
     (and in-height
          (<= 59 in-height 76)))))

(defn valid-codemap?
  [{:keys [byr iyr eyr hgt hcl ecl pid]}]
  (and
   (= 4 (count byr))
   (<= 1920 (Integer/parseInt byr) 2002)
   (= 4 (count iyr))
   (<= 2010 (Integer/parseInt iyr) 2020)
   (= 4 (count eyr))
   (<= 2020 (Integer/parseInt eyr) 2030)
   (valid-height? hgt)
   (some->> hcl
            (re-matches #"#[0-9a-f]{6}"))
   (#{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} ecl)
   (some->> pid
            (re-matches #"[0-9]{9}"))))

(defn extract-codemap [passport]
  (->> #"\s"
       str/split
       (-> passport)
       (map #(str/split % #":"))
       (into {})
       walk/keywordize-keys))

(comment

  (->> #"\n\n"
       str/split
       (-> "day_4/example"
           io/resource
           slurp)
       (map extract-codes)
       (filter #(set/subset? needed-codes %))
       count)

  (->> #"\n\n"
       str/split
       (-> "day_4/valid_example"
           io/resource
           slurp)
       (map extract-codemap)
       (filter valid-codemap?)
       count)

  (->> #"\n\n"
       str/split
       (-> "day_4/invalid_example"
           io/resource
           slurp)
       (map extract-codemap)
       first)

  (->> #"\n\n"
       str/split
       (-> "day_4/invalid_example"
           io/resource
           slurp)
       (map extract-codemap)
       (filter valid-codemap?)
       count)

(->> #"\n\n"
       str/split
       (-> (utils/get-problem-input-file))
       (map extract-codes)
       (filter #(set/subset? needed-codes %))
       count)


  (->> #"\n\n"
       str/split
       (-> (utils/get-problem-input-file))
       (map extract-codemap)
       (filter valid-codemap?)
       count)
)
