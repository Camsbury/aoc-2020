(ns aoc-2020.day-7.problem-1
  (:require [aoc-2020.utils :as utils]
            [clojure.string :as str]
            [clojure.set :as set]))


(def EXAMPLE
  ["light red bags contain 1 bright white bag, 2 muted yellow bags."
   "dark orange bags contain 3 bright white bags, 4 muted yellow bags."
   "bright white bags contain 1 shiny gold bag."
   "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."
   "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
   "dark olive bags contain 3 faded blue bags, 4 dotted black bags."
   "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."
   "faded blue bags contain no other bags."
   "dotted black bags contain no other bags."])

(defn parse-contents [contents]
  (->> contents
       (re-seq
        #"(?:\d (.+?) bag[s]{0,1})")
       (map second)
       set))

(defn parse-rule [raw]
  (let [[container contents] (str/split raw #" bags ")]
    [container (parse-contents contents)]))

(defn count-containers [rules style]
  (loop [styles #{style} remaining rules cnt 0]
    (let [[containers others]
          ((juxt filter remove)
           (fn [[_ possible-styles]] (seq
                                      (set/intersection possible-styles styles)))
           remaining)]
      ;; [containers others]
      (if (seq containers)
        (recur (->> containers (map first) set) others (+ cnt (count containers)))
        cnt))))

(comment
  (count-containers (map parse-rule EXAMPLE) "shiny gold")
  (count-containers (map parse-rule (utils/get-problem-input)) "shiny gold")
  (map parse-rule
   ))
