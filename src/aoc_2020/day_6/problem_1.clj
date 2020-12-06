(ns aoc-2020.day-6.problem-1
  (:require [aoc-2020.utils :as utils]
            [clojure.string :as str]
            [clojure.set :as set]))

(def EXAMPLE
  "abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb")

 (comment
   (let [get-set (fn [answers]
                   (->> answers
                        (map seq)
                        (map set)
                        (apply set/union)))]
        (->>
         (str/split EXAMPLE #"\n\n")
         (map #(str/split % #"\n"))
         (map get-set)
         (map count)
         (apply +)))
   (let [get-set (fn [answers]
                   (->> answers
                        (map seq)
                        (map set)
                        (apply set/union)))]
        (->>
         (str/split (utils/get-problem-input-file) #"\n\n")
         (map #(str/split % #"\n"))
         (map get-set)
         (map count)
         (apply +)))
   (let [get-set (fn [answers]
                   (->> answers
                        (map seq)
                        (map set)
                        (apply set/intersection)))]
        (->>
         (str/split (utils/get-problem-input-file) #"\n\n")
         (map #(str/split % #"\n"))
         (map get-set)
         (map count)
         (apply +))))
