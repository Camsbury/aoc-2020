(ns aoc-2020.day-1.problem-2
  (:require
   [clojure.java.io :as io]
   [clojure.string  :as str]
   [clojure.math.combinatorics :as combo]))


(defn process-matches
  "Lazily finds the combinations of an input and processes them if they match

  Should be able to do this in linear time like in problem 1,
  but this is more elegant"
  [{:keys [n match? process]} inputs]
  (->> (combo/combinations inputs n)
       (some match?)
       process))

(comment
  (let [process #(apply * %)
        match?  #(when (= 2020 (apply + %)) %)
        expenses
        (->> "day_1/problem_2"
             io/resource
             slurp
             str/split-lines
             (mapv #(Integer/parseInt %)))]
    (process-matches {:n 3
                      :match? match?
                      :process process}
                     expenses)))
