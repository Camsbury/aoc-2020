(ns aoc-2020.day-1.problem-1
  (:require [clojure.java.io :as io]
            [clojure.string  :as str]))

(defn process-match
  "Collects solution candidates in a set,
  then evaluates them when a match is found"
  [{:keys [process get-match xs]}]
  (let [find-solution
        (fn [[candidates _] x]
          (if (contains? candidates x)
            (reduced [candidates (process x)])
            [(conj candidates (get-match x)) nil]))]
    (->> xs
         (reduce find-solution [#{} nil])
         second)))

;; Solve the problem with the given input
(comment
  (let [get-match #(- 2020 %)
        process (fn [x] (* x (get-match x)))
        input
        (->> "day_1/problem_1"
             io/resource
             slurp
             str/split-lines
             (mapv #(Integer/parseInt %)))]
    (process-match
     {:process process
      :get-match get-match
      :xs input})))

