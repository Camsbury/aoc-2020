(ns aoc-2020.day-1.problem-2-test
  (:require [aoc-2020.day-1.problem-2 :as sut]
            [clojure.test :as t]))

(t/deftest process-match-test
  (t/testing "returns the right sum for a provided input"
    (t/is (= 241861950
             (let [process #(apply * %)
                   match?  #(when (= 2020 (apply + %)) %)
                   expenses [1721 979 366 299 675 1456]]
               (sut/process-matches {:n 3
                                     :match? match?
                                     :process process}
                                    expenses))))))

