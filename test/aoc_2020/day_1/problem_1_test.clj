(ns aoc-2020.day-1.problem-1-test
  (:require [clojure.test :as t]
            [aoc-2020.day-1.problem-1 :as sut]))

(t/deftest process-match-test
  (t/testing "returns the right sum for a provided input"
    (t/is (= 514579
           (let [get-match #(- 2020 %)
                 process
                 (fn [x]
                   (* x (get-match x)))]
             (sut/process-match
              {:process process
               :get-match get-match
               :xs [1721 979 366 299 675 1456]}))))))

