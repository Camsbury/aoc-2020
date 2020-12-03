(ns aoc-2020.day-3.problem-1-test
  (:require [aoc-2020.day-3.problem-1 :as sut]
            [clojure.test :as t]))

(def EXAMPLE
  ["..##......."
   "#...#...#.."
   ".#....#..#."
   "..#.#...#.#"
   ".#...##..#."
   "..#.##....."
   ".#.#.#....#"
   ".#........#"
   "#.##...#..."
   "#...##....#"
   ".#..#...#.#"])

(t/deftest count-test
  (t/testing "counts trees correctly"
    (t/is 7
          (sut/count-path-trees
           {:right-step 3
            :down-step 1
            :pattern EXAMPLE}))))
