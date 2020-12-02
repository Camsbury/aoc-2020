(ns aoc-2020.day-2.utils-test
  (:require [aoc-2020.day-2.utils :as sut]
            [clojure.test :as t]))

(t/deftest process-entry-test
  (t/testing "parses an entry"
    (t/is (= {:password "abcde", :letter \a, :fst 1, :snd 3}
             (sut/process-entry "1-3 a: abcde")))))
