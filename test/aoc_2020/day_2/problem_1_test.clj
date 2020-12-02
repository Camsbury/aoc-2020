(ns aoc-2020.day-2.problem-1-test
  (:require [aoc-2020.day-2.problem-1 :as sut]
            [aoc-2020.day-2.utils :as day-utils]
            [clojure.test :as t]))

(t/deftest valid-password-test
  (t/testing "validates a password"
    (t/is (sut/valid-password?
           {:password "abcde", :letter \a, :fst 1, :snd 3}))))

(t/deftest invalid-password-test
  (t/testing "invalidates a password"
    (t/is (not (sut/valid-password?
                {:password "cdefg", :letter \b, :fst 1, :snd 3})))))

(t/deftest count-valid-passwords-test
  (t/testing "counts valid passwords"
    (t/is (= 2
             (->>
              ["1-3 a: abcde"
               "1-3 b: cdefg"
               "2-9 c: ccccccccc"]
              (map day-utils/process-entry)
              sut/count-valid-passwords)))))
