(ns aoc-2020.day-19
  (:require
   [aoc-2020.utils :as utils]
   [clojure.set    :as set]
   [clojure.string :as str]
   [instaparse.core :as insta]))

(def example
  "1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
0: 4 1 5
4: \"a\"
5: \"b\"

ababbb
bababa
abbbab
aaabbb
aaaabbb")

(defn solve-p1 [raw]
  (let [[grammar input] (str/split raw #"\n\n")
        grammar (str/join "\n" (sort (str/split-lines grammar)))
        input (str/split-lines input)
        parse (insta/parser grammar)]
    (->> input
         (map #(parse %))
         (filter #(= :0 (first %)))
         count)))

(comment
  (solve-p1 example)
  (solve-p1 (utils/get-problem-input-file "day_19")))


(defn solve-p2 [raw]
  (let [[grammar input] (str/split raw #"\n\n")
        unsorted-grammar (->>
                          grammar
                          str/split-lines
                          (replace
                           {"8: 42" "8: 42 | 42 8"
                            "11: 42 31" "11: 42 31 | 42 11 31"}))
        grammar (str/join "\n" (sort unsorted-grammar))
        input (str/split-lines input)
        parse (insta/parser grammar)
        ]
    (->> input
         (map #(parse %))
         (filter #(= :0 (first %)))
         count)))

(comment
  (solve-p2 example)
  (solve-p2 (utils/get-problem-input-file "day_19"))

  )
