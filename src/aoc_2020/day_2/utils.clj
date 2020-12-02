(ns aoc-2020.day-2.utils
  (:require
   [clojure.string       :as str]))

(defn process-entry
  "parses each entry into a password and policy details"
  [entry]
  (let [[policy password] (str/split entry   #": ")
        [fst-snd letter]  (str/split policy  #" ")
        [fst snd]         (str/split fst-snd #"-")]
    {:password password
     :letter   (first letter)
     :fst    (Integer/parseInt fst)
     :snd   (Integer/parseInt snd)}))
