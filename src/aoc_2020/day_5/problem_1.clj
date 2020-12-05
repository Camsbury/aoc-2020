(ns aoc-2020.day-5.problem-1
  (:require [aoc-2020.utils :as utils]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn split-range [[low high] lower?]
  (let [midpoint (+ low (quot (- high low) 2))]
    (if lower?
      [low midpoint]
      [midpoint high])))

(defn get-id [pass]
  (let [
        row (->> pass
                 (take 7)
                 (map #(= \F %))
                 (reduce split-range [0 128])
                 first)
        col (->> pass
                 (drop 7)
                 (map #(= \L %))
                 (reduce split-range [0 8])
                 first)]
    (+ (* 8 row) col)))

(defn get-missing [passes]
  (let [ids (map get-id passes)
        min-id (apply min ids)
        max-id (apply max ids)
        seen   (set ids)
        should-see (set (range min-id (inc max-id)))]
    (set/difference should-see seen)))

(comment
  (->>
   (utils/get-problem-input-file)
   str/split-lines
   get-missing))

