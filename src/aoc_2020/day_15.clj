(ns aoc-2020.day-15
  (:require
   [aoc-2020.utils :as utils]
   [clojure.string :as str]))


(defn starter->val->last-index [starter]
  (->> starter
       (map-indexed (fn [idx val] [idx val]))
       (reduce (fn [acc [idx val]] (assoc acc val idx)) {})))

(defn nth-spoken [starter n]
  (let [lim (dec n)]
    (loop [turn (dec (count starter))
           curr (last starter)
           val->last-index (starter->val->last-index (butlast starter))]
      (if (not= turn lim)
        (recur (inc turn)
               (if (contains? val->last-index curr)
                 (- turn (val->last-index curr))
                 0)
               (assoc val->last-index curr turn))
        curr))))

(comment
  (nth-spoken [1 12 0 20 8 16] 2020)
  (nth-spoken [1 12 0 20 8 16] 30000000))
