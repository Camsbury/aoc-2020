(ns aoc-2020.day-8
  (:require [aoc-2020.utils :as utils]
            [clojure.string :as str]
            [clojure.walk   :as walk]
            [clojure.set    :as set]))

(def example
  ["nop +0"
   "acc +1"
   "jmp +4"
   "acc +3"
   "jmp -3"
   "acc -99"
   "acc +1"
   "jmp -4" ;; should change this to nop
   "acc +6"])

(defn- extract-instructions [raw]
  (let [clean-instruction
        (fn [[inst n]]
          [(keyword inst) (Integer/parseInt n)])]
    (->> raw
         (map #(str/split % #" "))
         (mapv clean-instruction))))

(defn solve-problem-1 [raw]
  (let [instructions (extract-instructions raw)
        pos-limit    (count instructions)]
    (loop [acc 0 pos 0 seen #{}]
      (if (or (contains? seen pos)
              (>= pos pos-limit))
        acc
        (let [seen (conj seen pos)
              [inst n] (nth instructions pos)]
          (case inst
            :nop (recur acc (inc pos) seen)
            :acc (recur (+ acc n) (inc pos) seen)
            :jmp (recur acc (+ pos n) seen)))))))


(defn solve-problem-2 [raw]
  (let [instructions (extract-instructions raw)
        pos-limit    (count instructions)]
    (loop [swapped 0]
      (if (#{:nop :jmp} (get-in instructions [swapped 0]))
        (let [attempted-solve
              (loop [acc 0 pos 0 seen #{}]
                (when-not (contains? seen pos)
                  (if (>= pos pos-limit)
                    acc
                    (let [seen (conj seen pos)
                          [inst n] (nth instructions pos)
                          inst (if (= pos swapped)
                                 (case inst
                                   :jmp :nop
                                   :nop :jmp)
                                 inst)]
                      (case inst
                        :nop (recur acc (inc pos) seen)
                        :acc (recur (+ acc n) (inc pos) seen)
                        :jmp (recur acc (+ pos n) seen))))))]
          (if attempted-solve
            attempted-solve
            (recur (inc swapped))))
        (recur (inc swapped))))))


(comment
  (solve-problem-1 example)
  (solve-problem-2 example)
  (solve-problem-2 (utils/get-problem-input))
  (solve-problem-1
   (utils/get-problem-input)))
