(ns aoc-2020.day-10
  (:require [aoc-2020.utils :as utils]
            [clojure.string :as str]
            [clojure.walk   :as walk]
            [clojure.set    :as set]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Problem I
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def example-1
  [16 10 15 5 1 11 7 19 6 12 4])

;; add zero and max + 3 to get the things you need to sort,
;; then find the differences categorized by 1 2 or 3
;; (* (get diffs 1) (get diffs 3))


;; Here is a larger example:

(def example-2
  [28 33 18 42 31 14 46 20 48 47 24 23 49 45 19 38 39 11 1 32 25 35 8 17 7 9 4 2
   34 10 3])

(comment
  (->> (conj (map #(Integer/parseInt %) (utils/get-problem-input)) 0)
       (sort >)
       (#(conj % (+ 3 (first %))))
       (partition 2 1)
       (map #(apply - %))
       (reduce #(update %1 %2 (fnil inc 0)) {})
       (#(* (get % 1) (get % 3)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Problem II
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn solve-part-2
  [input]
  (let [f (fn [acc x]
            (if (empty? acc)
              #{[x]}
              (let [filtered-acc
                    (filter #(<= (- (last %) x) 3) acc)]
                (set/union
                 filtered-acc
                 (set (map #(conj % x) filtered-acc))))))]
    (->> (conj input 0)
         (sort >)
         (#(conj % (+ 3 (first %))))
         (reduce f #{})
         (into #{} (filter #(= 0 (last %))))
         count)))

(defn solve-part-2-perf
  [input]
  (let [f (fn [acc x]
            (if (empty? acc)
              {x 1}
              (let [valid-prefixes (->> acc
                                        keys
                                        (filter #(<= (- % x) 3)))
                    filtered-acc (select-keys acc valid-prefixes)
                    new-count    (reduce + (map acc valid-prefixes))]
                (assoc filtered-acc x new-count))))]
    (->> (conj input 0)
         (sort >)
         (#(conj % (+ 3 (first %))))
         (reduce f {})
         (#(get % 0)))))

(comment
  (solve-part-2 example-1)
  (solve-part-2-perf example-1)
  (solve-part-2-perf (map #(Integer/parseInt %) (utils/get-problem-input)))
  )


