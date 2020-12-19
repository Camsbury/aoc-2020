(ns aoc-2020.day-13
  (:require
   [aoc-2020.utils :as utils]
   [clojure.string :as str]))

(def example
  ["939"
   "7,13,x,x,59,x,31,19"])

(defn parse-input [[time busses]]
  {:time (Integer/parseInt time)
   :busses (->> (str/split busses #",")
                (remove #(= % "x"))
                (map #(Integer/parseInt %)))})

(defn get-time [time id]
  {:id        id
   :wait-time (- id (rem time id))})

(defn get-answer [{:keys [wait-time id]}]
  (* id wait-time))

(defn solve-p1 [{:keys [time busses]}]
  (->> busses
       (map #(get-time time %))
       (apply (partial min-key :wait-time))
       get-answer))

(comment
  (-> (utils/get-problem-input "day_13")
      parse-input
      solve-p1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PART II
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --- Part Two ---
;; find the earliest timestamp such that
;; the first bus ID departs at that time and
;; each subsequent listed bus ID departs at that subsequent minute.

;; For example, suppose you have the same list of bus IDs as above:

;; 7,13,x,x,59,x,31,19
;; An x in the schedule means there are no constraints on what bus IDs must depart at that time.

;; The earliest timestamp that matches the list 17,x,13,19 is 3417.
;; 67,7,59,61 first occurs at timestamp 754018.
;; 67,x,7,59,61 first occurs at timestamp 779210.
;; 67,7,x,59,61 first occurs at timestamp 1261476.
;; 1789,37,47,1889 first occurs at timestamp 1202161486.
;; However, with so many bus IDs in your list, surely the actual earliest timestamp will be larger than 100000000000000!

;; What is the earliest timestamp such that all of the listed bus IDs depart at offsets matching their positions in the list?

(defn- parse-input-2 [[_ raw]]
  (->> (str/split raw #",")
       (mapv (fn [x]
              (when (not= x "x")
                (long
                 (Integer/parseInt x)))))))

(defn get-sol-bf [raw]
  (let [rules (parse-input-2 raw)
        diff  (count rules)]
    (loop [ts 0 candidates []]
      (let [candidates (conj candidates ts)]
        (if-let [sol (seq
                      (filter
                       #(= diff (- ts %))
                       candidates))]
          (-> sol first)
          (recur (inc ts)
                 (filter
                  #(if-let [bus (nth rules (- ts %))]
                     (= 0 (mod ts bus))
                     true)
                  candidates)))))))

(defn- get-max-bus [rules]
  (->> rules
       (map-indexed
        (fn [diff bus]
          (when bus
            {:diff diff
             :bus bus})))
       (remove nil?)
       (apply (partial max-key :bus))))

(defn valid-ts? [rules]
  (let [preds
        (->> rules
             (map-indexed
              (fn [diff bus]
                (when bus
                  [diff bus])))
             (remove nil?)
             (sort-by second >)
             (map
              (fn [[diff bus]]
                (fn [ts]
                  (= 0 (mod (+ ts diff) bus))))))]
    (fn [ts]
      (every? #(% ts) preds))))

(defn get-sol-2 [raw]
  (let [rules (parse-input-2 raw)
        {max-diff :diff
         max-bus  :bus}
        (get-max-bus rules)]
    (->> (range)
         (drop 1)
         (map #(* max-bus %))
         (map #(- % max-diff))
         (filter (valid-ts? rules))
         first)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NOTICED LCM PATTERN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-sorted-busses [rules]
  (->> rules
       (map-indexed
        (fn [diff bus]
          (when bus
            {:diff diff
             :bus  bus})))
       (remove nil?)
       (sort-by :bus >)))

(defn matching-bus? [ts {:keys [diff bus]}]
  (= 0 (mod (+ ts diff) bus)))

(defn get-sol [raw]
  (let [rules (parse-input-2 raw)
        [{max-bus :bus
          max-diff :diff} & sorted-busses] (get-sorted-busses rules)]
    (loop [ts       (- max-bus max-diff)
           remaining-busses sorted-busses
           ts-step          max-bus]
      (let [[matched unmatched] ((juxt filter remove)
                                 #(matching-bus? ts %)
                                 remaining-busses)
            new-ts-step (->> matched
                             (map :bus)
                             (apply utils/lcm ts-step))]
        (if (seq unmatched)
             (recur (+ ts new-ts-step) unmatched new-ts-step)
             ts)))))



(comment
  (get-sol-bf example)
  (get-sol-2 example)
  (get-sol example)
  (get-sol (utils/get-problem-input "day_13")))


