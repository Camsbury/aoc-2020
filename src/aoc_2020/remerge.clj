(ns aoc-2020.remerge
  (:require [aoc-2020.utils :as utils]
            [clojure.string :as str]
            [clojure.walk   :as walk]
            [clojure.set    :as set]))

(def example
  ["L.LL.LL.LL"
   "LLLLLLL.LL"
   "L.L.L..L.."
   "LLLL.LL.LL"
   "L.LL.LL.LL"
   "L.LLLLL.LL"
   "..L.L....."
   "LLLLLLLLLL"
   "L.LLLLLL.L"
   "L.LLLLL.LL"])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Day 11 - Part I
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn count-adj [seats x y]
  (->>
   [[(inc y) (inc x)]
    [(inc y) (dec x)]
    [(dec y) (dec x)]
    [(dec y) (inc x)]
    [y (inc x)]
    [y (dec x)]
    [(inc y) x]
    [(dec y) x]]
   (keep #(get-in seats %))
   (filter #(= % \#))
   count))

(defn update-seats [seats]
  (vec
   (for [y (range (count seats))]
     (vec
      (for [x (range (count (first seats)))]
        (case (nth (nth seats y) x)
          \. \.
          \L (if (= (count-adj seats x y) 0)
               \#
               \L)
          \# (if (> (count-adj seats x y) 3)
               \L
               \#)))))))

(defn final-seating [seat-init]
  (loop [seats seat-init]
    (let [new-seats (update-seats seats)]
      (if (= new-seats seats)
        seats
        (recur new-seats)))))

(defn final-occupied-count [seat-init]
  (->> seat-init
       final-seating
       (map (fn [xs] (filter #(= % \#) xs)))
       (map count)
       (reduce +)))

(comment
  (final-seating (map vec example))
  (final-occupied-count (map vec example))
  (final-occupied-count (map vec (utils/get-problem-input "day_11"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Day 11 - Part II
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- get-seen [seats x y]
  (fn [[delta-x delta-y]]
    (loop [[dx dy] [delta-x delta-y]]
      (let [x' (+ x dx)
            y' (+ y dy)
            seat (get-in seats [y' x'])]
        (if (= seat \.)
          (recur [(+ dx delta-x) (+ dy delta-y)])
          seat)))))

(defn count-seen [seats x y]
  (->>
   (for [a [(- 1) 0 1]
         b [(- 1) 0 1]
         :when (or (not= a 0)
                    (not= b 0))]
     [a b])
   (keep (get-seen seats x y))
   (filter #(= % \#))
   count))

(defn update-seats-2 [seats]
  (vec
   (for [y (range (count seats))]
     (vec
      (for [x (range (count (first seats)))]
        (case (nth (nth seats y) x)
          \. \.
          \L (if (= (count-seen seats x y) 0)
               \#
               \L)
          \# (if (> (count-seen seats x y) 4)
               \L
               \#)))))))

(defn final-seating-2 [seat-init]
  (loop [seats seat-init]
    (let [new-seats (update-seats-2 seats)]
      (if (= new-seats seats)
        seats
        (recur new-seats)))))

(defn final-occupied-count-2 [seat-init]
  (->> seat-init
       final-seating-2
       (map (fn [xs] (filter #(= % \#) xs)))
       (map count)
       (reduce +)))

(comment
  (final-seating-2 (map vec example))
  (final-occupied-count-2 (map vec example))
  (final-occupied-count-2 (map vec (utils/get-problem-input "day_11"))))
