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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Day 12 - Part I
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --- Day 12: Rain Risk ---

;; The navigation instructions (your puzzle input) consists of a sequence of single-character actions paired with integer input values. After staring at them for a few minutes, you work out what they probably mean:

;; Action N means to move north by the given value.
;; Action S means to move south by the given value.
;; Action E means to move east by the given value.
;; Action W means to move west by the given value.
;; Action L means to turn left the given number of degrees.
;; Action R means to turn right the given number of degrees.
;; Action F means to move forward by the given value in the direction the ship is currently facing.
;; The ship starts by facing east. Only the L and R actions change the direction the ship is facing. (That is, if the ship is facing east and the next instruction is N10, the ship would move north 10 units, but would still move east if the following action were F.)

(def example-12
  ["F10"
   "N3"
   "F7"
   "R90"
   "F11"])

;; These instructions would be handled as follows:

;; F10 would move the ship 10 units east (because the ship starts by facing east) to east 10, north 0.
;; N3 would move the ship 3 units north to east 10, north 3.
;; F7 would move the ship another 7 units east (because the ship is still facing east) to east 17, north 3.
;; R90 would cause the ship to turn right by 90 degrees and face south; it remains at east 17, north 3.
;; F11 would move the ship 11 units south to east 17, south 8.
;; At the end of these instructions, the ship's Manhattan distance (sum of the absolute values of its east/west position and its north/south position) from its starting position is 17 + 8 = 25.

;; Figure out where the navigation instructions lead. What is the Manhattan distance between that location and the ship's starting position?

(def directions
  [:east :south :west :north])

(defn- get-action [letter]
  (case letter
    \N :north
    \S :south
    \E :east
    \W :west
    \L :left
    \R :right
    \F :forward))

(defn- parse-code [code]
  {:action (get-action (first code))
   :amount (Integer/parseInt (subs code 1))})

(def init-state
  {:west->east 0
   :south->north 0
   :heading :east})

(defn- adjust-heading [heading amount]
  (let [heading-idx (.indexOf directions heading)
        quarter-rots (quot amount 90)]
    (nth directions
     (mod (+ heading-idx quarter-rots) 4))))

(defn- process-action
  [{:keys [west->east south->north heading] :as state}
   {:keys [action amount]}]
  (case action
    :north (update state :south->north #(+ % amount))
    :south (update state :south->north #(- % amount))
    :east (update state :west->east #(+ % amount))
    :west (update state :west->east #(- % amount))
    :forward (process-action state {:action heading :amount amount})
    :left (update state :heading #(adjust-heading % (- amount)))
    :right (update state :heading #(adjust-heading % amount))))

(defn final-location [actions]
  (reduce process-action init-state actions))

(comment
  (->> example-12
       (map parse-code)
       final-location
       (#(+ (utils/abs (:west->east %)) (utils/abs (:south->north %)))))
  (->> (utils/get-problem-input "day_12")
       (map parse-code)
       final-location
       (#(+ (utils/abs (:west->east %)) (utils/abs (:south->north %)))))
  )
