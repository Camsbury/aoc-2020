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

(def example-12
  ["F10"
   "N3"
   "F7"
   "R90"
   "F11"])

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
  {:east 0
   :north 0
   :heading :east})

(defn- adjust-heading [heading amount]
  (let [heading-idx (.indexOf directions heading)
        quarter-rots (quot amount 90)]
    (nth directions
     (mod (+ heading-idx quarter-rots) 4))))

(defn- process-action
  [{:keys [east north heading] :as state}
   {:keys [action amount]}]
  (case action
    :north (update state :north #(+ % amount))
    :south (update state :north #(- % amount))
    :east (update state :east #(+ % amount))
    :west (update state :east #(- % amount))
    :forward (process-action state {:action heading :amount amount})
    :left (update state :heading #(adjust-heading % (- amount)))
    :right (update state :heading #(adjust-heading % amount))))

(defn final-location [actions]
  (reduce process-action init-state actions))

(comment
  (->> example-12
       (map parse-code)
       final-location
       (#(+ (utils/abs (:east %)) (utils/abs (:north %)))))

  (->> (utils/get-problem-input "day_12")
       (map parse-code)
       final-location
       (#(+ (utils/abs (:east %)) (utils/abs (:north %))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Day 12 - Part II
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def init-state-2
  {:location
   {:east 0 :north 0}
   :waypoint
   {:east 10 :north 1}})

(defn- rotate-waypoint [{:keys [east north] :as wp} amount]
  (let [quarter-rots (mod (quot amount 90) 4)]
    (case quarter-rots
      1 {:east north
         :north (- east)}
      2 {:east (- east)
         :north (- north)}
      3 {:east (- north)
         :north east}
      0 wp)))

(defn- update-location [state amount]
  (let [[de dn]
        (->> [:east :north]
             (map #(get-in state [:waypoint %]))
             (map #(* % amount)))]
    (-> state
        (update-in [:location :north] #(+ % dn))
        (update-in [:location :east] #(+ % de)))))

(defn- process-action-2
  [state
   {:keys [action amount]}]
  (case action
    :north   (update-in state [:waypoint :north] #(+ % amount))
    :south   (update-in state [:waypoint :north] #(- % amount))
    :east    (update-in state [:waypoint :east]  #(+ % amount))
    :west    (update-in state [:waypoint :east]  #(- % amount))
    :left    (update state :waypoint #(rotate-waypoint % (- amount)))
    :right   (update state :waypoint #(rotate-waypoint % amount))
    :forward (update-location state amount)))

(defn manhattan-distance [{{:keys [east north]} :location}]
  (+ (utils/abs east) (utils/abs north)))

(defn final-location-2 [actions]
  (reduce process-action-2 init-state-2 actions))

(comment
  (->> example-12
       (map parse-code)
       final-location-2
       manhattan-distance)

  (->> (utils/get-problem-input "day_12")
       (map parse-code)
       final-location-2
       manhattan-distance))

;; Action N means to move the waypoint north by the given value.
;; Action S means to move the waypoint south by the given value.
;; Action E means to move the waypoint east by the given value.
;; Action W means to move the waypoint west by the given value.
;; Action L means to rotate the waypoint around the ship left (counter-clockwise) the given number of degrees.
;; Action R means to rotate the waypoint around the ship right (clockwise) the given number of degrees.
;; Action F means to move forward to the waypoint a number of times equal to the given value.
;; The waypoint starts 10 units east and 1 unit north relative to the ship. The waypoint is relative to the ship; that is, if the ship moves, the waypoint moves with it.

;; For example, using the same instructions as above:

;; F10 moves the ship to the waypoint 10 times (a total of 100 units east and 10 units north), leaving the ship at east 100, north 10. The waypoint stays 10 units east and 1 unit north of the ship.
;; N3 moves the waypoint 3 units north to 10 units east and 4 units north of the ship. The ship remains at east 100, north 10.
;; F7 moves the ship to the waypoint 7 times (a total of 70 units east and 28 units north), leaving the ship at east 170, north 38. The waypoint stays 10 units east and 4 units north of the ship.
;; R90 rotates the waypoint around the ship clockwise 90 degrees, moving it to 4 units east and 10 units south of the ship. The ship remains at east 170, north 38.
;; F11 moves the ship to the waypoint 11 times (a total of 44 units east and 110 units south), leaving the ship at east 214, south 72. The waypoint stays 4 units east and 10 units south of the ship.
;; After these operations, the ship's Manhattan distance from its starting position is 214 + 72 = 286.

;; Figure out where the navigation instructions actually lead. What is the Manhattan distance between that location and the ship's starting position?
