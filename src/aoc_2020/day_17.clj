(ns aoc-2020.day-17
  (:require
   [aoc-2020.utils :as utils]
   [clojure.set    :as set]
   [clojure.string :as str]))


(def example
  [".#."
   "..#"
   "###"])

(def input
  ["..#..#.#"
   "##.#..#."
   "#....#.."
   ".#..####"
   ".....#.."
   "...##..."
   ".#.##..#"
   ".#.#.#.#"])

(defn parse-input [dim raw]
  (->>
   (for [[ix x] (map-indexed
                 (fn [idx itm] [idx itm])
                 raw)
         [iy y] (map-indexed
                 (fn [idx itm] [idx itm])
                 x)
         :when (= y \#)]
     [ix iy])
   (map #(into % (vec (repeat (- dim 2) 0))))
   set))

(defn get-neighbors
  ([point]
   (disj
    (set
     (get-neighbors point [[]]))
    point))
  ([[x & xs] neighbors]
   (if x
     (->> neighbors
          (mapcat (fn [neighbor] [(conj neighbor (inc x))
                                  (conj neighbor x)
                                  (conj neighbor (dec x))]))
          (recur xs))
     neighbors)))

(defn count-active-neighbors [active-points point]
  (->> point
       get-neighbors
       (filter active-points)
       count))

(defn newly-active? [active-points point]
  (let [counted-active-neighbors
        (count-active-neighbors active-points point)]
    (or
     (= 3 counted-active-neighbors)
     (and (= 2 counted-active-neighbors)
          (active-points point)))))

(defn ex-cycle [active-points]
  (let [all-points (into active-points
                         (mapcat get-neighbors)
                         active-points)
        conj-newly-active
        (fn [acc point]
          (cond-> acc
            (newly-active? active-points point)
            (conj point)))]
    (reduce conj-newly-active #{} all-points)))

(defn solve [raw dim cycle-count]
  (->> raw
       (parse-input dim)
       ((apply comp (repeat cycle-count ex-cycle)))
       count))

(comment
  (solve input 3 6)
  (solve input 4 6)
  )
