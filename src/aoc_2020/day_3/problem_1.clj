(ns aoc-2020.day-3.problem-1
  (:require [aoc-2020.utils :as utils]))

 (defn count-path-trees
  "counts the trees on the path"
  [{:keys [right-step down-step pattern]}]
  (let [height (count pattern)
        width  (count (first pattern))
        coordinates (for [step (range (/ height down-step))]
                      (mapv #(* step %) [right-step down-step]))
        coordinate-has-tree?
        (fn [[right down]]
          (-> pattern
              (nth down)
              (nth (mod right width))
              (= \#)))]
    (->> coordinates
         (filter coordinate-has-tree?)
         count)))

(comment
  (count-path-trees
   {:right-step 3
    :down-step 1
    :pattern (utils/get-problem-input)})

  (let [slopes [[1 1] [3 1] [5 1] [7 1] [1 2]]
        pattern (utils/get-problem-input)
        count-slope-trees (fn [[right down]]
                            (count-path-trees
                             {:right-step right
                              :down-step  down
                              :pattern    pattern}))]
    (->> slopes
         (map count-slope-trees)
         (apply *))))
