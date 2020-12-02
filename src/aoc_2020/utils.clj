(ns aoc-2020.utils
  (:require [clojure.java.io :as io]
            [clojure.string  :as str]))

(defn get-problem-input []
  (let [[_ day problem]
        (-> *ns*
            ns-name
            str
            (str/replace "-" "_")
            (str/split #"\."))]
    (->> (str day "/" problem)
         io/resource
         slurp
         str/split-lines)))
