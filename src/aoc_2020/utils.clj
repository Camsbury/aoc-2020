(ns aoc-2020.utils
  (:require [clojure.java.io :as io]
            [clojure.string  :as str]
            [clojure.math.numeric-tower :as nt]))


(defn- get-problem-filename []
  (let [[_ day problem]
        (-> *ns*
            ns-name
            str
            (str/replace "-" "_")
            (str/split #"\."))]
    (str day "/" problem)))

(defn get-problem-input
  ([]
   (get-problem-input (get-problem-filename)))
  ([filename]
   (->> filename
        io/resource
        slurp
        str/split-lines)))

(defn get-problem-input-file
  ([]
   (get-problem-input-file (get-problem-filename)))
  ([filename]
   (->> filename io/resource slurp)))

(defn abs [x]
  (max x (- x)))

(defn lcm [& xs]
  (reduce nt/lcm 1 xs))

(defn transpose [m]
  (apply mapv vector m))

(defn n-times
  "Apply a function n times"
  [f n]
  (apply comp (repeat n f)))
