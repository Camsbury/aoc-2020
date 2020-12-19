(ns aoc-2020.utils
  (:require [clojure.java.io :as io]
            [clojure.string  :as str]))


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
