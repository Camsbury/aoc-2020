(ns aoc-2020.day-7.problem-2
  (:require [aoc-2020.utils :as utils]
            [clojure.string :as str]
            [clojure.set :as set]))

(def EXAMPLE
  ["shiny gold bags contain 2 dark red bags."
   "dark red bags contain 2 dark orange bags."
   "dark orange bags contain 2 dark yellow bags."
   "dark yellow bags contain 2 dark green bags."
   "dark green bags contain 2 dark blue bags."
   "dark blue bags contain 2 dark violet bags."
   "dark violet bags contain no other bags."])

(defn parse-contents [contents]
  (->> contents
       (re-seq
        #"(?:(\d+) (.+?) bag[s]{0,1})")
       (mapv (fn [[_ n style]]
               {:cnt (Integer/parseInt n)
                :style style}))))

(defn parse-rule [raw]
  (let [[container contents] (str/split raw #" bags ")]
    [container (parse-contents contents)]))

(defn rule-map [raw]
  (into {} (map parse-rule) raw))

(defn count-containers [rules style]
  (loop [styles #{style} remaining rules cnt 0]
    (let [[containers others]
          ((juxt filter remove)
           (fn [[_ possible-styles]]
             (seq
              (set/intersection possible-styles styles)))
           remaining)]
      (if (seq containers)
        (recur (->> containers (map first) set) others (+ cnt (count containers)))
        cnt))))

(defn count-contents [rules top-style]
  (loop [contents-stack [{:style top-style :cnt 1}] total (- 1)]
    (if (seq contents-stack)
      (let [{:keys [style cnt]} (peek contents-stack)
            extracted-rules     (get rules style)
            expand-count        (fn [{style :style
                                      inner-cnt :cnt}]
                                  {:style style
                                   :cnt (* cnt inner-cnt)})
            new-stack (reduce
                       conj
                       (pop contents-stack)
                       (mapv expand-count extracted-rules))
            new-cnt (+ total cnt)]
        (recur new-stack new-cnt))
      total)))

(comment
  (count-contents (rule-map EXAMPLE) "shiny gold")
  (count-contents (rule-map (utils/get-problem-input)) "shiny gold"))
