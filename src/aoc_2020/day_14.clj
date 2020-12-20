(ns aoc-2020.day-14
  (:require
   [aoc-2020.utils :as utils]
   [clojure.string :as str]))

(def example
  ["mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
   "mem[8] = 11"
   "mem[7] = 101"
   "mem[8] = 0"])

(defn- get-bitmasks [raw]
  {:one
   (-> raw
       (str/replace #"X" "0")
       (Long/parseLong 2))
   :zero
   (-> raw
       (str/replace #"X" "1")
       (Long/parseLong 2))})

(defn apply-bitmasks [{:keys [one zero]} val]
  (->> val
       (bit-or one)
       (bit-and zero)))

(defn parse-instruction [raw]
  (if-let [mask-match (re-matches #"mask = (.*)" raw)]
    (get-bitmasks (second mask-match))
    (let [set-match (re-matches #"mem\[(\d+)\] = (\d+)" raw)]
      {:addr (Long/parseLong (second set-match))
       :val  (Long/parseLong (nth set-match 2))})))

(defn execute-instruction
  [{:keys [bitmasks] :as state}
   {:keys [addr val] bitmask? :one :as instruction}]
  (if bitmask?
    (assoc state :bitmasks instruction)
    (assoc-in state [:memory addr] (apply-bitmasks bitmasks val))))

(defn solve-1 [raw]
  (->> raw
       (map parse-instruction)
       (reduce execute-instruction {:bitmasks nil :memory {}})
       :memory
       vals
       (apply +)))

(comment
  (solve-1 example)
  (solve-1 (utils/get-problem-input "day_14")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Part II
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def example-2
  ["mask = 000000000000000000000000000000X1001X"
   "mem[42] = 100"
   "mask = 00000000000000000000000000000000X0XX"
   "mem[26] = 1"])

(defn prepare-bitmask [raw]
  [(vec (str/replace raw #"0" "1")) (vec raw)])

(defn generate-bitmasks [[z o]]
  (let [x-ind (.indexOf z \X)]
    (if (not= x-ind (- 1))
      (concat
       (generate-bitmasks [(assoc z x-ind \1) (assoc o x-ind \1)])
       (generate-bitmasks [(assoc z x-ind \0) (assoc o x-ind \0)]))
      [{:zero (Long/parseLong (apply str z) 2)
        :one  (Long/parseLong (apply str o) 2)}])))

(defn parse-instruction-2 [raw]
  (if-let [mask-match (re-matches #"mask = (.*)" raw)]
    {:bitmasks (-> mask-match
                   second
                   prepare-bitmask
                   generate-bitmasks)}
    (let [set-match (re-matches #"mem\[(\d+)\] = (\d+)" raw)]
      {:addr (Long/parseLong (second set-match))
       :val  (Long/parseLong (nth set-match 2))})))

(defn update-state [{:keys [bitmasks] :as state} addr val]
  (->> bitmasks
       (map #(apply-bitmasks % addr))
       (reduce #(assoc-in %1 [:memory %2] val) state)))

(defn execute-instruction-2
  [state
   {:keys [addr val bitmasks]}]
  (if bitmasks
    (assoc state :bitmasks bitmasks)
    (update-state state addr val)))

(defn solve-2 [raw]
  (->> raw
       (map parse-instruction-2)
       (reduce execute-instruction-2 {:bitmasks nil :memory {}})
       :memory
       vals
       (apply +)))

(comment
  (solve-2 example-2)
  (solve-2 (utils/get-problem-input "day_14")))
