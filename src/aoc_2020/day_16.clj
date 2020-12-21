(ns aoc-2020.day-16
  (:require
   [aoc-2020.utils :as utils]
   [clojure.set    :as set]
   [clojure.string :as str]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Part I
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; extract

(defn parse-rule [raw]
  (let [[name from1 to1 from2 to2]
        (rest (re-matches #"(.*): (\d+)-(\d+) or (\d+)-(\d+)" raw))]
    [name #(or (<= (Integer/parseInt from1) % (Integer/parseInt to1))
               (<= (Integer/parseInt from2) % (Integer/parseInt to2)))]))

(defn parse-rules [raw]
  (into {} (map parse-rule) raw))

(defn parse-ticket [raw]
  (map #(Integer/parseInt %) (str/split raw #",")))

(defn get-rules-and-tickets [raw]
  (let [[rules yours nearby]
        (->>
         (str/split raw #"\n\n")
         (map #(str/split % #"\n")))]
    {:rules (parse-rules rules)
     :yours (parse-ticket (second yours))
     :nearby (map parse-ticket (rest nearby))}))

(def example
  {:rules (parse-rules
           ["class: 1-3 or 5-7"
            "row: 6-11 or 33-44"
            "seat: 13-40 or 45-50"])
   :yours (parse-ticket "7,1,14")
   :nearby (map parse-ticket
                ["7,3,47"
                 "40,4,50"
                 "55,2,20"
                 "38,6,12"])})

;; solve

(defn solve-p1 [{:keys [rules _ nearby]}]
  (let [any-rule? (fn [n] (some #(% n) (vals rules)))
        g #(if (any-rule? %2)
             %1
             (+ %1 %2))
        f #(reduce g %1 %2)]
    (reduce f 0 nearby)))

;;

(comment
  (solve-p1 example)
  (solve-p1
   (get-rules-and-tickets (utils/get-problem-input-file "day_16"))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Part II
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn valid-rule? [rules ticket]
  (let [any-rule? (fn [n] (some #(% n) (vals rules)))]
    (every? any-rule? ticket)))

(defn discard-invalid [{:keys [rules nearby] :as data}]
  (let [conj-valid (fn [acc ticket]
                      (if (valid-rule? rules ticket)
                        (conj acc ticket)
                        acc))]
    (->> nearby
         (reduce conj-valid [])
         (assoc data :nearby))))

(defn possible-labels [rules labels number]
  (reduce (fn [acc label]
            (if ((get rules label) number)
              (conj acc label)
              acc))
          #{}
          labels))

(defn- get-candidates [rules nums]
  (reduce (fn [acc [label pred]]
            (if (every? pred nums)
              (conj acc label)
              acc)) #{} rules))

(defn reduce-ordering [ans candidates]
  (let [[singles leftovers]
        ((juxt filter remove) #(= 1 (count (second %))) candidates)
        ans (reduce (fn [acc [idx label-set]] (assoc acc idx (first label-set))) ans singles)
        singles (apply set/union (map second singles))
        leftovers (map #(update % 1 set/difference singles) leftovers)]
    (if (seq leftovers)
      (recur ans leftovers)
      ans)))

(defn label-ordering [{:keys [rules nearby]}]
  (->> nearby
       utils/transpose
       (mapv #(get-candidates rules %))
       (map-indexed (fn [idx itm] [idx itm]))
       (reduce-ordering (vec (repeat (count rules) nil)))))

(defn solve-p2 [data]
  (let [{:keys [yours] :as data} (discard-invalid data)
        ordering (label-ordering data)]
    (->> ordering
         (map-indexed (fn [idx label] [idx label]))
         (keep
          (fn [[idx label]]
            (when (str/includes? label "departure")
              idx)))
         (map #(nth yours %))
         (reduce *))))

(comment
  (label-ordering (discard-invalid example))
  (label-ordering (discard-invalid (get-rules-and-tickets (utils/get-problem-input-file "day_16"))))
  (solve-p2 (get-rules-and-tickets (utils/get-problem-input-file "day_16"))))

;; departure fields multiplied
