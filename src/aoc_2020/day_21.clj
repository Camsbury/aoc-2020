(ns aoc-2020.day-21
  (:require
   [aoc-2020.utils :as utils]
   [clojure.set    :as set]
   [clojure.string :as str]
   [instaparse.core :as insta]))

(def example
  ["mxmxvkd kfcds sqjhc nhms (contains dairy, fish)"
   "trh fvjkl sbzzf mxmxvkd (contains dairy)"
   "sqjhc fvjkl (contains soy)"
   "sqjhc mxmxvkd sbzzf (contains fish)"])

(def parse-recipe
  (insta/parser
   "RECIPE = INGS CONTS
    INGS   = (ING SPC)+
    ING    = #'\\w+'
    SPC    = ' '
    CONTS  = '(contains ' CONTS2 ')'
    CONTS2 = (ALLER SEP*)+
    ALLER  = #'\\w+'
    SEP    = ', '"))

(defmulti  extract-recipe first)
(defmethod extract-recipe :RECIPE
  [[_ [_ & ings] [_ _ [_ & allers] _]]]
  (let [extract #(into #{} (keep extract-recipe) %)]
    {:ingredients (extract ings)
     :allergens (extract allers)}))
(defmethod extract-recipe :ING [[_ ing]] ing)
(defmethod extract-recipe :ALLER [[_ aller]] aller)
(defmethod extract-recipe :default [_] nil)

(defn assemble-recipes [raw]
  (mapv (comp extract-recipe parse-recipe) raw))

(defn get-candidate-ingredients [allergen recipes]
  (->> recipes
       (filter #(contains? (:allergens %) allergen))
       (map :ingredients)
       (apply set/intersection)))

(defn get-all-allergy-candidates [allergens recipes]
  (->> allergens
       (map #(get-candidate-ingredients % recipes))
       (apply set/union)))

(defn extract-union [xs k]
  (->> xs
       (map k)
       (apply set/union)))

(defn count-used [recipes ing]
  (->> recipes
       (filter #(contains? (:ingredients %) ing))
       count))

(defn solve-p1 [raw]
  (let [recipes
        (assemble-recipes raw)

        ingredients
        (extract-union recipes :ingredients)

        allergens
        (extract-union recipes :allergens)]

    (->> recipes
         (get-all-allergy-candidates allergens)
         (set/difference ingredients)
         (map #(count-used recipes %))
         (reduce +))))

(comment
  (solve-p1 (utils/get-problem-input "day_21")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PART II
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; borrowed from day 16
(defn reduce-candidates [ans candidates]
  (let [[singles leftovers]
        ((juxt filter remove) #(= 1 (count (second %))) candidates)
        ans (reduce (fn [acc [allergen ingredients]]
                      (assoc acc allergen (first ingredients)))
                    ans
                    singles)
        singles (apply set/union (map second singles))
        leftovers (map #(update % 1 set/difference singles) leftovers)]
    (if (seq leftovers)
      (recur ans leftovers)
      ans)))

(defn solve-p2 [raw]
  (let [recipes (assemble-recipes raw)

        allergens (extract-union recipes :allergens)

        allergen->candidates
        (fn [allergen]
          [allergen (get-candidate-ingredients allergen recipes)])

        candidates-by-allergen (mapv allergen->candidates allergens)
        ingredients-by-allergen (reduce-candidates {} candidates-by-allergen)]
    (->>
     ingredients-by-allergen
     (sort-by first)
     (map second)
     (str/join ","))))

(comment
  (solve-p2 (utils/get-problem-input "day_21")))
