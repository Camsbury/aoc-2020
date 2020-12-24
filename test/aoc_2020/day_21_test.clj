(ns aoc-2020.day-21-test
  (:require [aoc-2020.day-21 :as sut]
            [clojure.test :as t]
            [clojure.set :as set]))

(def example
  ["mxmxvkd kfcds sqjhc nhms (contains dairy, fish)"
   "trh fvjkl sbzzf mxmxvkd (contains dairy)"
   "sqjhc fvjkl (contains soy)"
   "sqjhc mxmxvkd sbzzf (contains fish)"])

(def example-recipes
  [{:ingredients #{"sqjhc" "nhms" "kfcds" "mxmxvkd"}
    :allergens #{"dairy" "fish"}}
   {:ingredients #{"fvjkl" "trh" "sbzzf" "mxmxvkd"}
    :allergens #{"dairy"}}
   {:ingredients #{"sqjhc" "fvjkl"}
    :allergens #{"soy"}}
   {:ingredients #{"sqjhc" "sbzzf" "mxmxvkd"}
    :allergens #{"fish"}}])

(def all-ingredients
  #{"sqjhc" "fvjkl" "nhms" "trh" "kfcds" "sbzzf" "mxmxvkd"})

(def all-allergens
  #{"dairy" "soy" "fish"})

(def fish-ings
  #{"sqjhc" "mxmxvkd"})

(def all-candidates
  #{"sqjhc" "fvjkl" "mxmxvkd"})

(def non-allergens
  #{"kfcds"
    "nhms"
    "sbzzf"
    "trh"})


(t/deftest assemble-recipes-test
  (t/testing "assemble-recipes works"
    (t/is (= example-recipes
             (sut/assemble-recipes example)))))

(t/deftest extract-union-test
  (t/testing "extract-union for ingredients"
    (t/is (= all-ingredients
             (sut/extract-union example-recipes :ingredients))))
  (t/testing "extract-union for allergens"
    (t/is (= all-allergens
             (sut/extract-union example-recipes :allergens)))))

(t/deftest candidates-test
  (t/testing "candidates are correct"
    (t/is (= fish-ings
             (sut/get-candidate-ingredients "fish" example-recipes)))
    (t/is (= all-candidates
             (sut/get-all-allergy-candidates all-allergens example-recipes)))
    (t/is (= non-allergens
             (set/difference all-ingredients all-candidates)))))

(t/deftest count-used-test
  (t/testing "is right"
    (t/is (= 3
             (sut/count-used example-recipes "sqjhc")))))

(t/deftest solve-p1-test
  (t/testing "is right"
    (t/is (= 5
             (sut/solve-p1 example)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PART II
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(t/deftest solve-p2-test
  (t/testing "is right"
    (t/is (= "mxmxvkd,sqjhc,fvjkl"
             (sut/solve-p2 example)))))
