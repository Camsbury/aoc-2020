(ns aoc-2020.day-18
  (:require
   [aoc-2020.utils :as utils]
   [clojure.set    :as set]
   [clojure.string :as str]
   [instaparse.core :as insta]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PART I
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def example
  ["2 * 3 + (4 * 5)"
   "5 + (8 * 3 + 9 + 3 * 4 * 3)"
   "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"
   "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"])
(def example-answers
  [26
   437
   12240
   13632])

(def parse-exp
  (insta/parser
   "EXP = VAL OP*
    OP  = (MUL | ADD) VAL
    MUL = ' * '
    ADD = ' + '
    VAL = NUM | PAREN
    PAREN = '(' EXP ')'
    NUM = #'[0-9]'+"))

(defmulti eval-exp first)
(defmethod eval-exp :NUM
  [[_ n]]
  (Integer/parseInt n))
(defmethod eval-exp :PAREN
  [[_ _ exp _]]
  (eval-exp exp))
(defmethod eval-exp :VAL
  [[_ v]]
  (eval-exp v))
(defmethod eval-exp :MUL [_] *)
(defmethod eval-exp :ADD [_] +)
(defmethod eval-exp :OP
  [[_ op exp]]
  #((eval-exp op) % (eval-exp exp)))
(defmethod eval-exp :EXP
  [[_ v & ops]]
  (reduce #(%2 %1) (eval-exp v) (map eval-exp ops)))

(defn solve-p1 [exps]
  (->> exps
       (map parse-exp)
       (map eval-exp)
       (reduce +)))

(comment
  (solve-p1 example)
  (solve-p1 (utils/get-problem-input "day_18")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PART II
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def parse-exp-2
  (insta/parser
   "EXP = (VAL MUL*)
    ADD = VAL ' + ' VAL
    MUL = ' * ' VAL
    VAL = NUM | ADD | PAREN
    PAREN = '(' EXP ')'
    NUM = #'[0-9]'+"))

(defmulti eval-exp-2 first)
(defmethod eval-exp-2 :NUM
  [[_ n]]
  (Integer/parseInt n))
(defmethod eval-exp-2 :PAREN
  [[_ _ exp _]]
  (eval-exp-2 exp))
(defmethod eval-exp-2 :VAL
  [[_ v]]
  (eval-exp-2 v))
(defmethod eval-exp-2 :ADD
  [[_ v1 _ v2]]
  (+ (eval-exp-2 v1) (eval-exp-2 v2)))
(defmethod eval-exp-2 :MUL
  [[_ _ exp]]
  #(* % (eval-exp-2 exp)))
(defmethod eval-exp-2 :EXP
  [[_ v & muls]]
  (reduce #(%2 %1) (eval-exp-2 v) (map eval-exp-2 muls)))

(defn solve-p2 [exps]
  (->> exps
       (map parse-exp-2)
       (map eval-exp-2)
       (reduce +)))

(comment
  (eval-exp-2 (parse-exp-2 "2 * 3 + (4 * 5)"))
  (solve-p2 (utils/get-problem-input "day_18")))
