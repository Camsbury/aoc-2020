(ns aoc-2020.day-2.problem-1
  (:require
   [aoc-2020.utils       :as utils]
   [aoc-2020.day-2.utils :as day-utils]))

(defn valid-password?
  "checks if a password is valid according to its policy"
  [{:keys [password letter fst snd]}]
  (->> password
       (filter #(= letter %))
       count
       (#(<= fst % snd))))

(defn count-valid-passwords
  "counts valid passwords in a list of entries"
  [entries]
  (->> entries
       (filter valid-password?)
       count))

(comment
  (->>
   (utils/get-problem-input)
   (map day-utils/process-entry)
   count-valid-passwords))
