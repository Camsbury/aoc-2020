(ns aoc-2020.day-2.problem-2
  (:require
   [aoc-2020.utils       :as utils]
   [aoc-2020.day-2.utils :as day-utils]))

(defn valid-password?
  "checks if a password is valid according to its policy"
  [{:keys [password letter fst snd]}]
  (let [fst (dec fst)
        snd (dec snd)]
      (or (and (=    letter (nth password fst))
               (not= letter (nth password snd)))
          (and (not= letter (nth password fst))
               (=    letter (nth password snd))))))

(defn count-valid-passwords
  "counts valid passwords in a list of entries"
  [entries]
  (->> entries
       (filter valid-password?)
       count))

(comment
  (valid-password?
   {:password "ccccccccc", :letter \c, :fst 2, :snd 9})
  (->>
   (utils/get-problem-input)
   (map day-utils/process-entry)
   count-valid-passwords))
