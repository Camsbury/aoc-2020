(ns aoc-2020.day-20
  (:require
   [aoc-2020.utils :as utils]
   [clojure.set    :as set]
   [clojure.string :as str]))

(def example
  ["Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###"

   "Tile 1951:
#.##...##.
#.####...#
.....#..##
#...######
.##.#....#
.###.#####
###.##.##.
.###....#.
..#.#..#.#
#...##.#.."

   "Tile 1171:
####...##.
#..##.#..#
##.#..#.#.
.###.####.
..###.####
.##....##.
.#...####.
#.##.####.
####..#...
.....##..."

   "Tile 1427:
###.##.#..
.#..#.##..
.#.##.#..#
#.#.#.##.#
....#...##
...##..##.
...#.#####
.#.####.#.
..#..###.#
..##.#..#."

   "Tile 1489:
##.#.#....
..##...#..
.##..##...
..#...#...
#####...#.
#..#.#.#.#
...#.#.#..
##.#...##.
..##.##.##
###.##.#.."

   "Tile 2473:
#....####.
#..#.##...
#.##..#...
######.#.#
.#...#.#.#
.#########
.###.#..#.
########.#
##...##.#.
..###.#.#."

   "Tile 2971:
..#.#....#
#...###...
#.#.###...
##.##..#..
.#####..##
.#..####.#
#..#.#..#.
..####.###
..#.#.###.
...#.#.#.#"

   "Tile 2729:
...#.#.#.#
####.#....
..#.#.....
....#..#.#
.##..##.#.
.#.####...
####.#.#..
##.####...
##..#.##..
#.##...##."

   "Tile 3079:
#.#.#####.
.#..######
..#.......
######....
####.#..#.
.#...#.##.
#.#####.##
..#.###...
..#.......
..#.###...
"])

(def e
  ["#.#.#####."
   ".#..######"
   "..#......."
   "######...."
   "####.#..#."
   ".#...#.##."
   "#.#####.##"
   "..#.###..."
   "..#......."
   "..#.###..."])

(defn side->nums [side]
  (let [num
        (-> side
            (str/replace #"#" "1")
            (str/replace #"\." "0"))]
    {:clockwise (Integer/parseInt num 2)
     :counter-clockwise (Integer/parseInt (str/reverse num) 2)}))

(defn tile->side-strings [tile]
  [(first tile)
   (apply str (map last tile))
   (str/reverse (last tile))
   (->> tile
        (map first)
        reverse
        (apply str))])

(defn parse-tile [tile]
  (let [[raw-id & body]
        (str/split-lines tile)]
    {:id (->> raw-id
              (re-matches #"Tile (\d+):")
              second
              Integer/parseInt)

     :sides (->> body
                 tile->side-strings
                 (map side->nums))
     :char-matrix (map seq body)}))

(defn side->key [{:keys [clockwise counter-clockwise]}]
  (vec (sort [clockwise counter-clockwise])))

(defn tile->sides [tile]
  (->> tile
       :sides
       (map side->key)))

(defn- add-side-count [counter side]
  (update counter side (fnil inc 0)))

(defn tiles->unique-sides [tiles]
  (->> tiles
       (mapcat tile->sides)
       (reduce add-side-count {})
       (keep (fn [[tile cnt]] (when (= 1 cnt) tile)))
       set))

(defn corner? [unique-sides tile]
  (->> tile
       tile->sides
       (filter unique-sides)
       count
       (= 2)))

(defn solve-p1 [raw]
  (let [tiles (map parse-tile raw)
        unique-sides (tiles->unique-sides tiles)]
    (->> tiles
         (filter #(corner? unique-sides %))
         (map :id)
         (reduce *))))

(comment
  (solve-p1 example)
  (-> "day_20"
      utils/get-problem-input-file
      (str/split #"\n\n")
      solve-p1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PART II
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn char-matrix->image
  [char-matrix]
  (->> char-matrix
       (map #(apply str %))
       (str/join "\n")))

(defn rotate-char-matrix [char-matrix]
  (->> char-matrix
       (apply map list)
       (map reverse)))

(defn rotate-sides [[a b c d]]
  [d a b c])

(defn flip-side [{c :clockwise cc :counter-clockwise}]
  {:clockwise cc
   :counter-clockwise c})

(defn flip-sides [[a b c d]]
  (mapv flip-side [c b a d]))

(defn- tiles->anchor [tiles]
  (let [unique-sides (tiles->unique-sides tiles)
        {:keys [id sides char-matrix]}
        (->> tiles
             (filter #(corner? unique-sides %))
             first)]
    (loop [sides sides char-matrix char-matrix]
      (if (and
           (->> sides
                first
                side->key
                unique-sides)
           (->> sides
                last
                side->key
                unique-sides))
        {:char-matrix char-matrix
         :id id
         :bottom (nth sides 2)
         :right (nth sides 1)
         :remaining-ids (->> tiles (map :id) (remove #{id}) set)}
        (recur (rotate-sides sides) (rotate-char-matrix char-matrix))))))

(defn- get-matching-tile [side-key tiles]
  (->> tiles
       (filter
        #(contains?
          (->> % :sides (map side->key) set)
          side-key))
       first))

(defn- match-horizontal [right tiles]
  (let [side-key (side->key right)
        {:keys [id
                sides
                char-matrix]} (get-matching-tile side-key tiles)]
    (loop [sides sides char-matrix char-matrix cnt 0]
      (cond
        (= (:counter-clockwise right) (:clockwise (last sides)))
        {:id id
         :right (second sides)
         :char-matrix char-matrix}
        (= side-key (side->key (last sides)))
        (recur (flip-sides sides) (reverse char-matrix) (inc cnt))
        :else
        (recur (rotate-sides sides) (rotate-char-matrix char-matrix) (inc cnt))))))

(defn- match-vertical [bottom tiles]
  (let [side-key (side->key bottom)
        {:keys [id
                sides
                char-matrix]} (get-matching-tile side-key tiles)]
    (loop [sides sides char-matrix char-matrix cnt 0]
      (cond
        (= (:counter-clockwise bottom) (:clockwise (first sides)))
        {:id id
         :right (second sides)
         :bottom (nth sides 2)
         :char-matrix char-matrix}
        (= side-key (side->key (first sides)))
        (recur (flip-sides sides) (reverse char-matrix) (inc cnt))
        :else
        (recur (rotate-sides sides) (rotate-char-matrix char-matrix) (inc cnt))))))

(defn image->char-matrix [image]
  (->> image
       str/split-lines
       (map seq)))

(defn- drop-border [tile]
  (->> tile
       rest
       (drop-last)
       (map rest)
       (map drop-last)))

(defn- merge-tiles [tiles]
  (mapv #(reduce into [] %) (utils/transpose tiles)))
(defn- merge-files [files]
  (reduce into [] files))
(defn- merge-image [tile-matrix]
  (->> tile-matrix
       (map #(map drop-border %))
       (map merge-tiles)
       merge-files
       char-matrix->image))

(defn build-image
  "Build up the final image of the sea"
  [tiles]
  (let [side-count (int (Math/sqrt (count tiles)))
        {:keys
         [char-matrix
          bottom
          right
          remaining-ids]} (tiles->anchor tiles)]
    (loop [tile-matrix []
           file [char-matrix]
           remaining-ids remaining-ids
           bottom bottom
           right right]
      (let [candidates (filter #(contains? remaining-ids (:id %)) tiles)]
        (cond
          (and (= side-count (count tile-matrix))
               (= side-count (count (last tile-matrix)))
               (empty? candidates))
          (merge-image tile-matrix)
          ;; tile-matrix
          (empty? file)
          (let [{:keys [bottom
                        right
                        id
                        char-matrix]} (match-vertical bottom candidates)]
            (recur tile-matrix
                   [char-matrix]
                   (disj remaining-ids id)
                   bottom
                   right))
          (< (count file) side-count)
          (let [{:keys [right id char-matrix]} (match-horizontal right candidates)]
            (recur tile-matrix
                   (conj file char-matrix)
                   (disj remaining-ids id)
                   bottom
                   right))
          :else
          (recur (conj tile-matrix file) [] remaining-ids bottom nil))))))

(def monster-matcher
  #"..................#.\n#....##....##....###\n.#..#..#..#..#..#...")

(defn image->chunks [image]
  (let [monster-length 20
        monster-height 3
        map-lines (str/split-lines image)
        row-length (count (first map-lines))
        col-length (count map-lines)]
    (for [x (range (- (inc row-length) monster-length))
          y (range (- (inc col-length) monster-height))]
      (->> map-lines
           (drop y)
           (take monster-height)
           (map #(subs % x (+ x monster-length)))
           (str/join "\n")))))

(defn count-sea-monsters-naive [image]
  (->> image
       image->chunks
       (keep #(re-matches monster-matcher %))
       count))

(defn get-candidate-maps
  "Get all transpositions of the map to check for monsters"
  [image]
  (let [char-matrix (image->char-matrix image)]
    (for [n-rotate (range 4)
          flip? [true false]]
      (-> char-matrix
          ((utils/n-times rotate-char-matrix n-rotate))
          (cond-> flip? reverse)
          char-matrix->image))))

(defn count-sea-monsters
  "Count the number of sea monsters in the image"
  [image]
  (->> image
       get-candidate-maps
       (map count-sea-monsters-naive)
       (apply max)))

(defn solve-p2
  "Count the rough waves or whatever bro"
  [raw]
  (let [tiles (map parse-tile raw)
        image (build-image tiles)
        num-hashes (count (re-seq #"#" image))
        num-monsters (count-sea-monsters image)
        monster-hashes 15]
    (- num-hashes (* monster-hashes num-monsters))))

(comment
  (solve-p2 example)
  (-> "day_20"
      utils/get-problem-input-file
      (str/split #"\n\n")
      solve-p2))



