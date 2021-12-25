(ns aoc.puzzle3
  (:require
   [clojure.string :as str]
   [clojure.set :as cset]
   [clojure.java.io :as io]))

(def input-values
  (line-seq (io/reader "resources/puzzle4.txt")))

(defn parse-input
  [input-values]
  (->> input-values
       (map #(str/split % #"^$"))
       (partition-by #(= % [""]))
       (filter #(not= % '([""])))))

(defn keep-digits
  "strips everything that isnt a digit"
  [string]
  (re-find #"\d+" string))

(defn polish-row-string-content
  "splits string by spaces and keeps nothing but the digits"
  [row-string]
  (filter #(keep-digits %) (str/split row-string #" ")))

(defn row-strings->int
  "takes a row from input and formats as int"
  [row]
  (->> row
       first
       polish-row-string-content
       (map #(Integer/parseInt %))))

(defn parse-draws
  "parses the draws coming in as a string with values seperated by comma"
  [draws-comma-sep]
  (map #(Integer/parseInt %) (str/split draws-comma-sep #",")))

(defn parse-games
  "parses the games / formats game's rows"
  [games]
  (for [game games]
    (for [row game
          :let [res (row-strings->int row)]]
      (vec res))))

(defn extract-draws-and-games
  [input-values]
  (let [[draws & games] (parse-input input-values)
        parsed-draws (parse-draws (first (first draws)))
        parsed-games (vec (parse-games games))]
    {:draws parsed-draws :games parsed-games}))

(defn transpose-matrix
  "transposes a matrix of rows and columns"
  [matrix]
  (for [i (range (count (first matrix)))]
    (for [entry matrix]
      (nth entry i))))


(defn empty-bingo-game-mask
  []
  [[false false false false false]
   [false false false false false]
   [false false false false false]
   [false false false false false]
   [false false false false false]])

;; Part 1

(def game-data (extract-draws-and-games input-values))

(defn game-map-default [i game]
  {:id i
   :rows game
   :columns (transpose-matrix game)
   :score (reduce + (flatten game))
   :draws []
   :hits-row (empty-bingo-game-mask)
   :hits-column (empty-bingo-game-mask)})

(defn map-draws
  [game-data]
  (game-data :draws))

(defn map-games
  [game-data]
  (let [games (game-data :games)]
    (for [i (range (count games))
          :let [game (nth games i)]]
      (game-map-default i game))))

(defn draw-in-game
  "Return Row/Columns as Boolean Mask where a hit was matched"
  [draw rows-or-columns]
  (map (fn [row]
         (map (fn [entry]
                filter (= entry draw))
              row))
       rows-or-columns))

(defn apply-bool-mask
  "applies a boolean mask to entries in a matrix"
  [mask-old mask-new]
  (let [result (for [i (range (count mask-old))]
                 (for [j (range (count (nth mask-old i)))
                       :let [new-i (nth (nth mask-new i) j)
                             new-o (nth (nth mask-old i) j)]]
                   (or new-i new-o)))]
    result))

(defn eval-draws-for-game
  "runs a draw through a bingo game sheet"
  [draw game]
  (let [{rows :rows
         columns :columns
         draws :draws
         hits-row :hits-row
         hits-column :hits-column} game
        hit-row (draw-in-game draw rows)
        hit-column (draw-in-game draw columns)]
    (assoc game
           :draws (cons draw draws)
           :hits-row (apply-bool-mask hits-row hit-row)
           :hits-column (apply-bool-mask hits-column hit-column))))

(defn identify-finished-list
  "checks for a 'BINGO!'"
  [row-or-column]
  (->> row-or-column
       (map (fn [item] (filter #(= % true) item)))
       (map count)
       (filter #(= % 5))))

(defn bingo?
  [hits-row hits-column]
  (or
   (not= () (identify-finished-list hits-row))
   (not= () (identify-finished-list hits-column))))

(defn game-bingo [games]
  (for [game games]
    (let [{hits-row :hits-row
           hits-column :hits-column} game
          result (bingo? hits-row hits-column)]
      (if (= false result) {} game))))

(defn play
  "runs the draws through games"
  [draws games-mapped]
  (loop [draws draws
         games games-mapped]
    (let [draw (first draws)
          bingo (game-bingo games)]
      (cond
        (or (= () draw) (nil? draw)) games
        (> (count (filter #(:id %) bingo)) 0) bingo
        :else (recur
               (drop 1 draws)
               (for [game games] (eval-draws-for-game draw game)))))))

(defn evaluate-bingo-game
  "scores the bingo game for the aoc challenge"
  [game]
  (let [{draws :draws} game
        last-drawn (first draws)
        flat-rows (flatten (:rows game))
        rows-flat-set (set flat-rows)
        draws-in-row (apply cset/intersection (map set [flat-rows draws]))
        drawn-hits (cset/difference rows-flat-set draws-in-row)
        sum-of-unmarked (apply + drawn-hits)
        bingo-score (* last-drawn sum-of-unmarked)]
    (prn "Bingo Score: " bingo-score)))

;; execution part1

;; run part1
(time
 (->
  (filter #(:id %) (play (map-draws game-data) (map-games game-data)))
  (first)
  (evaluate-bingo-game)))

;; part2

(defn find-game-by-id
  [bingo id]
  (->>
   bingo
   (filter #(= (:id %) (first id)))
   (first)))

(defn evaluate-last-finished-bingo-game
  [bingo last-bingo-game-won-id]
  (evaluate-bingo-game (find-game-by-id bingo last-bingo-game-won-id)))

(defn no-draws-left
  [draw]
  (or (= () draw) (nil? draw)))

(defn extract-game-id
  [bingo]
  (->>
   bingo
   (filter #(:id %))
   (map (fn [game] (:id game)))))

(defn play-to-the-end
  [draws games-mapped]
  (loop [draws draws
         games games-mapped
         last-bingo #{}]
    (let [draw (first draws)
          bingo (game-bingo games)
          bingo-set (set (extract-game-id bingo))
          update-bingo-set (cset/union last-bingo bingo-set)
          last-bingo-game-won-id (cset/difference bingo-set last-bingo)]
      (cond
        (no-draws-left draw) "no bingo"
        (= (count bingo-set) (count games)) (evaluate-last-finished-bingo-game bingo last-bingo-game-won-id)
        :else (recur
               (drop 1 draws)
               (for [game games]
                 (eval-draws-for-game draw game))
               update-bingo-set)))))

(time (play-to-the-end (map-draws game-data) (map-games game-data)))
