(ns aoc.puzzle8
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as cset]))

(defn read-input-file
  "filename as string path"
  [filename]
  (-> (io/reader filename)
      (line-seq)))

(def demo "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce
")

(def demo-input (str/split-lines demo))
(def output (flatten (map #(last (str/split % #" \| ")) demo-input)))
(def demo-input-parsed
  (str/split (str/join " " output) #" "))

(defn sort-string-alphabetically
  [string]
  (str/join #"" (sort string)))

(defn sort-list-of-strings-alphabetically
  [entries]
  (map #(sort-string-alphabetically %) entries))

;; part1

(defn parse-input-after-pipe
  [input-lines]
  (->> (flatten (map #(last (str/split % #" \| ")) input-lines))))

(defn parse-input-after-pipe-joined
  [input-lines]
  (-> (str/join " " (parse-input-after-pipe input-lines))
      (str/split #" ")))

(defn simple-find-by-count
  [counter string]
  (and
   (= counter (count string))
   (re-matches (re-pattern (str "[" string "]+")) string)))

(defn find-length-2
  [string]
  (simple-find-by-count 2 string))

(defn find-length-3
  [string]
  (simple-find-by-count 3 string))

(defn find-length-4
  [string]
  (simple-find-by-count 4 string))


(defn find-length-7
  [string]
  (simple-find-by-count 7 string))

(defn is-digit?
  [string]
  (cond
    (find-length-7 string) 8
    (find-length-4 string) 7
    (find-length-3 string) 4
    (find-length-2 string) 1
    :else 0))

;; execution part1

(read-input-file "resources/puzzle8.txt")

(->> "resources/puzzle8.txt"
     read-input-file
     parse-input-after-pipe-joined)

(->> "resources/puzzle8.txt"
     read-input-file
     parse-input-after-pipe-joined)

(->>
 "resources/puzzle8.txt"
 (read-input-file)
 (parse-input-after-pipe-joined)
 (map #(is-digit? %))
 (apply +)
 (prn "Part 1:"))

;; part2

(def demo [["acedgfb"
            "cdfbe"
            "gcdfa"
            "fbcad"
            "dab"
            "cefabd"
            "cdfgeb"
            "eafb"
            "cagedb"
            "ab"
            "|" "cdfeb" "fcadb" "cdfeb" "cdbaf"]])

(defn find-by-length
  [entries length]
  (filter #(= (count %) length) entries))

(defn find-by-length-first
  [entries length]
  (first (find-by-length entries length)))

(defn find-digit-1
  [entries]
  (find-by-length-first entries 2))

(defn find-digit-7
  [entries]
  (find-by-length-first entries 3))

(defn find-digit-4
  [entries]
  (find-by-length-first entries 4))

(defn find-digit-8
  [entries]
  (find-by-length-first entries 7))

(defn find-5-chars
  [entries]
  (set (find-by-length entries 5)))

(defn find-6-chars
  [entries]
  (set (find-by-length entries 6)))

(defn is-digit-9?
  [digit digit-4]
  (let [diff (cset/difference (set digit) digit-4)]
    (= (count diff) 2)))

(defn find-digit-9
  [entries digit-4]
  (let [digit-9 (first (filter #(is-digit-9? % digit-4) entries))
        rest-digits (filter #(not= % digit-9) entries)]
    [digit-9 rest-digits]))

(defn is-digit-0?
  [digit-1 first-digit]
  (let [diff (cset/difference first-digit digit-1)]
    (= (count diff) 4)))

(defn find-digit-0
  [digits-6-or-0 digit-1]
  (let [first-digit (set (first digits-6-or-0))
        second-digit (set (second digits-6-or-0))
        [digit-0 digit-6] (if (is-digit-0? digit-1 first-digit)
                            [first-digit second-digit]
                            [second-digit first-digit])]
    (map #(str/join %) [digit-0 digit-6])))

(defn is-digit-3?
  [digit digit-1]
  (let [diff (cset/difference (set digit) digit-1)]
    (= (count diff) 3)))

(defn find-digit-3
  [entries digit-1]
  (let [digit-3 (first (filter #(is-digit-3? % digit-1) entries))
        rest-digits (filter #(not= % digit-3) entries)]
    [digit-3 rest-digits]))

(defn is-digit-5?
  [first-digit digit-6]
  (let [diff (cset/difference first-digit (set digit-6))]
    (= (count diff) 0)))

(defn find-digit-5
  [digits-5-or-2 digit-6]
  (let [first-digit (set (first digits-5-or-2))
        second-digit (set (second digits-5-or-2))
        [digit-5 digit-2] (if (is-digit-5? first-digit digit-6)
                            [first-digit second-digit]
                            [second-digit first-digit])]
    (map #(str/join #"" %) [digit-5 digit-2])))

(defn decode
  [entries]
  (let
   [digit-1 (find-digit-1 entries)
    digit-4 (find-digit-4 entries)
    digit-7 (find-digit-7 entries)
    digit-8 (find-digit-8 entries)
    length-6-char (find-6-chars entries)
    length-5-char (find-5-chars entries)
    [digit-9 digits-6-or-0] (find-digit-9 length-6-char digit-4)
    [digit-0 digit-6] (find-digit-0 digits-6-or-0 digit-1)
    [digit-3 digits-5-or-2] (find-digit-3 length-5-char digit-1)
    [digit-5 digit-2] (find-digit-5 digits-5-or-2 digit-6)]
    {digit-0 0
     digit-1 1
     digit-2 2
     digit-3 3
     digit-4 4
     digit-5 5
     digit-6 6
     digit-7 7
     digit-8 8
     digit-9 9}))

(defn score
  [target-entries-sorted digit-map]
  (let [string-list (map #(str (get digit-map %)) target-entries-sorted)
        score-string (str/join #"" string-list)
        score (Integer/parseInt score-string)]
    score))

(def demo (let
           [input (read-input-file "resources/puzzle8.txt")
            input-split (map (fn [string] (str/split string #" ")) input)]
            input-split))

(let
 [parsed-scores  (for [entries demo]
                   (let [entries-sorted (set (sort-list-of-strings-alphabetically (take 10 entries)))
                         target-entries-sorted (sort-list-of-strings-alphabetically (drop 11 entries))]
                     (score target-entries-sorted (decode entries-sorted))))]
  (prn parsed-scores)
  (prn (apply + parsed-scores)))
