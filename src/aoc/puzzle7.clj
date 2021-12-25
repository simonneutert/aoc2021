(ns aoc.puzzle7
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]))

(def demo "16,1,2,0,4,2,7,1,2,14")

(defn read-input-file
  "filename as string path"
  [filename]
  (-> (io/reader filename)
      (line-seq)))

(defn split-string-with-as-int
  "Takes a string and splits by lambda function"
  [input-string splitter-lambda]
  (->>
   (str/split input-string splitter-lambda)
   (map #(Integer/parseInt %))))

(defn min-max-diff
  "simple min max diff (best for positive ints)"
  [x y]
  (- (max x y) (min x y)))

(defn sums-of-diffs
  "takes a list of int"
  [int-list]
  (for [position int-list
        :let [diffs (for [pos int-list]
                      (min-max-diff pos position))]]
    [position (reduce #(+ %1 %2) 0 diffs)]))

(defn least-diff [sums-of-diffs]
  (let [[position fuel] (first (sort-by #(last %) sums-of-diffs))]
    {:position position :fuel fuel}))

;; execution part 1

(def demo-int (map #(Integer/parseInt (str %)) (str/split demo #",")))
(least-diff (sums-of-diffs demo-int))

;; part 2

(defn gaussian-diff [diff]
  (if (<= diff 1) (list diff) (range 1 (+ 1 diff))))

(defn least-diff-extra [sums-of-diffs-extra]
  (let [results (sort-by #(last %) sums-of-diffs-extra)
        [position fuel] (first results)]
    {:position position :fuel fuel}))

(defn sums-of-diffs-extra [input range-max]
  (for [position (range 1 range-max)
        :let [movement (for [pos input]
                         (- (max position pos) (min position pos)))]]
    [position (reduce #(+ %1 (apply + (gaussian-diff %2))) 0 movement)]))

;; execution part2

(def input
  (->
   (read-input-file "resources/puzzle7.txt")
   (first)
   (split-string-with-as-int #",")))

(least-diff-extra (sums-of-diffs-extra input (apply max input)))