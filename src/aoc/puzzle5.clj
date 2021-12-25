(ns aoc.puzzle5
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io])
  (:use
   [aoc.flatten-one-level :only [flatten-one-level]]))

(defn diagonal [x y m n]
  (let [distance-x (- (max x m) (min x m))
        distance-y (- (max y n) (min y n))
        distance-tan (int (Math/ceil (* (Math/tan (Math/toRadians 45)) distance-y)))]
    (= distance-x distance-tan)))

(def demo "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2")

(def demo-input (str/split-lines demo))

(def input
  (line-seq (io/reader "resources/puzzle5.txt")))

(defn parse-input [input]
  (for [item input]
    (str/split item #" -> ")))

(defn range-join-first [fixed start end]
  (map #(conj [] fixed %) (range (min start end) (inc (max start end)))))

(defn range-join-last [fixed start end]
  (map #(conj [] % fixed) (range (min start end) (inc (max start end)))))

(defn range-up [distance start end]
  (for [i (range (inc distance))]
    [(+ i start) (+ i end)]))

(defn range-down [distance start end]
  (for [i (range (inc distance))]
    [(- start i) (+ i end)]))

(defn range-down-right [distance start end]
  (for [i (range (inc distance))]
    [(+ i start) (- end i)]))

(defn range-up-left [distance start end]
  (for [i (range (inc distance))]
    [(- start i) (- end i)]))

(defn range-join-diagonal
  [x1 y1 x2 y2]
  (cond
    (and (< x1 x2) (< y1 y2)) (range-up (- x2 x1) x1 y1)
    (and (> x1 x2) (< y1 y2)) (range-down (- x1 x2) x1 y1)
    (and (< x1 x2) (> y1 y2)) (range-down-right (- x2 x1) x1 y1)
    (and (> x1 x2) (> y1 y2)) (range-up-left (- x1 x2) x1 y1)))

(diagonal 0 0 6 6)
(range-join-diagonal 0 0 6 6)

(diagonal 6 0 0 6)
(range-join-diagonal 6 0 0 6)

(diagonal 0 12 6 6)
(range-join-diagonal 0 12 6 6)

(diagonal 0 6 6 12)
(range-join-diagonal 0 6 6 12)

(defn make-sense-of-input [parsed-input]
  (for [[from to] parsed-input
        :let [start (Integer/parseInt (first (str/split from #",")))
              end (Integer/parseInt (first (str/split to #",")))
              decimalstart (Integer/parseInt (last (vec (str/split from #","))))
              decimalend (Integer/parseInt (last (vec (str/split to #","))))]]
    (set (cond
           (= start end) (range-join-first start decimalstart decimalend)
           (= decimalstart decimalend) (range-join-last decimalstart start end)
           (diagonal start decimalstart end decimalend) (range-join-diagonal start decimalstart end decimalend)
           :else []))))

(defn lines-freq-least-2 [freqs]
  (apply + (for [[k v] freqs] (if (> k 1) v 0))))

(defn evaluate-input [input]
  (let [freqs (->>
               input
               (parse-input)
               (make-sense-of-input)
               (map vec)
               (flatten-one-level)
               (frequencies)
               (map #(last %))
               frequencies)
        freqs-least-2 (lines-freq-least-2 freqs)]
    (prn freqs)
    (prn freqs-least-2)))

(evaluate-input demo-input)
(evaluate-input input)