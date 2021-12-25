(ns aoc.puzzle1
  (:require [clojure.java.io :as io]))

(def demo [199
           200
           208
           210
           200
           207
           240
           269
           260
           263])

(def input-values
  (->>
   (line-seq (io/reader "resources/puzzle1.txt"))
   (map #(Integer/parseInt %))))

;; part1

(defn boolean->int [val]
  (get {true 1 false 0} val))

(defn check-value-2
  [input-vector]
  (loop [acc 0 [v1 v2 & vrest] input-vector]
    (cond
      (nil? v2) acc
      :else (recur (+ acc (boolean->int (< v1 v2))) (cons v2 vrest)))))

(time (check-value-2 demo))
(time (check-value-2 input-values))

;; part2

(defn check-value-3
  [input-vector]
  (loop [acc 0
         [v1 v2 v3 v4 :as v] input-vector]
    (cond
      (nil? v4) acc
      :else (let [sum1 (+ v1 v2 v3)
                  sum2 (+ v2 v3 v4)
                  result (boolean->int (< sum1 sum2))]
              (recur (+ acc result) (drop 1 v))))))

(time (check-value-3 demo))
(time (check-value-3 input-values))