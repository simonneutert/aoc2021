(ns aoc.puzzle3
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]))

(def demo '("00100"
            "11110"
            "10110"
            "10111"
            "10101"
            "01111"
            "00111"
            "11100"
            "10000"
            "11001"
            "00010"
            "01010"))

(defn list-of-ints->int [list-of-bits]
  (read-string (str "2r" (str/join "" list-of-bits))))

(def input-values
  (-> (fn [elem] (str/split elem #" "))
      (map (line-seq (io/reader "resources/puzzle3.txt")))
      flatten))

;; Part 1

(defn transpose-matrix [matrix]
  (for [i (range (count (first matrix)))]
    (for [entry matrix]
      (Integer/parseInt (str (nth entry i))))))

(defn gamma-rate [matrix]
  (map #(cond
          (> (reduce + %) (quot (count %) 2)) 1
          :else 0) matrix))

(defn epsilon-rate "reverts the bits of the gamma rate"
  [bit-vector]
  (map #(case %
          1 0
          0 1) bit-vector))

(defn power-consumption-ship [input-values]
  (let [transposed-matrix (transpose-matrix input-values)
        gamma (gamma-rate transposed-matrix)]
    (*
     (list-of-ints->int gamma)
     (list-of-ints->int (epsilon-rate gamma)))))

(time (power-consumption-ship demo))
(time (power-consumption-ship input-values))

;; Part 2


(defn first-bit [string]
  (Integer/parseInt (str (first string))))

(defn nth-bit [string position]
  (Integer/parseInt (str (nth string position))))

(defn filter-by-bit-position [input-values bit bitpos]
  (filter #(= bit (nth-bit % bitpos)) input-values))

(defn prominent-bit-at [matrix position]
  (let [reduced-count (reduce + (nth matrix position))
        counter (count (nth matrix position))
        truthy reduced-count
        falsey (- counter reduced-count)]
    (cond
      (> truthy falsey) 1
      (> falsey truthy) 0
      (= falsey truthy) 1
      :else 0)))

(defn find-oxygen-reading [input-values]
  (loop [input input-values
         position 0]
    (let [matrix (transpose-matrix input)
          prominent-bit (prominent-bit-at matrix position)
          hits (filter-by-bit-position input prominent-bit position)]
      (cond
        (> (count hits) 1) (recur hits (inc position))
        :else hits))))

(defn co2prominent-bit-at [matrix position]
  (let [reduced-count (reduce + (nth matrix position))
        counter (count (nth matrix position))
        truthy reduced-count
        falsey (- counter reduced-count)]
    (cond
      (> truthy falsey) 0
      (> falsey truthy) 1
      (= falsey truthy) 0
      :else 1)))

(defn find-co2-reading [input-values]
  (loop [input input-values
         position 0]
    (let [matrix (transpose-matrix input)
          prominent-bit (co2prominent-bit-at matrix position)
          hits (filter-by-bit-position input prominent-bit position)]
      (cond
        (> (count hits) 1) (recur hits (inc position))
        :else hits))))

(defn life-support-rating [input-values]
  (*
   (list-of-ints->int (find-oxygen-reading input-values))
   (list-of-ints->int (find-co2-reading input-values))))

(time (life-support-rating demo))
(time (life-support-rating input-values))

