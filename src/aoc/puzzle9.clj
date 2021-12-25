(ns aoc.puzzle9
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as cset])
  (:use [aoc.flatten-one-level :only [flatten-one-level]]))

(defn read-input-file
  "filename as string path"
  [filename]
  (-> (io/reader filename)
      (line-seq)))

(def demo-input "2199943210
3987894921
9856789892
8767896789
9899965678")

(defn char->int
  [c]
  (Integer/parseInt (str c)))

(defn table-key
  [x y]
  (str x "-" y))

;; part 1

(defn is-in-set?
  [s elem]
  (> (count (cset/select #(= % elem) s)) 0))

(defn parse-rows-columns
  [row row-index]
  (map-indexed
   #(list (vector row-index %1) (char->int %2))
   row))

(defn parse-table
  [input]
  (->
   (map-indexed
    (fn [idx row]
      (parse-rows-columns row idx)) input)
   (flatten-one-level)))

(defn extract-coordinates-from-table
  [table]
  (for [[[x y] _] table]
    [x y]))

(defn find-bounds
  [table]
  (let [coordinates (extract-coordinates-from-table table)
        coordinates-rated (for [[x y] coordinates]
                            [(+ x y) x y])
        coordinates-rated-xy (sort-by #(first %) coordinates-rated)
        [_ min-x min-y] (first coordinates-rated-xy)
        [_ max-x max-y] (last coordinates-rated-xy)]

    {:min-max {:min-x min-x
               :max-x max-x
               :min-y min-y
               :max-y max-y}
     :bounds {:left-upper [min-x min-y]
              :left-down [min-x max-y]
              :right-upper [max-x min-y]
              :right-down [max-x max-y]}}))

(defn simple-table->hash-table
  [parsed-table]
  (reduce
   (fn [acc [[x y] value]] (assoc acc (str x "-" y) [x y value]))
   {}
   parsed-table))

(defn local-min-values-of-hash-table
  [hash-table]
  (->>
   (for [k (keys hash-table)
         :let [[x y v] (get hash-table k)
               [top-x top-y top-val] (get hash-table (table-key x (- y 1)) nil)
               [bottom-x bottom-y bottom-val] (get hash-table (table-key x (+ y 1)) nil)
               [left-x left-y left-val] (get hash-table (table-key (- x 1) y) nil)
               [right-x right-y right-val] (get hash-table (table-key (+ x 1) y) nil)
               neighbours (remove nil? [top-val bottom-val left-val right-val])
               self-min (if (< v (apply min neighbours)) v nil)]]
     self-min)
   (remove nil?)))

(defn calculate-score
  [scores]
  (apply + (map #(+ 1 %) scores)))

(defn find-lows-heightmap
  [input]
  (let [parsed-table (parse-table input)
        bounds (find-bounds parsed-table)
        hash-table (simple-table->hash-table parsed-table)]
    (->
     (local-min-values-of-hash-table hash-table)
     (calculate-score))))

;; execution part1

(def demo-input-parsed
  (str/split demo-input #"\n"))

(->
 (find-lows-heightmap demo-input-parsed)
 (prn))

(->
 (find-lows-heightmap (read-input-file "resources/puzzle9.txt"))
 (prn))

;; part2

(defn find-neighbours-in-table-by-key
  [hash-table key-index]
  (let [[x y v] (get hash-table key-index)
        top-key (table-key x (- y 1))
        bottom-key (table-key x (+ y 1))
        left-key (table-key (- x 1) y)
        right-key (table-key (+ x 1) y)
        [top-x top-y top-val] (get hash-table top-key nil)
        [bottom-x bottom-y bottom-val] (get hash-table bottom-key nil)
        [left-x left-y left-val] (get hash-table left-key nil)
        [right-x right-y right-val] (get hash-table right-key nil)
        neighbours (remove #(nil? (last %)) [[top-key top-val] [bottom-key bottom-val] [left-key left-val] [right-key right-val]])]
    neighbours))

;; PART2

(defn- neighbours-matching-specs
  [neighbours found-basin-fields]
  (filter
   #(and
     (< (last %) 9)
     (not (is-in-set? found-basin-fields %)))
   neighbours))

(defn- extract-keys-neighbours-matching-specs
  [neighbours found-basin-fields]
  (map
   #(first %)
   (neighbours-matching-specs neighbours found-basin-fields)))

(defn- find-matching-neighbours
  [table positions-to-check found-basin-fields]
  (flatten (for [position-key positions-to-check
                 :let [neighbours (find-neighbours-in-table-by-key table position-key)]]
             (extract-keys-neighbours-matching-specs neighbours found-basin-fields))))

(defn- find-neighbouring-minimums
  [table local-minimum]
  (loop
   [positions-to-check #{local-minimum}
    found-basin-fields #{local-minimum}]
    (let [matching-neighbours (find-matching-neighbours table positions-to-check found-basin-fields)
          matching-neighbours-cleaned (cset/difference (set matching-neighbours) found-basin-fields)]
      (cond
        (> (count matching-neighbours) 0) (recur
                                           matching-neighbours-cleaned
                                           (apply conj found-basin-fields matching-neighbours))
        :else found-basin-fields))))

(defn find-basins
  [table local-minimum]
  (find-neighbouring-minimums table local-minimum))

(defn find-local-mins-in-hash-table
  [hash-table]
  (->>
   (for [k (keys hash-table)
         :let [[x y v] (get hash-table k)
               [top-x top-y top-val] (get hash-table (table-key x (- y 1)) nil)
               [bottom-x bottom-y bottom-val] (get hash-table (table-key x (+ y 1)) nil)
               [left-x left-y left-val] (get hash-table (table-key (- x 1) y) nil)
               [right-x right-y right-val] (get hash-table (table-key (+ x 1) y) nil)
               neighbours (remove nil? [top-val bottom-val left-val right-val])
               self-min (if (< v (apply min neighbours)) v nil)]]
     (if (nil? self-min) nil [k [x y v]]))
   (remove nil?)
   (reduce #(assoc %1 (first %2) (last (last %2))) {})))

;; execution part2

(defn solve-puzzle
  [basins]
  (->> basins
       (sort-by count #(compare %2 %1))
       (take 3)
       (map #(count %))
       (apply *)))

(defn run-puzzle
  [file-string]
  (let [table (-> (read-input-file file-string)
                  (parse-table)
                  (simple-table->hash-table))
        min-hash-table (find-local-mins-in-hash-table table)]
    (->>
     (for [minimum-position (map #(first %) min-hash-table)]
       (find-basins table minimum-position))
     (solve-puzzle))))

(->
 (run-puzzle "resources/puzzle9.txt")
 (prn))
