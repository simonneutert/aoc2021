(ns aoc.puzzle2
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]))

(def demo [["forward" 5]
           ["down" 5]
           ["forward" 8]
           ["up" 3]
           ["down" 8]
           ["forward" 2]])

(def input-values
  (-> (fn [elem] (str/split elem #" "))
      (map (line-seq (io/reader "resources/puzzle2.txt")))))

;; part1

(defn input-values-parsed
  [list-of-commands]
  (->> list-of-commands
       (map (fn [elem] [(str/trim (first elem)) (Integer/parseInt (second elem))]))))

(defn move-boat
  [commands]
  (loop [position [0 0]
         [[command value] :as cmds] commands]
    (case command
      "forward" (let [[x y] position]
                  (recur [(+ x value) y] (drop 1 cmds)))
      "up" (let [[x y] position]
             (recur [x (- y value)] (drop 1 cmds)))
      "down" (let [[x y] position]
               (recur [x (+ y value)] (drop 1 cmds)))
      {"position" position "result" (apply * position)})))

(time (move-boat (input-values-parsed input-values)))

;; part2

(defn move-boat-with-aim
  [commands]
  (loop [position [0 0]
         aim 0
         [[command value] :as cmds] commands]
    (case command
      "forward" (let [[x y] position
                      new-position-x (+ x value)
                      new-position-y (+ y (* aim value))]
                  (recur [new-position-x new-position-y] aim (drop 1 cmds)))
      "up" (let [[x y] position]
             (recur [x y] (- aim value) (drop 1 cmds)))
      "down" (let [[x y] position]
               (recur [x y] (+ aim value) (drop 1 cmds)))
      {"position" position "result" (apply * position)})))

(time (move-boat-with-aim demo))
(time (move-boat-with-aim (input-values-parsed input-values)))