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

(defn input-values-parsed
  [list-of-commands]
  (->> list-of-commands
       (map (fn [elem] [(str/trim (first elem)) (Integer/parseInt (second elem))]))))

;; part1

(defn position-from-commands
  [start-x start-y commands]
  (loop [position [start-x start-y]
         [[command value] :as cmds] commands]
    (let [[x y] position]
      (case command
        "forward" (recur [(+ x value) y] (drop 1 cmds))
        "up" (recur [x (- y value)] (drop 1 cmds))
        "down" (recur [x (+ y value)] (drop 1 cmds))
        {:position position :result (apply * position)}))))

;; execution part1

(time
 (prn
  (->>
   input-values
   (input-values-parsed)
   (position-from-commands 0 0))))

;; part2

(defn position-from-commands-with-aim
  [start-x start-y aim-init commands]
  (loop [position [start-x start-y]
         aim aim-init
         [[command value] :as cmds] commands]
    (let [[x y] position]
      (case command
        "forward" (let [new-position-x (+ x value)
                        new-position-y (+ y (* aim value))]
                    (recur [new-position-x new-position-y] aim (drop 1 cmds)))
        "up" (recur [x y] (- aim value) (drop 1 cmds))
        "down" (recur [x y] (+ aim value) (drop 1 cmds))
        {:position position :aim aim :result (apply * position)}))))

;; execution part2

(time
 (position-from-commands-with-aim 0 0 0 demo))

(time
 (->>
  input-values
  (input-values-parsed)
  (position-from-commands-with-aim 0 0 0)))