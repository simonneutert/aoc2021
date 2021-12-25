(ns aoc.puzzle6
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]))

(defn sort-kv-by-k-desc [m]
  (into (sorted-map-by >) m))

(def demo-input '(3 4 3 1 2))

(defn read-input-file [filename]
  (-> (io/reader filename)
      (line-seq)))

(def data
  (let [input-digits (->
                      (read-input-file "resources/puzzle6.txt")
                      (first)
                      (str/split  #","))]
    (->>
     input-digits
     (map #(Integer/parseInt %)))))

(defn dec-frequency
  "returns a decremented frequency map"
  [freq]
  (reduce (fn [acc [k v]] (assoc acc (dec k) v)) {} freq))

(defn dec-frequency-sorted-desc
  "returns a desc sorted frequency map"
  [freq]
  (->> freq
       dec-frequency
       sort-kv-by-k-desc))

(defn fish-past-shelf-life
  "returns the accumulator, modified by the condition of
   what to do if fish's life score is lower than 0"
  [freqs acc counter]
  (assoc acc
         6 (+ counter (get freqs 6 0))
         8 (+ counter (get freqs 8 0))))

(defn fish-life-by-age
  "treats the fish depending on its age/life/score"
  [freqs acc val counter]
  (case (< val 0)
    true (fish-past-shelf-life freqs acc counter)
    (assoc acc val counter)))

(defn life-cycle-freq
  "calculates the next life cycle frequencies"
  [frequencies]
  (let [freqs (dec-frequency-sorted-desc frequencies)]
    (reduce (fn [acc [val counter]]
              (fish-life-by-age freqs acc val counter)) {} freqs)))

(defn run-simulation-freq
  "runs the simulation takes a turn limit and 
   the init frequency of a fish swarm"
  [turns frequencies]
  (loop [turn 0
         freqs frequencies]
    (case (= turn turns)
      true freqs
      (recur (inc turn) (life-cycle-freq freqs)))))

(time (let [freqs (run-simulation-freq 256 (frequencies data))]
        (prn "Started ...")
        (prn "freqs " freqs)
        (prn "Result is " (apply + (vals freqs)))))
