(ns aoc.flatten-one-level)

(defn flatten-one-level
  "flattens a collection one level deep"
  [coll]
  (mapcat #(if (sequential? %) % [%]) coll))
