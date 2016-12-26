(ns trees.style
  (:require [clojure.math.combinatorics :as combos]
            [clojure.set :as sets])
  (:import (java.util UUID)))


(defn map->set [m] (into #{} m))

(defn intersect [& s]
  (if (empty? s)
    #{}
    (apply sets/intersection s)))

(defn find-intersecting-styles [sets]
  (into {} (apply intersect (map (comp map->set :styles) sets))))

(defn get-unique-combinations [sets]
  (filter not-empty (combos/subsets sets)))

(defn nodes->rules [nodes]
  (sort-by :score >
           (filter
             #(not-empty (:properties %))
             (map
               #(let [nodes (into #{} %) intersection (find-intersecting-styles %)]
                  {:class      (gensym "class-")
                   :nodes      nodes
                   :properties intersection
                   :score      (+ (* 1.5 (count intersection)) (* 2 (count %)))})
               (get-unique-combinations nodes)))))

(defn complement-map [m1 m2]
  (select-keys m1 (sets/difference (set (keys m1)) (set (keys m2)))))

(defn apply-rule [nodes rule]
  (let [candidates (:nodes rule)
        class-name (:class rule)
        properties (:properties rule)]
    (map (fn [node]
           (if (contains? candidates node)
             (as-> node m
                   (assoc m :classes (conj (get m :classes #{}) class-name))
                   (update m :styles #(complement-map % properties)))
             node)) nodes)))

(defn generate-classes [nodes]
  (loop [nodes nodes result []]
    (let [rules (nodes->rules nodes)]
      (if-not (not-empty rules)
        {:nodes (map #(dissoc % :styles) nodes) :rules result}
        (recur (apply-rule nodes (first rules))
               (conj result (select-keys (first rules) [:class :properties])))))))

