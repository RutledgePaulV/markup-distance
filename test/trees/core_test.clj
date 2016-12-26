(ns trees.core-test
  (:require [clojure.test :refer :all]
            [trees.core :refer :all]
            [clojure.pprint :as pprint]))


(def a '(:a (:b (:c :d))))
(def b '(:a (:b (:c :d)) (:b (:c :d))))
(def c '(:a (:b (:c :d)) (:e (:f :g))))
(def d '(:a (:b (:c :d :h)) (:b (:c :d :h))))
(def e '(:a (:b (:c :d :h)) (:e (:f :g :h))))
(def f '(:a (:b (:c :d :e))))
(def g '(:a (:m (:b (:c :d :f)))))
(def h '(:a (:m (:g (:h :i :j)))))
(def i '(:a (:b :c)))
(def j '(:e (:f :g)))

(deftest tests
  (testing "cartesian product includes all pairs"
    (is (= '((1 3) (1 4) (2 3) (2 4)) (cartesian [[1 2] [3 4]]))))

  (testing "flattening a tree produces a depth first sequence"
    (is (= [:a :b :c :d :h :b :c :d :h] (flatten-tree d))))

  (testing "the cardinality of a tree is the number of total nodes in the tree"
    (is (= 9 (cardinality d))))

  (testing "subtrees returns the immediate subtrees of the root node of a tree"
    (is (= '((:b (:c :d :h)) (:b (:c :d :h))) (first-level-subtrees d))))

  (testing "the cost to insert a tree is equal to the cost of inserting each of its nodes"
    (is (= 9 (cost-to-insert-tree d))))

  (testing "the cost to delete a tree is equal to the cost of deleting each of its nodes"
    (is (= 9 (cost-to-delete-tree d))))

  (testing "common subtree cardinality"
    (is (= 3 (common-subtree-cardinality (second a) (second d)))))

  (testing "pairs of a tree return as expected"
    (is (= ['(:a 0) '(:b 1) '(:c 2) '(:d 2)] (pairs a))))

  (testing "tree operation costs return as expected"
    (is (= {:delete [1.7142857142857142], :insert [2.2857142857142856 2.2857142857142856]}
           (tree-operation-costs a d))))

  (testing "edit distance between a and d is as expected"
    (is (= 3.2857142857142856 (distance a d)))))