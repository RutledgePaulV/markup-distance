(ns trees.core
  (:require [clojure.walk :as walk]
            [clojure.set :as sets]))


(defn cartesian [colls]
  (if (empty? colls)
    '(())
    (for [x (first colls) more (cartesian (rest colls))]
      (cons x more))))

(defn maptree [f tree]
  (let [result (atom [])]
    (walk/postwalk
      #(when-not (seq? %)
         (swap! result conj (f %))) tree)
    @result))

(defn flatten-tree [t]
  (maptree identity t))

(defn cardinality [t]
  (count (flatten-tree t)))

(defn first-level-subtrees [tree]
  (filterv some? (take-while (every-pred seq? #(some? (first (filter seq? %)))) (drop 1 tree))))

(defn foreach [f coll]
  (dorun (map f coll)))

(defn pairs [tree]
  (letfn [(one-node [depth node]
            (if (seq? node)
              (map (partial one-node (inc depth)) node)
              (vector node depth)))]
    (into [] (partition 2 (flatten (map (partial one-node 0) tree))))))

(defn cost-to-delete [node] 1)

(defn cost-to-insert [node] 1)

(defn cost-to-delete-tree [t]
  (reduce + 0 (maptree cost-to-delete t)))

(defn cost-to-insert-tree [t]
  (reduce + 0 (maptree cost-to-insert t)))

(defn nodes-are-equivalent? [node1 node2]
  (= node1 node2))

(defn common-subtree-cardinality [t1 t2]
  (letfn
    [(nodes-equal? [node1 node2]
       (and (= (second node1) (second node2))
            (nodes-are-equivalent? (first node1) (first node2))))

     (compute-insert-edge [dist t2-pairs m]
       (aset dist 0 m (int (+ (aget dist 0 (dec m)) (cost-to-insert (get t2-pairs m))))))

     (compute-delete-edge [dist t1-pairs n]
       (aset dist n 0 (int (+ (aget dist (dec n) 0) (cost-to-delete (get t1-pairs n))))))

     (compute-internals [dist t1-pairs t2-pairs n m]
       (let [t1-node (get t1-pairs (dec n))
             t2-node (get t2-pairs (dec m))
             del-cost (+ (aget dist (dec n) m) (cost-to-delete (get t1-pairs (dec n))))
             ins-cost (+ (aget dist n (dec m)) (cost-to-insert (get t2-pairs (dec m))))
             inj-cost (when (nodes-equal? t1-node t2-node) (aget dist (dec n) (dec m)))]
         (aset dist n m (int (apply min (filter some? [del-cost ins-cost inj-cost]))))))]

    (let [t1-pairs (pairs t1)
          t2-pairs (pairs t2)
          t1-cardinality (count t1-pairs)
          t2-cardinality (count t2-pairs)
          t1-ceiling (inc t1-cardinality)
          t2-ceiling (inc t2-cardinality)
          distances (make-array Integer/TYPE t1-ceiling t2-ceiling)
          coords (cartesian [(range 1 t1-ceiling) (range 1 t2-ceiling)])]
      (aset distances 0 0 (int 0))
      (foreach #(compute-delete-edge distances t1-pairs %) (range 1 t1-ceiling))
      (foreach #(compute-insert-edge distances t2-pairs %) (range 1 t2-ceiling))
      (foreach #(compute-internals distances t1-pairs t2-pairs (first %) (second %)) coords)
      (/ (- (+ t1-cardinality t2-cardinality) (aget distances t1-cardinality t2-cardinality)) 2))))


(defn reciprocal [subtree1 subtree2]
  (let [s1-cardinality (cardinality subtree1)
        s2-cardinality (cardinality subtree2)
        common-cardinality (common-subtree-cardinality subtree1 subtree2)]
    (/ 1.0 (+ 1.0 (/ common-cardinality (max s1-cardinality s2-cardinality))))))


(defn for-indexed [items f]
  (dorun
    (map
      (fn [[idx el]]
        (f idx el))
      (zipmap (range 0 (count items)) items))))

(defn forloop [start end callback]
  (dorun (map #(callback %) (range start (inc end)))))

(defn symmetric-difference [s1 s2]
  (sets/union (sets/difference s1 s2) (sets/difference s2 s1)))

(defn tree-operation-costs [t1 t2]
  (let [t1-subs (first-level-subtrees t1)
        t2-subs (first-level-subtrees t2)
        results {:delete (make-array Double/TYPE (count t1-subs))
                 :insert (make-array Double/TYPE (count t2-subs))}]

    (letfn [(get-delete [idx] (aget (:delete results) idx))
            (put-delete [idx element] (aset (:delete results) idx (double element)))
            (get-insert [idx] (aget (:insert results) idx))
            (put-insert [idx element] (aset (:insert results) idx (double element)))]

      (for-indexed
        t1-subs
        (fn [t1-index t1-sub]

          (put-delete t1-index (cost-to-delete-tree t1-sub))

          (for-indexed
            t2-subs
            (fn [t2-index t2-sub]
              (put-insert t2-index (cost-to-insert-tree t2-sub))

              (put-delete t1-index
                          (min (get-delete t1-index)
                               (* (cost-to-delete-tree t1-sub)
                                  (reciprocal t1-sub t2-sub))))

              (put-insert t2-index
                          (min (get-insert t2-index)
                               (* (cost-to-insert-tree t2-sub)
                                  (reciprocal t1-sub t2-sub))))))))


      (for-indexed
        t1-subs
        (fn [t1-index t1-sub]
          (put-delete
            t1-index
            (min (get-delete t1-index)
                 (* (cost-to-delete-tree t1-sub)
                    (reciprocal t1-sub t2))))))

      (for-indexed
        t2-subs
        (fn [t2-index t2-sub]
          (put-insert
            t2-index
            (min (get-insert t2-index)
                 (* (cost-to-insert-tree t2-sub)
                    (reciprocal t1 t2-sub)))))))

    {:delete (into [] (:delete results))
     :insert (into [] (:insert results))}))



(defn distance [t1 t2]
  (let [t1-subs (first-level-subtrees t1)
        t2-subs (first-level-subtrees t2)
        M (count t1-subs)
        N (count t2-subs)
        dists (make-array Double/TYPE (inc M) (inc N))
        costs (tree-operation-costs t1 t2)]

    (letfn [(gets [i j] (aget dists i j))
            (sets [i j e] (aset dists i j (double e)))]

      (if (= M N 0)
        (aset dists 0 0 (count
                          (symmetric-difference
                            (set (flatten-tree t1))
                            (set (flatten-tree t2)))))
        (aset dists 0 0 0.0))

      (forloop 1 M (fn [i] (sets i 0 (+ (gets (dec i) 0) (nth (:delete costs) (dec i))))))

      (forloop 1 N (fn [j] (sets 0 j (+ (gets 0 (dec j)) (nth (:insert costs) (dec j))))))

      (forloop
        1 M
        (fn [i]
          (forloop
            1 N
            (fn [j]
              (sets i j (min
                          (+ (gets (dec i) (dec j))
                             (distance (nth t1-subs (dec i))
                                       (nth t2-subs (dec j))))

                          (+ (gets (dec i) j)
                             (nth (:delete costs) (dec i)))

                          (+ (gets i (dec j))
                             (nth (:insert costs) (dec j)))))))))

      (gets M N))))


(defn similarity [t1 t2]
  (let [distance (distance t1 t2)]
    (/ 1.0 (+ 1.0 distance))))