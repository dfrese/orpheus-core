(ns dfrese.orpheus.patch.util-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [dfrese.orpheus.patch.util :as util]))

(deftest patch-map-test
  (testing "it works as expected"
    (is (= {"a" 1 "b" 2 "c" 3}
           (util/patch-map {"a" 1 "b" 1 "d" 4}
                           {:a 1 :b 1 :d 4}
                           {:a 1 :b 1 :c 3}
                           (fn [m k]
                             (dissoc m (name k)))
                           (fn [m k v]
                             (assoc m (name k) v))
                           (fn [m k o n]
                             (assoc m (name k)
                                    (if (= :b k)
                                      (+ o n)
                                      n))))))))

(defrecord Node [src])

(deftest fold-diff-patch-functionality-test
  ;; Note: patchability does not matter for functionality
  (let [node #(Node. %) ;; Need identity, and 'simulates' difference between the lists of vnodes and resulting list of nodes.
        node-name :src
        patch* (fn [olds news patchable?]
                 (let [nodes (map node olds)]
                   (util/fold-diff-patch nodes
                                         (fn append [res new] ;;(println "append" (map node-name res) new)
                                           (concat res [(node new)]))
                                         (fn remove [res old] ;;(println "remove" (map node-name res) (node-name old))
                                           (filter #(not (identical? %1 old))
                                                   res))
                                         (fn cut [res olds]
                                           ;; remove all at end...
                                           (reduce (fn [res old]
                                                     (filter #(not (identical? %1 old))
                                                             res))
                                                   res olds))
                                         (fn patch [res old new] ;;(println "patch" (map node-name res) (node-name old) new)
                                           (map #(if (identical? old %1) (node new) %1)
                                                res))
                                         nodes news
                                         patchable?)))
        patch-all (fn [olds news]
                    (map node-name (patch* olds news (constantly true))))
        patch-none (fn [olds news]
                     (map node-name (patch* olds news (constantly false))))

        abc [:a :b :c]]

    ;; unchanged
    (is (= abc (patch-all [:a :b :c] [:a :b :c])))
    (is (= abc (patch-none [:a :b :c] [:a :b :c])))

    ;; add middle
    (is (= abc (patch-all [:a :c] [:a :b :c])))
    (is (= abc (patch-none [:a :c] [:a :b :c])))

    ;; remove middle
    (is (= abc (patch-all [:a :b :d :c] [:a :b :c])))
    (is (= abc (patch-none [:a :b :d :c] [:a :b :c])))

    ;; add front
    (is (= abc (patch-all [:b :c] [:a :b :c])))
    (is (= abc (patch-none [:b :c] [:a :b :c])))

    ;; add back
    (is (= abc (patch-all [:a :b] [:a :b :c])))
    (is (= abc (patch-none [:a :b] [:a :b :c])))

    ;; remove front
    (is (= abc (patch-all [:d :a :b :c] [:a :b :c])))
    (is (= abc (patch-none [:d :a :b :c] [:a :b :c])))
    
    ;; remove back
    (is (= abc (patch-all [:a :b :c :d] [:a :b :c])))
    (is (= abc (patch-none [:a :b :c :d] [:a :b :c])))
    )
  )

(deftest fold-diff-patch-efficiency-test
  (let [node #(Node. %) ;; 'simulates' difference between the lists of vnodes and resulting list of nodes.
        node-name :name
        patch* (fn [olds news patchable?]
                 (let [nodes olds]
                   (util/fold-diff-patch []
                                         (fn append [res new]
                                           (conj res [:append new]))
                                         (fn remove [res old]
                                           (conj res [:remove old]))
                                         (fn cut [res olds]
                                           ;; remove all at end... ignore this here..
                                           (reduce #(conj %1 [:remove %2])
                                                   res olds))
                                         (fn patch [res old new]
                                           (conj res [:patch old new]))
                                         nodes news
                                         patchable?)))
        patch-all (fn [olds news]
                    (patch* olds news (constantly true)))
        patch-none (fn [olds news]
                     (patch* olds news (constantly false)))

        na (node :a)
        nb (node :b)
        nc (node :c)

        counts (fn [res]
                 (into {} (map (fn [[k v]]
                                 [k (count v)])
                               (group-by first res))))
        ]

    ;; unchanged
    (is (= #{[:patch na :a] [:patch nb :b] [:patch nc :c]}
           (set (patch-all [na nb nc] [:a :b :c]))))
    (is (= #{[:remove na] [:remove nb] [:remove nc] [:append :a] [:append :b] [:append :c]}
           (set (patch-none [na nb nc] [:a :b :c]))))
    
    ;; add middle
    (is (= {:patch 2 :append 1}
           (counts (patch-all [na nc] [:a :b :c]))))
    (is (= {:remove 2 :append 3}
           (counts (patch-none [:a :c] [:a :b :c]))))
    
    ;; remove middle
    (is (= {:patch 3 :remove 1} (counts (patch-all [:a :b :d :c] [:a :b :c]))))
    (is (= {:remove 4 :append 3} (counts (patch-none [:a :b :d :c] [:a :b :c]))))

    ;; add front
    (is (= {:patch 2 :append 1} (counts (patch-all [:b :c] [:a :b :c]))))
    (is (= {:remove 2 :append 3} (counts (patch-none [:b :c] [:a :b :c]))))
    
    ;; add back
    (is (= {:patch 2 :append 1} (counts (patch-all [:a :b] [:a :b :c]))))
    (is (= {:remove 2 :append 3} (counts (patch-none [:a :b] [:a :b :c]))))

    ;; remove front
    (is (= {:patch 3 :remove 1} (counts (patch-all [:d :a :b :c] [:a :b :c]))))
    (is (= {:remove 4 :append 3} (counts (patch-none [:d :a :b :c] [:a :b :c]))))
    
    ;; remove back
    (is (= {:patch 3 :remove 1} (counts (patch-all [:a :b :c :d] [:a :b :c]))))
    (is (= {:remove 4 :append 3} (counts (patch-none [:a :b :c :d] [:a :b :c])))))

  ;; TODO
  (comment
    (testing "move from start to end"
      (is (= [[:remove [:a]] [:append [:a]]]
             (actions [[:a] [:b] [:c]], [[:b] [:c] [:a]]))))
    (testing "move from end to start"
      (is (= [[:insert [:c] [:a]] [:remove [:c]]] ;; ?? probably ok, because of identity of nodes? (TODO: doesn't insert do a move at the same time?)
             (actions [[:a] [:b] [:c]], [[:c] [:a] [:b]]))))
    (testing "reverse"
      (is (= [[:insert [:c] [:a]] [:remove [:a]] [:remove [:c]] [:append [:a]]] ;; ?? probably ok, because of identity of nodes?
             ;;[[:remove [:a]] [:update [:b] [:b]] [:remove [:c]] [:append [:a]]]
             (actions [[:a] [:b] [:c]], [[:c] [:b] [:a]]))))
    ;; sort, filter, activate?
)
  )

(deftest fold-diff-patch-keyed-test
  (let [node (fn [src]
               (assert (not (instance? Node src)) (pr-str src))
               (Node. src))
        vtype :type
        vkey :key
        node-src :src
        v (fn [type props & [key]]
            {:type type :props props :key key})

        patch (fn [olds news]
                (util/fold-diff-patch-keyed []
                                            (fn append [res node]
                                              (conj res [:append node]))
                                            (fn remove [res node]
                                              (conj res [:remove node]))
                                            (fn patch [res old new]
                                              (conj res [:patch old new]))
                                            olds
                                            news
                                            (fn patchable? [old new]
                                              (= (vtype (node-src old))
                                                 (vtype new)))
                                            (fn old-key [old]
                                              (vkey (node-src old)))
                                            (fn new-key [new]
                                              (vkey new))
                                            (fn create [v]
                                              (assert (not (instance? Node v)) (pr-str v))
                                              (node v))
                                            (fn destroy! [node]
                                              (assert (instance? Node v) (pr-str v))
                                              nil)
                                            (fn resurrect [node]
                                              node)))]
    (let [v1 (v :t1 "a" :k1)
          nv1 (node v1)
          v2 (v :t1 "b" :k1)

          w1 (v :t1 "a" :w)
          w2 (v :t1 "b" :w)
          nw1 (node w1)
          y1 (v :t1 "a" :y)
          y2 (v :t1 "b" :y)
          ny1 (node y1)

          x1 (v :t1 "x")]
      ;; patch keyed in place
      (is (= [[:patch nv1 v2]]
             (patch [nv1] [v2])))
      ;; patch keyed in front
      (is (= [[:patch nv1 v2] [:append (node x1)]]
             (patch [nv1] [v2 x1])))
      ;; patch keyed at back
      (is (= [[:remove nv1] [:append (node x1)] [:append nv1] [:patch nv1 v2]]
             (patch [nv1] [x1 v2])))
      (is (identical? nv1 (second (nth (patch [nv1] [x1 v2]) 2)))) ;; not only, but same node.
      ;; patch keyed two, flipped
      (is (or (= [[:remove nw1] [:patch ny1 y2] [:append nw1] [:patch nw1 w2]]
                 (patch [nw1 ny1] [y2 w2]))
              (= [[:remove ny1] [:patch nw1 w2] [:append ny1] [:patch ny1 y2]]
                 (patch [nw1 ny1] [y2 w2]))))
      ;; more?
      )))

      #_(try 
          (catch :default e
            (println (.-stack e))))
