(ns orpheus.patch.util-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [orpheus.patch.util :as util]))

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

(deftest diff-patch-test
  (testing "it works as expected"
    (let [mk (fn [v] (if (vector? v)
                       (str (name (first v))
                            (second v))
                       (name v)))]
      (is (= ["a" "b" "c" "d"]
             (util/diff-patch ["c0" "x"]
                              [[:c 0] :x]
                              [:a :b :c :d]
                              #(not= %1 :x)
                              =
                              (fn append [v k]
                                ;;(println "append" v k)
                                (vec (concat v [(mk k)])))
                              (fn insert
                                [v k ref]
                                ;;(println "insert" v k ref)
                                (vec (concat (take-while #(not= % (mk ref)) v)
                                             (cons (mk k)
                                                   (drop-while #(not= % (mk ref)) v)))))
                              (fn update [v o n]
                                ;;(println "update" v o n)
                                (assert (= o [:c 0]))
                                (vec (map (fn [k]
                                            (if (= "c0" k) (mk n) (mk k)))
                                          v)))
                              (fn remove [v k]
                                ;;(println "remove" v k)
                                (assert (= k :x))
                                (vec (filter #(not= % (mk k)) v)))))))))

(deftest diff-patch-efficiency-test
  (let [actions (fn [olds news]
                  (util/diff-patch [] olds news
                                   (fn similar? [a1 a2]
                                     (= (first a1) (first a2)))
                                   =
                                   (fn append [res x]
                                     (conj res [:append x]))
                                   (fn insert [res x y]
                                     (conj res [:insert x y]))
                                   (fn update [res x y]
                                     (conj res [:update x y]))
                                   (fn remove [res x]
                                     (conj res [:remove x]))))]
    (testing "no change"
      (is (= []
             (actions [[:a] [:b] [:c]] [[:a] [:b] [:c]]))))
    (testing "identical and similar elements"
      (let [d1 [:div 1]
            d2 [:div 2]]
        (is (= [[:append d2]]
               (actions [d1] [d1 d2])))
        (is (= [[:insert d2 d1]]
               (actions [d1] [d2 d1])))
        (is (= [[:remove d2]]
               (actions [d1 d2] [d1])))
        (is (= [[:update d1 d2]]
               (actions [d1] [d2])))))
    ;; ---
    (testing "insert first"
      (is (= [[:insert [:c] [:a]]]
             (actions [[:a] [:b]], [[:c] [:a] [:b]]))))
    (testing "insert last"
      (is (= [[:append [:c]]]
             (actions [[:a] [:b]], [[:a] [:b] [:c]]))))
    (testing "remove first"
      (is (= [[:remove [:a]]]
             (actions [[:a] [:b] [:c]], [[:b] [:c]]))))
    (testing "remove last"
      (is (= [[:remove [:c]]]
             (actions [[:a] [:b] [:c]], [[:a] [:b]]))))
    ;; ---
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
    ))
