(ns calliope-vdom.impl.util-spec
  (:require-macros [speclj.core :refer [describe it should= should should-not should-be-nil]])
  (:require [speclj.core]
            [calliope-vdom.impl.util :as util]
            [calliope-vdom.dom :as dom]))

(describe "patch-map"
          (it "works as expected"
              (should= {"a" 1 "b" 2 "c" 3}
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
                                                  n)))))))

(describe "diff-patch"
          (it "works as expected"
              (should= ["a" "b" "c" "d"]
                       (util/diff-patch ["c0" "x"]
                                        [[:c 0] :x]
                                        [:a :b :c :d]
                                        #(not= %1 :x)
                                        (fn insert
                                          ([v k]
                                           (vec (concat v [(name k)])))
                                          ([v k ref]
                                           (vec (concat (take-while #(not= % ref) v)
                                                        [(name k)]
                                                        (drop-while #(not= % ref) v)))))
                                        (fn update [v o n]
                                          (assert (= o [:c 0]))
                                          (vec (map (fn [k]
                                                      (if (= "c0" k) (name n) (name k)))
                                                    v)))
                                        (fn remove [v k]
                                          (assert (= k :x))
                                          (vec (filter #(not= % (name k)) v)))))))
