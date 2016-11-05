(ns orpheus.transformer-test
  (:require #?(:cljs [cljs.test :refer-macros [deftest is testing]])
            #?(:clj [clojure.test :refer [deftest is testing]])
            [orpheus.transformer :as t]))

(deftest transformers-test
  (testing "they chain up"
    (let [f (t/trans-> (t/tag :a)
                       (t/trans-> (t/tag :b)
                                  (t/tag :c))
                       (t/tag :d))
          res (atom nil)]
      (is (= (t/tagged :d (t/tagged :c (t/tagged :b (t/tagged :a 42))))
             (f 42)))))
  (testing "they are referentially transparent"
    (let [f1 (t/tag :a)
          f2 (t/tag :b)
          h (t/trans-> f2 f1)]
      (is (= (t/trans-> f2 f1)
             h)))))
