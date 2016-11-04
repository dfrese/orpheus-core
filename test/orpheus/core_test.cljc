(ns orpheus.core-test
  (:require #?(:cljs [cljs.test :refer-macros [deftest is testing]])
            #?(:clj [clojure.test :refer [deftest is testing]])
            [orpheus.core :as core]))

;; TODO: velement
;; TODO: memoize

(deftest transformers-test
  (testing "they chain up"
    (let [f (core/trans-> (core/tag :b)
                          (core/tag :a))
          res (atom nil)]
      (is (= [:a [:b 42]] (f 42)))))
  (testing "they are referentially transparent"
    (let [f1 (core/tag :a)
          f2 (core/tag :b)
          h (core/trans-> f2 f1)]
      (is (= (core/trans-> f2 f1)
             h)))))
