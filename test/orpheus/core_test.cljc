(ns orpheus.core-test
  (:require #?(:cljs [cljs.test :refer-macros [deftest is testing]])
            #?(:clj [clojure.test :refer [deftest is testing]])
            [orpheus.core :as core]))

(deftest h-test
  (is (= (core/h "div" {"a" 42} "b")
         (core/h "div" {"a" 42} "b")))
  (is (= (core/h "div" {"a" 42
                        "childNodes" ["b"]})
         (core/h "div" {"a" 42} "b"))))

(deftest element-type-test
  (is (= (core/element-type "div")
         (core/element-type "DIV")))
  (let [t (core/element-type "div")]
    (is (= t (core/ve-type (core/h t {}))))))
