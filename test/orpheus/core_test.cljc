(ns orpheus.core-test
  (:require #?@(:cljs [[cljs.test :refer-macros [deftest is testing]]
                       [orpheus.core :as core :include-macros true]])
            #?@(:clj [[clojure.test :refer [deftest is testing]]
                      [orpheus.core :as core]])))

(deftest h-test
  (is (= (core/h "div" {"a" 42} "b")
         (core/h "div" {"a" 42} "b")))
  (is (= (core/h "div" {"a" 42
                        "childNodes" ["b"]})
         (core/h "div" {"a" 42} "b")))
  (is (= (core/h "div" "test" "test" "test")
         (core/h "div" {"childNodes" (repeat 3 "test")}))))

(deftest element-type-test
  (is (= (core/element-type "div")
         (core/element-type "DIV")))
  (let [t (core/element-type "div")]
    (is (= t (core/ve-type (core/h t {}))))))

(deftest function-type-test

  (core/defnc testf "My doc" [a b]
    (core/h "div" a b))

  (core/defnc testf2 [& args]
    (apply core/h "div" args))

  (is (= "My doc" (:doc (meta testf))))
  
  (is (= (core/h "div" "Hello" (core/h "span"))
         (core/expand-fnc (testf "Hello" (core/h "span")))))

  (is (= (testf "Hello" (core/h "span"))
         (testf "Hello" (core/h "span"))))

  (is (= (core/h "div" "Hello" "World")
         (core/expand-fnc (testf2 "Hello" "World")))))
