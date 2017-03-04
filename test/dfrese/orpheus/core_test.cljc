(ns dfrese.orpheus.core-test
  (:require #?@(:cljs [[cljs.test :refer-macros [deftest is testing]]
                       [dfrese.orpheus.core :as core :include-macros true]])
            #?@(:clj [[clojure.test :refer [deftest is testing]]
                      [dfrese.orpheus.core :as core]])))

(deftest h-test
  (is (= (core/h "div" {"a" 42} "b")
         (core/h "div" {"a" 42} "b")))
  (is (= (core/h "div" {"a" 42
                        "childNodes" ["b"]})
         (core/h "div" {"a" 42} "b")))
  (is (= (core/h "div" "test" "test" "test")
         (core/h "div" {"childNodes" (repeat 3 "test")})))
  (is (vector? (get (core/ve-props (core/h "div" {"childNodes" (repeat 3 "test")}))
                    "childNodes")))
  (is (vector? (get (core/ve-props (apply core/h "div" (repeat 3 "test")))
                    "childNodes"))))

(deftest element-type-test
  (is (= (core/element-type "http://www.w3.org/1999/xhtml" "div")
         (core/element-type "http://www.w3.org/1999/xhtml" "DIV")))
  (let [t (core/element-type "http://www.w3.org/1999/xhtml" "div")]
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

(deftest translate-test
  (let [t (core/translate :element inc)]
    (is (= (core/translate :element inc)
           t))
    (is (core/with-context-update? t))
    (is (some? (:update-options t)))
    (let [res (atom 0)]
      ((:dispatch! ((:update-options t) {:dispatch! #(reset! res %)}))
       42)
      (is (= 43 @res)))))
