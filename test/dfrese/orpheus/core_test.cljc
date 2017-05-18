(ns dfrese.orpheus.core-test
  (:require #?@(:cljs [[cljs.test :refer-macros [deftest is testing]]
                       [dfrese.orpheus.core :as core :include-macros true]])
            #?@(:clj [[clojure.test :refer [deftest is testing]]
                      [dfrese.orpheus.core :as core]])
            [dfrese.orpheus.types :as types]
            [dfrese.orpheus.types.element :as element]
            [dfrese.orpheus.types.indirection :as indirection]))

(deftest h-test
  (is (= (core/h "div" {"a" 42} "b")
         (core/h "div" {"a" 42} "b")))
  (is (= (core/h "div" {"a" 42
                        "childNodes" ["b"]})
         (core/h "div" {"a" 42} "b")))
  (is (= (core/h "div" "test" "test" "test")
         (core/h "div" {"childNodes" (repeat 3 "test")})))
  (is (= 3 (count (core/get-property (core/h "div" {"childNodes" (repeat 3 "test")})
                                     "childNodes"))))
  (is (= 3 (count (core/get-property (apply core/h "div" (repeat 3 "test")) "childNodes")))))

(deftest element-type-test
  (is (= (element/element-type "http://www.w3.org/1999/xhtml" "div")
         (element/element-type "http://www.w3.org/1999/xhtml" "DIV")))
  (let [t (element/element-type "http://www.w3.org/1999/xhtml" "div")]
    (is (= t (types/ve-type (element/h t {}))))))

(defn expand-fnc [c]
  (indirection/expand-indirection (types/ve-type c) (types/ve-props c)))

(deftest function-type-test

  (core/defnc testf "My doc" [a b]
    (core/h "div" a b))

  (core/defnc testf2 [& args]
    (apply core/h "div" args))

  (is (= "My doc" (:doc (meta testf))))
  
  (is (= (core/h "div" "Hello" (core/h "span"))
         (expand-fnc (testf "Hello" (core/h "span")))))

  (is (= (testf "Hello" (core/h "span"))
         (testf "Hello" (core/h "span"))))

  (is (= (core/h "div" "Hello" "World")
         (expand-fnc (testf2 "Hello" "World")))))

(deftest translate-test
  (let [t (core/translate :element inc)]
    (is (= (core/translate :element inc)
           t))
    (is (types/with-context-update? t))
    (is (some? (types/with-context-update-f t)))
    (let [res (atom 0)]
      ((:dispatch! ((types/with-context-update-f t) {:dispatch! #(reset! res %)}))
       42)
      (is (= 43 @res)))))
