(ns dfrese.orpheus.html-test
  (:require #?(:cljs [cljs.test :refer-macros [deftest is testing]])
            #?(:clj [clojure.test :refer [deftest is testing]])
            [dfrese.orpheus.types :as types]
            [dfrese.orpheus.types.element :as element]
            [dfrese.orpheus.html :as html]))

(deftest html-test
  (is (= (element/element-type html/html-ns "div")
         (types/ve-type (html/div {} "foo"))))
  (is (= (element/element-type html/html-ns "div")
         (types/ve-type (html/div "foo"))))
  (is (= (element/element-type html/html-ns "div")
         (types/ve-type (html/div))))
  
  (is (= {"childNodes" ["foo"]}
         (types/ve-props (html/div {} "foo"))))
  (is (= {"childNodes" ["foo"]}
         (types/ve-props (html/div "foo"))))
  (is (= {}
         (types/ve-props (html/div {}))))
  (is (= {}
         (types/ve-props (html/div))))
  
  (is (= (html/div {"bar" 42} "foo")
         (html/div {"bar" 42} "foo"))))
