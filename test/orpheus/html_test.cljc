(ns orpheus.html-test
  (:require #?(:cljs [cljs.test :refer-macros [deftest is testing]])
            #?(:clj [clojure.test :refer [deftest is testing]])
            [orpheus.core :as core]
            [orpheus.html :as html]))

(deftest html-test
  (is (= (core/element-type html/html-ns "div")
         (core/ve-type (html/div {} "foo"))))
  (is (= (core/element-type html/html-ns "div")
         (core/ve-type (html/div "foo"))))
  (is (= (core/element-type html/html-ns "div")
         (core/ve-type (html/div))))
  
  (is (= {"childNodes" ["foo"]}
         (core/ve-props (html/div {} "foo"))))
  (is (= {"childNodes" ["foo"]}
         (core/ve-props (html/div "foo"))))
  (is (= {}
         (core/ve-props (html/div {}))))
  (is (= {}
         (core/ve-props (html/div))))
  
  (is (= (html/div {"bar" 42} "foo")
         (html/div {"bar" 42} "foo"))))
