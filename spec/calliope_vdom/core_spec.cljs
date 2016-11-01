(ns calliope-vdom.core-spec
  ;;(:require-macros [speclj.core :refer ])
  (:require [speclj.core :refer-macros [describe it should= should should-not should-be-nil]]
            [calliope-vdom.core :as core]
            [calliope-vdom.extension :as ext]))

(describe "h"
          (it "creates simple virtual dom elements"
              (let [e (core/h "div" {})]
                (should (instance? core/VElement e))))
          (it "creates nested virtual dom elements"
              (let [e (core/h "div" {} (core/h "span" {}) "Text")]
                (should (instance? core/VElement e)))))

(describe "handler"
          (it "chains handlers up"
              (let [h (core/handlers-> (core/handler (fn [e]
                                                       [:b e]))
                                       (core/handler (fn [e]
                                                       [:a e])))
                    res (atom nil)
                    f (ext/-convert h {:dispatch! identity})]
                (should= [:a [:b 42]] (f 42))))
          (it "is referentially transparent"
              (let [f1 (fn [e] [:a e])
                    f2 (fn [e] [:b e])
                    h (core/handlers-> (core/handler f2)
                                       (core/handler f1))]
                (should= (core/handlers-> (core/handler f2)
                                          (core/handler f1))
                         h))))
