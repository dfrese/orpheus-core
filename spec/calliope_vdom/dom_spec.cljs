(ns calliope-vdom.dom-spec
  (:require-macros [speclj.core :refer [describe it should= should should-not should-be-nil]])
  (:require [speclj.core]
            [calliope-vdom.core :as core]
            [calliope-vdom.dom :as dom]))

(defn cleanup! []
  (let [b (.-body js/document)]
    (doseq [c (vec (array-seq (.-childNodes b)))]
      (.removeChild b c))
    (doseq [a (vec (array-seq (.-attributes b)))]
      (.removeAttribute b a))))

(defn prepare! []
  (cleanup!)
  (.appendChild (.-body js/document)
                (.createElement js/document "div"))
  [(.-body js/document) [(core/h "div" {})]])

(describe "patch-children!"
          (it "patches simple properties"
              (let [[node prev] (prepare!)
                    e (core/h "div" {"className" "test"})]
                (should (not= "test" (aget (.-firstChild node) "className")))
                (dom/patch-children! node prev [e])
                (dom/flush!)
                (should= "test" (aget (.-firstChild node) "className"))))
          (it "creates and patches children"
              (let [[node prev] (prepare!)
                    e (core/h "div" {}
                              (core/h "span" {}))]
                (dom/patch-children! node prev [e])
                (dom/flush!)
                (should= "SPAN" (.-nodeName (.-firstChild (.-firstChild node))))
                (dom/patch-children! node [e]
                                     [(core/h "div" {}
                                              (core/h "p" {}))])
                (dom/flush!)
                (should= "P" (.-nodeName (.-firstChild (.-firstChild node))))))
          (it "patches style maps"
              (let [[node prev] (prepare!)
                    test #(.getPropertyValue (.-style (.-firstChild node)) "background-color")]
                (should (not= "red" (test)))
                ;; once
                (dom/patch-children! node prev [(core/h "div" {"style" {"background-color" "red"}})])
                (dom/flush!)
                (should= "red" (test))
                ;; and again
                (dom/patch-children! node prev [(core/h "div" {"style" {"background-color" "blue"}})])
                (dom/flush!)
                (should= "blue" (test))
                ))
          (it "reused dom nodes if it can"
              (let [[node prev] (prepare!)
                    e (core/h "div" {}
                              (core/h "span" {} "abc"))]
                (dom/patch-children! node prev [e])
                (dom/flush!)
                (let [nodes #(vector
                              (.-firstChild node) ;; 'div'
                              (.-firstChild (.-firstChild node)) ;; 'span'
                              ;;(.-firstChild (.-firstChild (.-firstChild node))) ;; txt
                              )
                      before (nodes)]
                  (dom/patch-children! node
                                       [e]
                                       [(core/h "div" {}
                                                (core/h "span" {} "def"))])
                  (dom/flush!)
                  (should= before (nodes)))))
          
          )
