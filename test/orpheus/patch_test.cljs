(ns orpheus.patch-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [orpheus.patch :as patch]
            [orpheus.lift :as lift]
            [orpheus.html :as html]
            [orpheus.core :as core :include-macros true]))

(defn cleanup! []
  (let [b (.-body js/document)]
    (doseq [c (vec (array-seq (.-childNodes b)))]
      (.removeChild b c))
    (doseq [a (vec (array-seq (.-attributes b)))]
      (.removeAttribute b a))))

(defn prepare! []
  (cleanup!)
  (let [e (.-body js/document)]
    (.appendChild e (.createElement js/document "div"))
    [e (lift/lift-properties e)]))

(defn patch-properties! [node state props]
  (patch/patch-properties! node state props))

(deftest patch-properties!-test
  (testing "it patches simple properties"
    (let [[node state] (prepare!)
          e {"className" "test"}]
      (is (not= "test" (aget node "className")))
      (patch-properties! node state e)
      (is (= "test" (aget node "className"))))))

(deftest patch-properties!-children-test
  (testing "it creates and patches children"
    (let [[node state] (prepare!)
          e (html/div {}
                      (html/span {}))
          state (patch-properties! node state {"childNodes" [e]})]
      (is (= "SPAN" (.-nodeName (.-firstChild (.-firstChild node)))))
      (let [state (patch-properties! node state
                                     {"childNodes" [(html/div {}
                                                              (html/p {}))]})]
        (is (= "P" (.-nodeName (.-firstChild (.-firstChild node))))))))
  (testing "it adds and removes children"
    (let [[node state] (prepare!)
          e #(hash-map "childNodes" (vector (apply html/div (repeat % (html/span {})))))]
      (let [state (patch-properties! node state (e 3))]
        (is (= 3 (.-length (.-childNodes (.-firstChild node)))))
        (patch-properties! node state (e 0))
        (is (= 0 (.-length (.-childNodes (.-firstChild node))))))))
  (testing "reused dom nodes if it can"
    (let [[node state] (prepare!)
          state (patch-properties! node state
                                   {"childNodes" [(html/div {}
                                                            (html/span {} "abc"))]})]
      (let [nodes #(vector
                    (.-firstChild node)                ;; 'div'
                    (.-firstChild (.-firstChild node)) ;; 'span'
                    ;;(.-firstChild (.-firstChild (.-firstChild node))) ;; txt
                    )
            before (nodes)]
        (patch-properties! node state
                           {"childNodes" [(html/div {}
                                                    (html/span {} "def"))]})
        (is (= before (nodes)))))))

(deftest patch-styles-test
  (testing "it patches style maps"
    (let [[node state] (prepare!)
          test #(.getPropertyValue (.-style (.-firstChild node)) "background-color")]
      (is (not= "red" (test)))
      ;; once
      (let [state (patch-properties! node state {"childNodes" [(html/div {"style" {"background-color" "red"
                                                                                   "padding-left" "12px"}})]})]
        (is (= "red" (test)))
        ;; and again
        (patch-properties! node state {"childNodes" [(html/div {"style" {"background-color" "blue"}})]})
        (is (= "blue" (test)))))))

(deftest patch-attributes-test
  (testing "it patches attribute maps"
    (let [[node state] (prepare!)
          test #(.-value (.getNamedItem (.-attributes (.-firstChild node)) "data-id"))]
      ;; once
      (let [style (patch-properties! node state {"childNodes" [(html/div {"attributes" {"data-id" "red"}})]})]
        (is (= "red" (test)))
        ;; and again
        (patch-properties! node state {"childNodes" [(html/div {"attributes" {"data-id" "blue"}})]})
        (is (= "blue" (test)))))))

(deftest patch-indirect-types-test
  (core/defnc my-comp [id]
    (html/div {"id" id}))

  (let [[node state] (prepare!)
        mk (fn [n]
             {"childNodes" [(apply html/div {} (map my-comp (range n)))]})
        getn (fn [node]
               (.-length (.-childNodes (.-firstChild node))))
        state (patch-properties! node state (mk 3))]
    (is (= 3 (getn node)))
    (let [state (patch-properties! node state (mk 10))]
      (is (= 10 (getn node)))
      (let [state (patch-properties! node state (mk 3))]
        (is (= 3 (getn node)))))))
