(ns dfrese.orpheus.patch-test
  (:require [dfrese.orpheus.patch :as patch]
            [dfrese.edomus.core :as dom]
            [dfrese.orpheus.test-utils :as t]
            #?@(:cljs [[cljs.test :refer-macros [deftest is testing]]])
            #?@(:clj [[clojure.test :refer [deftest is testing]]])
            [dfrese.orpheus.html :as html]
            [dfrese.orpheus.core :as core :include-macros true]))

(defn patch-properties! [node state props & [options]]
  (patch/patch-properties! node state props options))

(deftest patch-properties!-test
  (testing "it patches simple properties"
    (let [[node state] (t/prepare!)
          e {"className" "test"}]
      (is (not= "test" (dom/get-property node "className")))
      (patch-properties! node state e)
      (is (= "test" (dom/get-property node "className"))))))

(defn f-child [e]
  (first (dom/child-nodes e)))

(defn ff-child [e]
  (first (dom/child-nodes (first (dom/child-nodes e)))))

(defn sf-child [e]
  (second (dom/child-nodes (first (dom/child-nodes e)))))

(deftest patch-properties!-children-test
  (testing "it creates and patches children"
    (let [[node state] (t/prepare!)
          e (html/div {}
                      (html/span {}))
          state (patch-properties! node state {"childNodes" [e]})]
      (is (= "SPAN" (dom/element-name (ff-child node))))
      (let [state (patch-properties! node state
                                     {"childNodes" [(html/div {}
                                                              (html/p)
                                                              (html/h1))]})]
        (is (= "P" (dom/element-name (ff-child node))))
        (is (= "H1" (dom/element-name (sf-child node))))
        (let [state (patch-properties! node state
                                       {"childNodes" [(html/div {}
                                                                (html/h1)
                                                                (html/p))]})]
          (is (= "H1" (dom/element-name (ff-child node))))
          (is (= "P" (dom/element-name (sf-child node))))))))
  (testing "it adds and removes children"
    (let [[node state] (t/prepare!)
          e #(hash-map "childNodes" (vector (apply html/div (repeat % (html/span {})))))]
      (let [state (patch-properties! node state (e 3))]
        (is (= 3 (count (dom/child-nodes (f-child node)))))
        (patch-properties! node state (e 0))
        (is (= 0 (count (dom/child-nodes (f-child node))))))))
  (testing "reused dom nodes if it can"
    (let [[node state] (t/prepare!)
          state (patch-properties! node state
                                   {"childNodes" [(html/div {}
                                                            (html/span {} "abc"))]})]
      (let [nodes #(vector
                    (f-child node)  ;; 'div'
                    (ff-child node) ;; 'span'
                    )
            before (nodes)]
        (patch-properties! node state
                           {"childNodes" [(html/div {}
                                                    (html/span {} "def"))]})
        (is (= before (nodes)))))))

(deftest patch-styles-test
  (testing "it patches style maps"
    (let [[node state] (t/prepare!)
          test #(when-let [e (f-child node)]
                  (dom/get-style e "background-color"))]
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
    (let [[node state] (t/prepare!)
          test #(dom/get-attribute (f-child node) "data-id")]
      ;; once
      (let [state (patch-properties! node state {"childNodes" [(html/div {"attributes" {"data-id" "red"}})]})]
        (is (= "red" (test)))
        ;; and again
        (patch-properties! node state {"childNodes" [(html/div {"attributes" {"data-id" "blue"}})]})
        (is (= "blue" (test)))))))

(deftest patch-indirect-types-test
  (core/defnc my-comp [id]
    (html/div {"id" id}))

  (let [[node state] (t/prepare!)
        mk (fn [n]
             {"childNodes" [(apply html/div {} (map my-comp (range n)))]})
        getn (fn [node]
               (count (dom/child-nodes (f-child node))))
        state (patch-properties! node state (mk 3))]
    (is (= 3 (getn node)))
    (let [state (patch-properties! node state (mk 10))]
      (is (= 10 (getn node)))
      (let [state (patch-properties! node state (mk 3))]
        (is (= 3 (getn node)))))))

#?(:cljs (deftest patch-with-context-test
  (let [[node state] (t/prepare!)
        ev (atom nil)
        state (patch-properties! node state {})]
    ;; basic
    (let [state (patch-properties!
                 node state
                 {:childNodes [(core/with-context
                                 (html/div {:onClick (constantly :test)})
                                 {:dispatch! (fn [x] (reset! ev x))})]})]
      (.dispatchEvent (.-firstChild node)
                      (new js/Event "click"))
      (is (= @ev :test))

      ;; changing context
      (let [state (patch-properties!
                   node state
                   {:childNodes [(core/with-context
                                   (html/div {:onClick (constantly :test)})
                                   {:dispatch! (fn [x] (reset! ev :foobar))})]})]
        (.dispatchEvent (.-firstChild node)
                        (new js/Event "click"))
        (is (= @ev :foobar))

        ;; back to default.
        (let [state (patch-properties!
                     node state
                     {:childNodes [(html/div {:onClick (constantly :test)})]}
                     {:dispatch! (fn [x] (reset! ev :baz))})]
          (.dispatchEvent (.-firstChild node)
                          (new js/Event "click"))
          (is (= @ev :baz))
          
          ))))))

#?(:cljs (deftest patch-translate-test
  (let [[node state] (t/prepare!)
        ev (atom nil)
        state (patch-properties! node state {})]
    ;; basic
    (let [state (patch-properties!
                 node state
                 {:childNodes [(core/translate
                                (html/div {:onClick (constantly 42)})
                                inc)]}
                 {:dispatch! (fn [x] (reset! ev x))})]
      (.dispatchEvent (.-firstChild node)
                      (new js/Event "click"))
      (is (= @ev 43))))))
