(ns dfrese.orpheus.patch.base-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [dfrese.orpheus.patch.base :as base]
            [dfrese.orpheus.html :as html]
            [dfrese.orpheus.core :as core]
            [dfrese.edomus.core :as dom]
            [dfrese.edomus.virtual :as virtual]
            [dfrese.edomus.event :as dom-event]))

(deftest event-type?-test
  (is (base/event-type? "onclick"))
  (is (base/event-type? "onClick"))
  (is (base/event-type? "onClickCapture"))
  (is (= (dom-event/event-type "click" false) (base/event-type? "onclick")))
  (is (= (dom-event/event-type "click" false) (base/event-type? "onClick")))
  (is (= (dom-event/event-type "click" true) (base/event-type? "onClickCapture")))
  (is (= nil (base/event-type? "value"))))

(defn patch [old new]
  (try (let [doc (virtual/new-document)
             element (dom/create-element doc "div")]
         (let [state (base/patch-properties! [] element {} old doc {})]
           (base/patch-properties! state element old new doc {}))
         element)
       (catch :default e
         (println (.-stack e))
         (throw e))))

(deftest patch-children!-test
  ;; no change
  (is (= ["DIV" "DIV" "DIV"]
         (map dom/element-name
              (dom/child-nodes (patch {:childNodes [(html/div) (html/div) (html/div)]}
                                      {:childNodes [(html/div) (html/div) (html/div)]})))))
  ;; all change
  (is (= ["SPAN" "SPAN" "SPAN"]
         (map dom/element-name
              (dom/child-nodes (patch {:childNodes [(html/div) (html/div) (html/div)]}
                                      {:childNodes [(html/span) (html/span) (html/span)]})))))
  ;; some change
  (is (= ["SPAN" "DIV" "SPAN"]
         (map dom/element-name
              (dom/child-nodes (patch {:childNodes [(html/div) (html/span) (html/div)]}
                                      {:childNodes [(html/span) (html/div) (html/span)]})))))
  ;; adding
  (is (= ["SPAN" "DIV" "SPAN"]
         (map dom/element-name
              (dom/child-nodes (patch {:childNodes [(html/div) (html/span)]}
                                      {:childNodes [(html/span) (html/div) (html/span)]})))))

  ;; removal
  (is (= ["SPAN" "DIV"]
         (map dom/element-name
              (dom/child-nodes (patch {:childNodes [(html/div) (html/span) (html/span)]}
                                      {:childNodes [(html/span) (html/div)]})))))

  ;; with keys..
  (is (= ["BUTTON" "SPAN" "DIV"]
         (map dom/element-name
              (dom/child-nodes (patch {:childNodes [(html/div) (-> (html/span) (core/keyed :a)) (-> (html/button) (core/keyed :b))]}
                                      {:childNodes [(-> (html/button) (core/keyed :b)) (-> (html/span) (core/keyed :a)) (html/div)]})))))
  (is (= ["SPAN" "DIV" "SPAN"]
         (map dom/element-name
              (dom/child-nodes (patch {:childNodes [(-> (html/span) (core/keyed :a)) (-> (html/div) (core/keyed :b)) (-> (html/span) (core/keyed :c))]}
                                      {:childNodes [(-> (html/span) (core/keyed :c)) (-> (html/div) (core/keyed :b)) (-> (html/span) (core/keyed :a))]})))))
  (is (= ["SPAN" "DIV" "SPAN"]
         (map dom/element-name
              (dom/child-nodes (patch {:childNodes [(-> (html/span) (core/keyed :a)) (-> (html/div) (core/keyed :b))]}
                                      {:childNodes [(-> (html/span) (core/keyed :c)) (-> (html/div) (core/keyed :b)) (-> (html/span) (core/keyed :a))]})))))
  (is (= ["SPAN" "DIV"]
         (map dom/element-name
              (dom/child-nodes (patch {:childNodes [(-> (html/div) (core/keyed :a)) (-> (html/div) (core/keyed :b)) (-> (html/span) (core/keyed :c))]}
                                      {:childNodes [(-> (html/span) (core/keyed :a)) (-> (html/div) (core/keyed :b))]})))))
  (is (= ["SPAN" "DIV"]
         (map dom/element-name
              (dom/child-nodes (patch {:childNodes [(-> (html/span) (core/keyed :a)) (-> (html/span) (core/keyed :b)) (-> (html/div) (core/keyed :c))]}
                                      {:childNodes [(-> (html/span) (core/keyed :b)) (-> (html/div) (core/keyed :c))]})))))

  ;; mixed
  (is (= ["SPAN" "DIV" "BUTTON" "H1"]
         (map dom/element-name
              (dom/child-nodes (patch {:childNodes [(-> (html/span) (core/keyed :a)) (-> (html/div) (core/keyed :b))]}
                                      {:childNodes [(-> (html/span) (core/keyed :b)) (html/div) (-> (html/button) (core/keyed :c)) (html/h1)]})))))
  )

;; TODO: Test keyed guarantees (same order preserves nodes; maybe event no removal)

(deftest patch-state-test
  (let [doc (virtual/new-document)
        element (dom/create-element doc "div")]
    (is (= :foo (base/set-simple-property! :foo element "height" 42 {})))

    (is (= :foo (base/patch-style! :foo element {:padding "10" :margin "5"} {:padding "20" :color "red"})))
    (is (= :foo (base/init-style! :foo element {:padding "10" :margin "5"})))

    (is (= :foo (base/patch-classes! :foo element #{"a" "b"} #{"b" "c"})))
    (is (= :foo (base/init-classes! :foo element #{"d" "e"})))

    (is (= :foo (base/patch-attributes! :foo element {"title" "bar" "data" "42"} {"title" "baz" "aria" "foo"})))
    (is (= :foo (base/init-attributes! :foo element {"title" "bar"})))

    (is (= :foo (base/patch-property! :foo element "classList" #{"a" "b"} #{"b" "c"} doc {})))
    (let [v #{"a" "b"}]
      (is (= :foo (base/patch-property! :foo element "classList" v v doc {})))))
  
  )
