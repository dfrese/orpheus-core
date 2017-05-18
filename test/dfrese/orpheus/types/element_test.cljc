(ns dfrese.orpheus.types.element-test
  (:require #?(:cljs [cljs.test :refer-macros [deftest is testing]])
            #?(:clj [clojure.test :refer [deftest is testing]])
            [dfrese.orpheus.types.element :as e]
            [dfrese.orpheus.html :as html]
            [dfrese.orpheus.core :as core]
            [dfrese.edomus.core :as dom]
            [dfrese.edomus.virtual :as virtual]
            [dfrese.edomus.event :as dom-event]))

(deftest event-type?-test
  (is (e/event-type? "onclick"))
  (is (e/event-type? "onClick"))
  (is (e/event-type? "onClickCapture"))
  (is (= (dom-event/event-type "click" false) (e/event-type? "onclick")))
  (is (= (dom-event/event-type "click" false) (e/event-type? "onClick")))
  (is (= (dom-event/event-type "click" true) (e/event-type? "onClickCapture")))
  (is (= nil (e/event-type? "value"))))

#_(deftest patch-state-test
  (let [doc (virtual/new-document)
        element (dom/create-element doc "div")]
    (is (= :foo (e/set-simple-property! element "height" 42 {})))

    (is (= :foo (e/patch-style! element {:padding "10" :margin "5"} {:padding "20" :color "red"})))
    (is (= :foo (e/init-style! element {:padding "10" :margin "5"})))

    (is (= :foo (e/patch-classes! element #{"a" "b"} #{"b" "c"})))
    (is (= :foo (e/init-classes! element #{"d" "e"})))

    (is (= :foo (e/patch-attributes! element {"title" "bar" "data" "42"} {"title" "baz" "aria" "foo"})))
    (is (= :foo (e/init-attributes! element {"title" "bar"})))

    (is (= :foo (e/patch-property! element "classList" #{"a" "b"} #{"b" "c"} doc {})))
    (let [v #{"a" "b"}]
      (is (= :foo (e/patch-property! element "classList" v v doc {})))))
  
  )
