(ns dfrese.orpheus.patch.base-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [dfrese.orpheus.patch.base :as base]
            [dfrese.edomus.event :as dom-event]))

(deftest event-type?-test
  (is (base/event-type? "onclick"))
  (is (base/event-type? "onClick"))
  (is (base/event-type? "onClickCapture"))
  (is (= (dom-event/event-type "click" false) (base/event-type? "onclick")))
  (is (= (dom-event/event-type "click" false) (base/event-type? "onClick")))
  (is (= (dom-event/event-type "click" true) (base/event-type? "onClickCapture")))
  (is (= nil (base/event-type? "value"))))

