(ns dfrese.orpheus.patch.lifting-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [dfrese.orpheus.core :as core]
            [dfrese.orpheus.patch.lifting :as lifting]
            [dfrese.orpheus.html :as html]))

(deftest lift-test
  (testing "it lifts basic children"
    (let [node (doto (.createElement js/document "div")
                 (.appendChild (.createElement js/document "span")))
          [state vdom] (lifting/lift-properties node)]
      (is (= {"childNodes" [(html/span {"childNodes" []})]}
             vdom))
      (is (= [[]]
             state)))))
