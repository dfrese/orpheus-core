(ns dfrese.orpheus.lift-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [dfrese.orpheus.core :as core]
            [dfrese.orpheus.lift :as lift]
            [dfrese.orpheus.html :as html]))

(deftest lift-test
  (testing "it lifts basic children"
    (let [node (doto (.createElement js/document "div")
                 (.appendChild (.createElement js/document "span")))]
      (is (= {"childNodes" [(html/span {"childNodes" []})]}
             (try (lift/lift-properties node)
         (catch :default e
           (println (.-stack e))
           (throw e))))))))
