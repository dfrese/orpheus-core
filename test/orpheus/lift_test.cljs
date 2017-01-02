(ns orpheus.lift-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [orpheus.core :as core]
            [orpheus.lift :as lift]
            [orpheus.html :as html]))

(deftest lift-test
  (testing "it lifts basic children"
    (let [node (doto (.createElement js/document "div")
                 (.appendChild (.createElement js/document "span")))]
      (is (= {"childNodes" [(html/span {"childNodes" []})]}
             (try (lift/lift-properties node)
         (catch :default e
           (println (.-stack e))
           (throw e))))))))
