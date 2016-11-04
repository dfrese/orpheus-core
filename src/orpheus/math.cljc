(ns orpheus.math
  (:require [orpheus.dom :as dom]))

(def mathml-ns "http://www.w3.org/1998/Math/MathML")

(defn- v [name]
  (let [type (dom/ElementType. mathml-ns name nil)]
    (partial dom/h type)))

(def math (v "math"))
;; ...
