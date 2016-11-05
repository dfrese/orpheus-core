(ns orpheus.svg
  (:require [orpheus.dom :as dom]))

(def svg-ns "http://www.w3.org/2000/svg")

(defn- v [name]
  (let [type (dom/element-type svg-ns name nil)]
    (partial dom/h type)))

(def svg (v "svg"))
;; see http://www.w3schools.com/graphics/svg_intro.asp
