(ns dfrese.orpheus.mathml
  "Functions to create virtual dom elements from the MathML namespace."
  (:require [dfrese.orpheus.core :as core]
            #?(:cljs [dfrese.orpheus.types.element :refer-macros (deftag)])
            #?(:clj [dfrese.orpheus.types.element :refer (deftag)])))

(def ^{:doc "The MathML namespace."} mathml-ns "http://www.w3.org/1998/Math/MathML")

(deftag math mathml-ns)
;; ...
