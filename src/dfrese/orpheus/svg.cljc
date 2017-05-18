(ns dfrese.orpheus.svg
  (:require [dfrese.orpheus.core :as core]
            #?(:cljs [dfrese.orpheus.types.element :refer-macros (deftag)])
            #?(:clj [dfrese.orpheus.types.element :refer (deftag)])))

(def ^:no-doc svg-ns "http://www.w3.org/2000/svg")

(deftag svg svg-ns)
;; see http://www.w3schools.com/graphics/svg_intro.asp

;; TODO: extend property patching; e.g. :points of svg polyline elements. (so per element type?)
