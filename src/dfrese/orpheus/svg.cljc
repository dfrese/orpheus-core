(ns dfrese.orpheus.svg
  (:require [dfrese.orpheus.core :as core]
            #?(:cljs [dfrese.orpheus.impl.util :refer-macros (deftag)])
            #?(:clj [dfrese.orpheus.impl.util :refer (deftag)])))

(def ^:no-doc svg-ns "http://www.w3.org/2000/svg")

(deftag svg svg-ns)
;; see http://www.w3schools.com/graphics/svg_intro.asp
