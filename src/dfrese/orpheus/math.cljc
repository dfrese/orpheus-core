(ns dfrese.orpheus.math
  (:require [dfrese.orpheus.core :as core]
            #?(:cljs [dfrese.orpheus.types.element :refer-macros (deftag)])
            #?(:clj [dfrese.orpheus.types.element :refer (deftag)])))

(def ^:no-doc mathml-ns "http://www.w3.org/1998/Math/MathML")

(deftag math mathml-ns)
;; ...
