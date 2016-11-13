(ns orpheus.math
  (:require [orpheus.core :as core]
            #?(:cljs [orpheus.impl.util :refer-macros (deftag)])
            #?(:clj [orpheus.impl.util :refer (deftag)])))

(def ^:no-doc mathml-ns "http://www.w3.org/1998/Math/MathML")

(deftag math mathml-ns)
;; ...
