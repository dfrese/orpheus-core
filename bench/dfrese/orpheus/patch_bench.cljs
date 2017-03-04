(ns dfrese.orpheus.patch-bench
  (:require [dfrese.orpheus.butil :refer-macros [defbench]]
            [dfrese.orpheus.core :as core :include-macros true]
            [dfrese.orpheus.patch :as patch]
            [dfrese.orpheus.test-utils :as t]))

(defn random-props [n]
  (t/generate t/props n 92739872))

(defbench patch-random-bench
  (let [e1 (random-props 30)
        e2 (random-props 20)

        [node state] (t/prepare!)
        N 1000]
    ;; a3be45c90e43680246622ab7a62707418901315a: 2267.5000132   ~2ms per patch
    ;; no lifecycle: 2149.7340216   -5%
    (doall (reduce (fn [st n]
                     (patch/patch-properties! node
                                              (patch/patch-properties! node st e1)
                                              e2))
                   state
                   (range N)))))
