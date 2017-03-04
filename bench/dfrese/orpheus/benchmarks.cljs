(ns dfrese.orpheus.benchmarks
  (:require [doo.runner :refer-macros [doo-tests]]
            dfrese.orpheus.patch-bench
            ))

(doo-tests 'dfrese.orpheus.patch-bench)
