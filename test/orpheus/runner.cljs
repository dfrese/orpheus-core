(ns orpheus.runner
  (:require [doo.runner :refer-macros [doo-tests]]
            orpheus.core-test
            ))

(doo-tests 'orpheus.core-test
           )

