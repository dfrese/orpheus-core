(ns orpheus.runner
  (:require [doo.runner :refer-macros [doo-tests]]
            orpheus.transformer-test
            ))

(doo-tests 'orpheus.transformer-test
           )

