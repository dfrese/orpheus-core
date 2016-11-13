(ns orpheus.runner
  (:require [doo.runner :refer-macros [doo-tests]]
            orpheus.core-test
            orpheus.html-test))

(doo-tests 'orpheus.core-test
           'orpheus.html-test
           )

