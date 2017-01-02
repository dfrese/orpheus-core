(ns orpheus.runner
  (:require [doo.runner :refer-macros [doo-tests]]
            orpheus.core-test
            orpheus.html-test
            orpheus.patch-test
            orpheus.patch.util-test
            orpheus.patch.base-test
            orpheus.lift-test
            ))

(doo-tests 'orpheus.core-test
           'orpheus.html-test
           'orpheus.patch-test
           'orpheus.patch.util-test
           'orpheus.patch.base-test
           'orpheus.lift-test
           )

