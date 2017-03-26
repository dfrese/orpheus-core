(ns dfrese.orpheus.runner
  (:require [doo.runner :refer-macros [doo-tests]]
            dfrese.orpheus.core-test
            dfrese.orpheus.html-test
            dfrese.orpheus.patch-test
            dfrese.orpheus.patch.util-test
            dfrese.orpheus.patch.base-test
            dfrese.orpheus.patch.lifting-test
            ))

(doo-tests 'dfrese.orpheus.core-test
           'dfrese.orpheus.html-test
           'dfrese.orpheus.patch-test
           'dfrese.orpheus.patch.util-test
           'dfrese.orpheus.patch.base-test
           'dfrese.orpheus.patch.lifting-test
           )

