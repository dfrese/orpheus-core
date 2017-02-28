(ns dfrese.orpheus.patch
  "Functions to apply a virtual dom to a real dom."
  (:require [dfrese.orpheus.core :as core]
            [dfrese.orpheus.patch.base :as base]
            [dfrese.edomus.core :as dom]
            ;; Load the type extensions that enable to pass ordinary native DOM elements in cljs:
            #?(:cljs [dfrese.edomus.browser])))

;; TODO: fix doc
(defn patch-properties!
  "Alter the given dom element, by modifying all properties that
  changed when comparing `old-props` with `new-props`. The `options`
  argument may be a map with a `:sync?` setting, resulting in
  synchronous modification, and a `:dispatch!` function used to
  augment event handlers."
  [element state props & [options]]
  ;; TODO: make state the first arg?
  (assert (map? props) (str "Properties to patch must be a map of properties to their values, not: " (pr-str props)))
  (let [props (core/normalize-props props)]
    (base/patch-properties! element
                            state props
                            (dom/element-owner-document element)
                            options)
    props))
