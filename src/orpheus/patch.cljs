(ns orpheus.patch
  "Functions to apply a virtual dom to a real dom."
  (:require [edomus.core :as dom]
            [orpheus.core :as core]
            [orpheus.patch.base :as base]
            [edomus.browser :as dom-browser]))

;; TODO: fix doc
(defn patch-properties!
  "Alter the given dom element, by modifying all properties that
  changed when comparing `old-props` with `new-props`. The `options`
  argument may be a map with a `:sync?` setting, resulting in
  synchronous modification, and a `:dispatch!` function used to
  augment property value of type [[core/event-handler]]."
  [element state props & [options]]
  ;; TODO: make state the first arg?
  (assert (map? props) (str "Properties to patch must be a map of properties to their values, not: " (pr-str props)))
  (let [props (core/normalize-props props)]
    (dom-browser/execute #(base/patch-properties! element
                                                  state props
                                                  (dom/element-owner-document element)
                                                  options))
    props))
