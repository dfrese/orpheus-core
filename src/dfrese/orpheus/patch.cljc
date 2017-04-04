(ns dfrese.orpheus.patch
  "Functions to apply a virtual dom to a real dom."
  (:require [dfrese.orpheus.core :as core]
            [dfrese.orpheus.patch.base :as base]
            [dfrese.orpheus.patch.lifting :as lifting]
            [dfrese.edomus.core :as dom]
            ;; Load the type extensions that enable to pass ordinary native DOM elements in cljs:
            #?(:cljs [dfrese.edomus.browser])))

(defrecord ^:no-doc State [vdom state])

(defn lift [element]
  (let [[state vdom] (lifting/lift-properties element)]
    (State. vdom state)))

;; TODO: fix doc
(defn patch!
  "Alter the given dom element, by modifying all properties that
  changed when comparing `old-props` with `new-props`. The `options`
  argument may be a map with a `:sync?` setting, resulting in
  synchronous modification, and a `:dispatch!` function used to
  augment event handlers."
  [state element props & [options]]
  (assert (map? props) (str "Properties to patch must be a map of properties to their values, not: " (pr-str props)))
  (assert (instance? State state) "The state argument must be the result of lift or a previous call to patch!.")
  (State. props
          (base/patch-properties! (:state state) element
                                  (:vdom state) props
                                  (dom/element-owner-document element)
                                  options)))
