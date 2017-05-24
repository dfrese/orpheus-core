(ns dfrese.orpheus.patch
  "Functions to apply a virtual dom to a real dom."
  (:require [dfrese.orpheus.core :as core]
            [dfrese.orpheus.patch.base :as base]
            [dfrese.orpheus.patch.lifting :as lifting]
            [dfrese.edomus.core :as dom]
            ;; Load the type extensions that enable to pass ordinary native DOM elements in cljs:
            #?(:cljs [dfrese.edomus.browser])))

(defrecord ^:no-doc State [vdom state])

(defn lift
  "Returns a patch state (see [[patch!]]), representing the current
  property values of the given dom element node. The tree below the
  given element may be modified as a side effect, e.g. removing
  elements that have no representation in the virtual dom tree, like
  comments."
  [element]
  (let [[state vdom] (lifting/lift-properties element)]
    (State. vdom state)))

(defn patch!
  "Modifies the given dom element node, by updating all properties given, and
  removing all properties previously set but not present anymore.

  The `:childNodes` key must contain a sequence of virtual dom
  elements. All event handlers and some other properties are also treated
  specially.

  Takes a patch state as the first argument, which must be the one
  returned from the previous call, or from a call to [[lift]].

  The options map may contain:

   - a `:dispatch!` function is called on all non-nil values returned
  by events handlers.
  "
  [^State state element props & [options]]
  (assert (map? props) (str "Properties to patch must be a map of properties to their values, not: " (pr-str props)))
  (assert (instance? State state) "The state argument must be the result of lift or a previous call to patch!.")
  (State. props
          (base/patch-properties! (:state state) element
                                  (:vdom state) props
                                  (dom/element-owner-document element)
                                  options)))
