(ns dfrese.orpheus.events
  (:require [dfrese.edomus.core :as dom]
            [dfrese.clj.functions :as f]))

;; TODO: keys, mouse positions/clicks?

;; TODO: add more of this to edomus

(defn target-value
  "Returns `target.value` of the event."
  [event]
  (.-value (.-target event)))

(defn target-checked
  "Returns `target.checked` of the event."
  [event]
  (.-checked (.-target event)))

(defn prevent-default!
  "Calls the `preventDefault` method of the event, and returns it."
  [event]
  (.preventDefault event)
  event)

(defn stop-propagation!
  "Calls the `stopPropagation` method of the event, and returns it."
  [event]
  (.stopPropagation event)
  event)

