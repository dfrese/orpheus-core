(ns dfrese.orpheus.events
  (:require [dfrese.edomus.core :as dom]
            [dfrese.clj.functions :as f]))

;; TODO: keys, mouse positions/clicks?

;; TODO: add js/Event to edomus
(defn target-value
  "Return the `target.value` of an event"
  [e]
  (.-value (.-target e)))

(defn target-checked
  "Return the `target.checked` of an event."
  [e]
  (.-checked (.-target e)))

#?(:cljs
   (defn prevent-default! [^js/Event e]
     (.preventDefault e)
     e))

#?(:cljs
   (defn stop-propagation! [^js/Event e]
     (.stopPropagation e)
     e))

