(ns calliope-vdom.html
  (:require [calliope-vdom.core :as core]))

;; TODO: all others..?
(defn div [& args] (apply core/h "div" args))

(def prevent-default
  (core/handler (fn [e]
                  (.preventDefault e)
                  e)))

(def stop-propagation
  (core/handler (fn [e]
                  (.stopPropagation e)
                  e)))

(def target-value ^{:doc "A handler, that returns the target.value"}
  (core/handler (fn [e]
                  (.-value (.-target e)))))

(def target-checked ^{:doc "A handler, that returns the `target.checked`."}
  (core/handler (fn [e]
                  (.-checked (.-target e)))))

;; TODO: keys, mouse positions/clicks.

