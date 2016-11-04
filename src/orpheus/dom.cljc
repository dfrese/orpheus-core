(ns orpheus.dom
  (:require [orpheus.core :as core]))

(def prevent-default
  (core/transformer (fn [e]
                      (.preventDefault e)
                      e)))

(def stop-propagation
  (core/transformer (fn [e]
                      (.stopPropagation e)
                      e)))

(defn- _h
  [type props children]
  (core/velement type (-> props
                          (assoc "childNodes" children))))

(defn h "Conveniently creates a virtual dom element."
  ([type] (_h type nil nil))
  ([type arg0 & args]
   (if (and (map? arg0) (not (core/velement? arg0)))
     (_h type arg0 args)
     (_h type nil (cons arg0 args)))))

(defrecord ElementType [ns name options])

(defrecord CustomType [ctor])

;; defrecord ReactType [...] (which is not IDOMNode)...
