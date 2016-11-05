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

(defn element-type [ns name options]
  (ElementType. ns name options))

;; Note custom elements can also be created via ElementType, but this
;; is for creation via a constructor function which is more convenient
;; in some situations.
(defrecord CustomElementType [ctor args])

(defn custom-element-type [ctor & args]
  (CustomElementType. ctor args))

;; A foreign type could be something like a react component, which can
;; be integrated into the dom, but has special rules for construction
;; and patching.
(defprotocol ForeignType
  (-create-node [this props options] "Create a dom node for this type and props.")
  (-patch-node! [this old-props new-props options] "Update the dom node for new props of the same type."))

;; Note a 'recursive' foreign type might be useful too, which takes a 'recurse' fn to render embedded velements.
