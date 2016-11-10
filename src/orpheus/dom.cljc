(ns orpheus.dom
  (:require [orpheus.core :as core]
            [orpheus.transformer :as t]))

(defrecord EventHandler [t]
  #?@(:clj [clojure.lang.IFn
            (invoke [this e]
                    (t/transform e t))])
  #?@(:cljs [IFn
             (-invoke [this e]
                      (t/transform e t))])
  t/ITransformable
  (-get-fs [this] (t/-get-fs t))
  (-update-fs [this f]
    (EventHandler. (t/-update-fs t f))))

(defn event-handler
  ([] (EventHandler. t/ident))
  ([v]
   (if (instance? EventHandler v)
     v
     (EventHandler. (t/transformer v))))
  ([f a0 & args]
   (EventHandler. (apply t/transformer a0 args))))

(defn event-handler? [v]
  (instance? EventHandler v))

(defn const-handler [v]
  (event-handler (t/const v)))

(defn handler-> [h0 & hs]
  (apply t/trans-> (event-handler h0) hs))

(defn create-js-event-handler [h dispatch!]
  (assert (event-handler? h))
  (comp (if dispatch!
          (fn [e] (when (some? e)
                    (dispatch! e)))
          (constantly nil))
        (t/transformed h)))

(comment TODO put somewhere
         "Turns `f` into an event handler. The DOM event is passed to `f`,
  and if it returns non-nil, that value will be passed to the
  `:dispatch!` function from the options passed to [[patch-children!]]
  if defined. Another difference between raw functions and handlers
  is, that handlers can be chained in a referentially transparent
  way. See [[comp-handlers]]."
         )

(def prevent-default
  (event-handler (fn [e]
                   (.preventDefault e)
                   e)))

(def stop-propagation
  (event-handler (fn [e]
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
  (-patch-node! [this node old-props new-props options] "Update the dom node for new props of the same type.")
  (-destroy-node! [this node props options] "Clean up the dom node."))

;; Note a 'recursive' foreign type might be useful too, which takes a 'recurse' fn to render embedded velements.
