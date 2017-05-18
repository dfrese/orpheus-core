(ns dfrese.orpheus.types.element
  (:require [dfrese.orpheus.types :as core]
            [dfrese.edomus.core :as dom]
            [dfrese.edomus.event :as dom-event]
            [dfrese.orpheus.patch.util :as util]
            [clojure.string :as string]
            [clojure.set :as set]))

(defn- is-props? [arg0]
  (and (or (nil? arg0) (map? arg0))
       (not (core/velement? arg0))))

(defn- arg-props [t arg0]
  (if t
    arg0
    {}))

(defn- arg-children [t arg0 args]
  (if t
    args
    (cons arg0 args)))

(defn ^:no-doc h
  ([type] (core/velement type {}))
  ([type arg0 & args]
   (let [t (is-props? arg0)
         props (arg-props t arg0)
         children (arg-children t arg0 args)]
     (assert (map? props) (str "Props must be a map, not: " (pr-str props)))
     (if (or (contains? props "childNodes")
             (contains? props :childNodes))
       (do
         (assert (empty? children)
                 "Specify the child nodes either as a property, or as the argument list, but not both.")
         (core/velement type props))
       (core/velement type (cond-> props
                             ;; childNodes as a vector greatly helps patching.
                             (not-empty children) (assoc "childNodes" (vec children))))))))

(defrecord ^:no-doc ElementType
  [ns name options]
  #?@(:cljs [IFn
             (-invoke [this & args] (apply h this args))])
  #?@(:clj [clojure.lang.IFn
            (invoke [this] (h this))
            (invoke [this a1] (h this a1))
            (invoke [this a1 a2] (h this a1 a2))
            (invoke [this a1 a2 a3] (h this a1 a2 a3))
            (invoke [this a1 a2 a3 a4] (h this a1 a2 a3 a4))
            (invoke [this a1 a2 a3 a4 a5] (h this a1 a2 a3 a4 a5))
            (invoke [this a1 a2 a3 a4 a5 a6] (h this a1 a2 a3 a4 a5 a6))
            (invoke [this a1 a2 a3 a4 a5 a6 a7] (h this a1 a2 a3 a4 a5 a6 a7))
            (invoke [this a1 a2 a3 a4 a5 a6 a7 a8] (h this a1 a2 a3 a4 a5 a6 a7 a8))
            (invoke [this a1 a2 a3 a4 a5 a6 a7 a8 a9] (h this a1 a2 a3 a4 a5 a6 a7 a8 a9))
            (invoke [this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10] (h this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10))
            (invoke [this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11] (h this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11))
            (invoke [this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12] (h this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12))
            (invoke [this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13] (h this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13))
            (invoke [this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14] (h this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14))
            (invoke [this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15] (h this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15))
            (invoke [this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16] (h this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16))
            (invoke [this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17] (h this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17))
            (invoke [this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18] (h this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18))
            (invoke [this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19] (h this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19))
            (invoke [this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20] (h this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20))
            (invoke [this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21] (h this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21))
            (applyTo [this args] (apply h this args))]))

(defn element-type?
  "Return if v is an ElementType."
  [v]
  (instance? ElementType v))

(defn ^:no-doc create-element-node [document type]
  (dom/create-element-ns document (:ns type) (:name type) (:options type)))

(defn element-type
  "Returns a velement type for dom elements, given a node type string,
  and optionally a namespace and options."
  ;; Note: lowercase is at least the standard for html..
  ([ns name]
   (element-type ns name nil))
  ([ns name options]
   (ElementType. ns (string/lower-case name) options)))

#?(:clj
   (defmacro deftag [name ns]
     (let [doc (str "Returns a virtual `" (str name) "` element for an optional property map and children.")
           m {:doc doc
              :arglists ''([props & children] [& children])}]
       `(def 
          ~(vary-meta name merge m)
          (dfrese.orpheus.types.element/element-type ~ns ~(str name) nil)))))

(defmulti ^{:doc "TODO"
            :arglists '[[element property-name value options]]}
  init-global-property!
  (fn [element property-name value options]
    property-name)
  :default ::default)

(defmulti ^{:doc "TODO"
            :arglists '[[element property-name old-value new-value options]]}
  patch-global-property!
  (fn [element property-name old-value new-value options]
    property-name)
  :default ::default)

(defmulti ^{:doc "TODO"
            :arglists '[[element property-name value options]]}
  init-property!
  (fn [element property-name value options]
    [(dom/element-namespace element) (dom/element-name element) property-name])
  :default ::default)

(defmulti ^{:doc "TODO"
            :arglists '[[element property-name old-value new-value options]]}
  patch-property!
  (fn [element property-name old-value new-value options]
    [(dom/element-namespace element) (dom/element-name element) property-name])
  :default ::default)

(def ^:no-doc event-type-re #"(?i)on(.*)")
(def ^:no-doc event-type-capture-re #"(?i)on(.*)capture")

(defn ^:no-doc event-type-name? [s]
  (if-let [[_ name] (re-matches event-type-re s)]
    name
    nil))

(defn ^:no-doc event-type? [s]
  (if (and (> (count s) 2) ;; optimze a little with a quick preliminary test.
           (= "on" (subs s 0 2)))
    (if-let [[_ name] (re-matches event-type-capture-re s)]
      (dom-event/event-type name true)
      (if-let [name (event-type-name? s)]
        (dom-event/event-type name false)
        nil))
    nil))

(defn ^:no-doc create-js-event-handler [h dispatch!]
  (comp (if dispatch!
          (fn [e] (when (some? e)
                    (dispatch! e)))
          (constantly nil))
        h))

(defn ^:no-doc set-simple-property! [element name value options]
  (if-let [etype (and (or (nil? value) ;; nil for removal :-/ TODO: look at old-v if it's an handler instead?
                          (ifn? value))
                      (event-type? name))]
    (do
      ;; set event handlers as a side effect, unfortunately; but the
      ;; properties 'onclick' etc., cannot be called properly, for
      ;; custom event triggering (at least I did not find out). Only
      ;; the 'addEventListener' handlers can be called properly via
      ;; dispatchEvent - so we map them here.
      (if (nil? value)
        (dom-event/unset-event-handler! element etype)
        (let [f (create-js-event-handler value (:dispatch! options))]
          (dom-event/set-event-handler! element etype f))))
    ;; any other prop
    (dom/set-property! element name value)))

(defmethod init-global-property! ::default
  [element property-name value options]
  (set-simple-property! element property-name value options))

(defmethod init-property! ::default
  [element property-name value options]
  (init-global-property! element property-name value options))

(defmethod patch-global-property! ::default
  [element property-name old-value new-value options]
  (when (not= old-value new-value)
    (set-simple-property! element property-name new-value options)))

(defmethod patch-property! ::default
  [element property-name old-value new-value options]
  (patch-global-property! element property-name old-value new-value options))

;; style
;; classes
;; attributes

(defn ^:no-doc patch-style! [element old-v new-v]
  (util/patch-map-simple nil old-v new-v
                         #(dom/remove-style! element %2)
                         #(dom/set-style! element %2 %3)))

(defn ^:no-doc init-style! [element v]
  (reduce-kv #(dom/set-style! element %2 %3)
             nil
             v))

(defmethod init-global-property! "style"
  [element property-name value options]
  (init-style! element value))

(defmethod patch-global-property! "style"
  [element property-name old-value new-value options]
  (patch-style! element old-value new-value))

(defn- set!? [v]
  (if (set? v) v (set v)))

(defn ^:no-doc patch-classes! [element old-v new-v]
  (let [o (set!? old-v) ;; should be sets already for optimal performance
        n (set!? new-v)]
    (doseq [c (set/difference o n)]
      (dom/remove-class! element c))
    (doseq [c (set/difference n o)]
      (dom/add-class! element c))))

(defn ^:no-doc init-classes! [element v]
  (dom/set-classes! element (set!? v)))

(defmethod init-global-property! "classList"
  [element property-name value options]
  (init-classes! element value))

(defmethod patch-global-property! "classList"
  [element property-name old-value new-value options]
  (patch-classes! element old-value new-value))

(defn ^:no-doc patch-attributes! [element old-v new-v]
  (util/patch-map-simple nil old-v new-v
                         #(dom/remove-attribute! element %2)
                         #(dom/set-attribute! element %2 %3)))

(defn ^:no-doc init-attributes! [element v]
  (reduce-kv #(dom/set-attribute! element %2 %3)
             nil
             v))

(defmethod init-global-property! "attributes"
  [element property-name value options]
  (init-attributes! element value))

(defmethod patch-global-property! "attributes"
  [element property-name old-value new-value options]
  (patch-attributes! element old-value new-value))
