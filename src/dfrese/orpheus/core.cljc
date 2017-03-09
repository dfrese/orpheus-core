(ns dfrese.orpheus.core
  (:require [dfrese.edomus.core :as dom]
            [dfrese.clj.functions :as f]
            [clojure.string :as string]))

(defrecord ^:no-doc VElement [type props key])

(def ^{:doc "Returns the type of a velement."
       :arglists '([v])}
  ve-type :type)

(def ^{:doc "Returns the properties of a velement."
       :arglists '([v])}
  ve-props :props)

(def ^{:doc "Returns the key of a velement or nil."
       :arglists '([v])}
  ve-key :key)

(defn velement
  "Returns a velement given a velement type and a property map."
  ([type props]
   (VElement. type props nil))
  ([type props key]
   (VElement. type props key)))

(defn keyed
  "Adds a key to the given velement, which will guarantee that a node
  created for it, is reused during patching within the same child
  list."
  [velement key]
  (assoc velement :key key))

(defn velement? "Returns true if `v` is a velement." [v]
  (instance? VElement v))

(defrecord ^:no-doc WithContextUpdate [content update-options])

(defn with-context-update [content update-options]
  (WithContextUpdate. content update-options))

(defn with-context-update? [v]
  (instance? WithContextUpdate v))

(defn- dispatch [f args msg]
  (apply f msg args))

(defn- update-dispatcher [[f args] options]
  (update options :dispatch!
          (fn [dp]
            (f/comp dp (f/partial dispatch f args)))))

(defn translate
  "Translate all messages sent by `element`, by piping them through `f`, which may return nil to skip the message."
  ([element f]
   (with-context-update element (f/partial update-dispatcher [f nil])))
  ([element f & args]
   (with-context-update element
     (f/partial update (f/partial update-dispatcher [f args])))))

(defn with-context [content options]
  (with-context-update content (f/constantly options)))

;; --- event handler ---

(defn ^:no-doc create-js-event-handler [h dispatch!]
  (comp (if dispatch!
          (fn [e] (when (some? e)
                    (dispatch! e)))
          (constantly nil))
        h))

(comment TODO put somewhere
         "Turns `f` into an event handler. The DOM event is passed to `f`,
  and if it returns non-nil, that value will be passed to the
  `:dispatch!` function from the options passed to [[patch-children!]]
  if defined. Another difference between raw functions and handlers
  is, that handlers can be chained in a referentially transparent
  way. See [[comp-handlers]]."
         )

#?(:cljs
   (defn prevent-default! [^js/Event e]
     (.preventDefault e)
     e))

#?(:cljs
   (defn stop-propagation! [^js/Event e]
     (.stopPropagation e)
     e))

;; --- virtual dom elements ---

(defn- is-props? [arg0]
  (and (or (nil? arg0) (map? arg0))
       (not (velement? arg0))))

(defn ^:no-doc normalize-props [m]
  ;; Note: keeps string keys as the most efficient usage.
  (reduce-kv (fn [m k v]
               ;; TODO: also look into style?!
               (cond-> m
                 (not (string? k))
                 (-> (dissoc k) (assoc (name k) v))

                 ;; childNodes as a vector greatly helps patching.
                 (and (= "childNodes" (name k))
                      (not (vector? v)))
                 (update "childNodes" vec)))
             m
             m))

(defn- arg-props [t arg0]
  (if t
    (normalize-props arg0)
    {}))

(defn- arg-children [t arg0 args]
  (if t
    args
    (cons arg0 args)))

(defn h "Conveniently creates a virtual dom element of the given type,
  where `arg0` may be a property map, and all following arguments are
  used as child nodes."
  ([type] (velement type {}))
  ([type arg0 & args]
   (let [t (is-props? arg0)
         props (arg-props t arg0)
         children (arg-children t arg0 args)]
     (assert (map? props) (str "Props must be a map, not: " (pr-str props)))
     (if (contains? props "childNodes")
       (do
         (assert (empty? children)
                 "Specify the child nodes either as a property, or as the argument list, but not both.")
         (velement type props))
       (velement type (cond-> props
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

(defn create-element-node [document type]
  (dom/create-element-ns document (:ns type) (:name type) (:options type)))

(defn element-type
  "Returns a velement type for dom elements, given a node type string,
  and optionally a namespace and options."
  ;; Note: lowercase is at least the standard for html..
  ([ns name]
   (element-type ns name nil))
  ([ns name options]
   (ElementType. ns (string/lower-case name) options)))

;; A lead type could be something like a react component, which can
;; be integrated into the dom, but has special rules for construction
;; and may also participate in the patching, by using it as the type
;; of a velement.
(defprotocol ILeafType
  "A protocol for velement types with special node creation and update
  methods. There are no restrictions on the type of properties, which
  are directly taken from the argument to [[velement]]."
  (leaf-type-create [this props options] "Create a dom node for this type and props.")
  (leaf-type-patch! [this node old-props new-props options] "Update the dom node for new props of the same type.")
  (leaf-type-destroy! [this node props options] "Clean up the dom node."))

(defn leaf-type?
  "Returns if v is a ILeafType."
  [v]
  (satisfies? ILeafType v))

;; function type components:

(defrecord ^:no-doc IndirectionType [f])

(defn indirection-type?
  "Return if v is an IndirectionType."
  [v]
  (instance? IndirectionType v))

(defn expand-indirection
  "Returns a different velement object, that this type and properties stand for."
  [this props]
  (apply (:f this) props))

(defn expand-fnc [c] ;; TODO: doc; only for testing/and recurse?
  (expand-indirection (ve-type c) (ve-props c)))

(defn- indirection-type [f]
  (IndirectionType. f))

(defn ^:no-doc function-ctor
  "Returns a function to elements for an argument list. To render such
  an element, `f` will be called with the arguments of the particular
  instance."
  [f]
  (let [t (indirection-type f)]
    (fn [& args]
      (velement t args))))

#?(:clj
   (defmacro defnc
     ([name docstring? bindings? & body]
      (let [docstring (and (string? docstring?) docstring?)
            bindings (if docstring bindings? docstring?)
            body (if docstring body (cons bindings? body))]
        `(def ~name
           (cond-> (function-ctor (fn ~name ~bindings ~@body))
             ~docstring (vary-meta assoc :doc ~docstring)))))))
