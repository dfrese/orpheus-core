(ns orpheus.core
  (:require [active.clojure.arrow :as a]
            [clojure.string :as string]))

(defrecord ^:no-doc VElement [type props])

(def ^{:doc "Returns the type of a velement."
       :arglists '([v])}
  ve-type :type)

(def ^{:doc "Returns the properties of a velement."
       :arglists '([v])}
  ve-props :props)

(defn velement "Returns a velement given a velement type and a property map." [type props]
  (assert (map? props) (str "Props must be a map, not: " (pr-str props)))
  (VElement. type props))

(defn velement? "Returns true if `v` is a velement." [v]
  (instance? VElement v))

(defn memoize-1
  "Returns a memoized version of the given function `f` just
  like [[clojure.core/memoize]], but only one result is cached."
  [f]
  (let [mem (atom nil)]
    (fn [& args]
      (let [v @mem]
        (if (and v (= (first v) args))
          (second v)
          (let [v (apply f args)]
            (reset! mem [args v])
            v))))))

(defn memoize-n
  "Returns a memoized version of the given function `f` just
  like [[clojure.core/memoize]], but only the last `n` results are
  cached."
  [n f]
  (let [mem (atom (array-map))]
    (fn [& args]
      (if-let [e (find @mem args)]
        (val e)
        (let [ret (apply f args)]
          (swap! mem (fn [m]
                       (-> m
                           (dissoc (first (first m)))
                           (assoc args ret))))
          ret)))))

(defn memoize-2
  "Returns a memoized version of the given function `f` just
  like [[clojure.core/memoize]], but only the last two results are
  cached."
  [f]
  (memoize-n 2 f))

;; --- tagging ---

(defrecord ^:no-doc Tagged [value tag])

(defn tagged
  "Returns `v` tagged with `tag`."
  [tag v]
  (Tagged. v tag))

(defn tagged?
  "Returns true if `v` is a tagged value."
  [v]
  (instance? Tagged v))

(defn tag
  "Returns an arrow that wraps any value as a [[tagged]] value with `t` as the tag."
  [t]
  (a/arrow ->Tagged t))

(defn untag
  "Returns a tuple `[tag value]`, if `v` is a [[tagged]] value, or `[v nil]` otherwise."
  [v]
  (if (tagged? v)
    [(:tag v) (:value v)]
    [v nil]))

;; --- event handler ---

(defrecord EventHandler ^:no-doc [arrow]
  ;; Implement IFn, for convenience in test cases.
  #?@(:clj [clojure.lang.IFn
            (invoke [this e]
                    (arrow e))])
  #?@(:cljs [IFn
             (-invoke [this e]
                      (arrow e))]))

(defn event-handler [f & args]
  (EventHandler. (apply a/arrow f args)))

(defn event-handler?
  "Returns true if `v` is an event handler."
  [v]
  (instance? EventHandler v))

(defn ^:no-doc when-not-nil
  ([v a]
   (when-not (nil? v)
     (a v)))
  ([a]
   (a/arrow when-not-nil a)))

(defn ^:no-doc event-handler-arrow [^EventHandler eh]
  (.-arrow eh))

(defn event-handler->
  "Returns an event handler, from an arrow or one-argument function,
  optionally adding arrows to transform the event before it is
  dispatched later on. Note that the compisiton of the arrows is
  modified, so that if one returns `nil`, the following arrows are not
  applied anymore."
  ([]
   (event-handler-> a/ident))
  ([v]
   (if (event-handler? v)
     v
     (EventHandler. (a/arrow v))))
  ([a0 & as]
   (if (event-handler? a0)
     (EventHandler. (apply a/>>> (event-handler-arrow a0) (map when-not-nil as)))
     (EventHandler. (apply a/>>> a0 (map when-not-nil as))))))

(defn const-handler
  "Returns an event handler, that always translates an event to the given value `v`."
  [v]
  (event-handler-> (a/const v)))

(defn ^:no-doc create-js-event-handler [h dispatch!]
  (assert (event-handler? h))
  (comp (if dispatch!
          (fn [e] (when (some? e)
                    (dispatch! e)))
          (constantly nil))
        (event-handler-arrow h)))

(comment TODO put somewhere
         "Turns `f` into an event handler. The DOM event is passed to `f`,
  and if it returns non-nil, that value will be passed to the
  `:dispatch!` function from the options passed to [[patch-children!]]
  if defined. Another difference between raw functions and handlers
  is, that handlers can be chained in a referentially transparent
  way. See [[comp-handlers]]."
         )

#?(:cljs
   (def prevent-default
     (event-handler-> (fn [^js/Event e]
                        (.preventDefault e)
                        e))))

#?(:cljs
   (def stop-propagation
     (event-handler-> (fn [^js/Event e]
                        (.stopPropagation e)
                        e))))

;; --- virtual dom elements ---

(defn- is-props? [arg0]
  (and (or (nil? arg0) (map? arg0))
       (not (velement? arg0))))

(defn- arg-props [arg0]
  (if (is-props? arg0)
    arg0
    {}))

(defn- arg-children [arg0 args]
  (if (is-props? arg0)
    args
    (cons arg0 args)))

(defn h "Conveniently creates a virtual dom element of the given type,
  where `arg0` may be a property map, and all following arguments are
  used as child nodes."
  ([type] (velement type {}))
  ([type arg0 & args]
   (let [props (arg-props arg0)
         children (arg-children arg0 args)]
     (if (contains? props "childNodes")
       (do
         (assert (empty? children)
                 "Specify the child nodes either as a property, or as the argument list, but not both.")
         (velement type props))
       (velement type (cond-> props
                        (not-empty children) (assoc "childNodes" children)))))))

(defrecord ^:no-doc ElementType [ns name options])

(defn element-type
  "Returns a velement type for dom elements, given a node type string,
  and optionally a namespace and options."
  ;; Note: lowercase is at least the standard for html..
  ([name] (string/lower-case name))
  ([ns name]
   (element-type ns name nil))
  ([ns name options]
   (ElementType. ns (string/lower-case name) options)))

;; Note custom elements can also be created via ElementType, but this
;; is for creation via a constructor function which is more convenient
;; in some situations.
(defrecord ^:no-doc CustomElementType [ctor args])

(defn custom-element-type
  "Returns a velement type for a custom constructor function and optional arguments."
  [ctor & args]
  (CustomElementType. ctor args))

;; A foreign type could be something like a react component, which can
;; be integrated into the dom, but has special rules for construction
;; and patching.
(defprotocol ForeignType
  "A protocol for velement types with special node creation and update
  methods, where the velement properties may represent something
  other that dom element properties."
  (-create-node [this props options] "Create a dom node for this type and props.")
  (-patch-node! [this node old-props new-props options] "Update the dom node for new props of the same type.")
  (-destroy-node! [this node props options] "Clean up the dom node."))

;; Note a 'recursive' foreign type might be useful too, which takes a 'recurse' fn to render embedded velements.


