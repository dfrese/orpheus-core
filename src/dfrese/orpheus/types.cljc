(ns dfrese.orpheus.types)

;; The vdom tree may consist of: velements, with-context-update and strings.
;; velements have a type, which is one of: element (dom), foreign-type, indirection-type

(defrecord ^:no-doc VElement [type props key])

(def ^{:doc "Returns the type of a velement."
       :arglists '([v])}
  ve-type
  #?(:cljs (fn [^VElement e] (.-type e)))
  #?(:clj :type))

(def ^{:doc "Returns the properties of a velement."
       :arglists '([v])}
  ve-props
  #?(:cljs (fn [^VElement e] (.-props e)))
  #?(:clj :props))

(def ^{:doc "Returns the key of a velement or nil."
       :arglists '([v])}
  ve-key
  #?(:cljs (fn [^VElement e] (.-key e)))
  #?(:clj :key))

(defn set-ve-props [element v]
  (VElement. (ve-type element) v (ve-key element)))

(defn set-ve-key [^VElement element v]
  (VElement. (ve-type element) (ve-props element) v))

(defn velement
  "Returns a velement given a velement type and a property map."
  ([type props]
   (VElement. type props nil))
  ([type props key]
   (VElement. type props key)))

(defn velement? "Returns true if `v` is a velement." [v]
  (instance? VElement v))

(defrecord ^:no-doc WithContextUpdate [content update-options])

(defn with-context-update [content update-options]
  (WithContextUpdate. content update-options))

(defn with-context-update? [v]
  (instance? WithContextUpdate v))

(def with-context-update-content :content)
(def with-context-update-f :update-options)

#_(defn with-context [content options]
  (with-context-update content (f/constantly options)))

