(ns dfrese.orpheus.core
  "Functions to create and work with the Orpheus virtual dom elements."
  (:require [dfrese.orpheus.types :as types]
            [dfrese.orpheus.types.element :as element]
            [dfrese.orpheus.types.indirection :as indirection]
            [dfrese.clj.functions :as f]
            [clojure.string :as string]
            [clojure.set :as set])
  (:refer-clojure :exclude [key]))

(defn keyed
  "Adds a key to the given velement, which will guarantee that a node
  created for it, is reused during patching within the same child
  list.

  This can speed up patching if inserting a small number of elements
  in a long child list. But it can also be necessary for a good user
  experience of the page, because if all keyed elements in a child
  list do not change their order, it's also guaranteed that the real
  dom node for them is not removed from the page, and so it does not
  lose the focus. This may be important if the user is entering some
  text while the page around the input field changes."
  [velement key]
  (assert (types/velement? velement))
  (types/set-ve-key velement key))

(defn key
  "Returns the key of the given velement, or nil if none is set."
  [velement]
  (types/ve-key velement))

(defn- dispatch [f args msg]
  (apply f msg args))

(defn- update-dispatcher [[f args] options]
  (update options :dispatch!
          (fn [dp]
            (f/comp dp (f/partial dispatch f args)))))

(defn translate
  "Translate all messages dispatched from event handlers of `element`
  and its children, by piping them through `f`, which may return nil
  to skip the message."
  ([element f]
   (types/with-context-update element (f/partial update-dispatcher [f nil])))
  ([element f & args]
   (types/with-context-update element
     (f/partial update (f/partial update-dispatcher [f args])))))

(def ^{:doc "Conveniently creates a virtual dom element of the given
  type (see [[element-type]], where `arg0` may be a property map, and all
  following arguments are used as child nodes.
 
  Note that it's encouraged to use the even more convenient and
  readable element constructors in
  the [[dfrese.orpheus.html]], [[dfrese.orpheus.svg]]
  and [[dfrese.orpheus.mathml]] namespaces."
       :arglists '([type]
                   [type arg0 & args])}
  h element/h)

(def ^{:doc "Returns a velement type for dom elements, given a node
  name string, and namespace and optional options."
       :arglists '([ns name]
                   [ns name options])}
  element-type
  element/element-type)

(defn ^:no-doc function-ctor
  "Returns a function to elements for an argument list. To render such
  an element, `f` will be called with the arguments of the particular
  instance."
  [f]
  (let [t (indirection/indirection-type f)]
    (fn [& args]
      (types/velement t args))))

#?(:clj
   (defmacro defnc
     "Defines a 'functional component', which is just like a function,
  but which can speed up patching:

   `(defnc name [args]
  <a virtual dom element>)`

  Contruction of the virtual element tree stops at a functional
  component. Instead the function body is evaluated during patching,
  only if the argments to it are different than before (or there was a
  different type of element at that position in the tree
  previously). That implies that the function body should be free of
  side effects and return the same virtual dom element for the same
  arguments.

  This will significantly speed up patching, if the
  returned element tree is large or hard to compute, and if the
  arguments to it don't change all the time."
     ([name docstring? bindings? & body]
      (let [docstring (and (string? docstring?) docstring?)
            bindings (if docstring bindings? docstring?)
            body (if docstring body (cons bindings? body))]
        `(def ~name
           (cond-> (function-ctor (fn ~name ~bindings ~@body))
             ~docstring (vary-meta assoc :doc ~docstring)))))))

(defn get-property
  "Returns the value of property `p` for the given `element`."
  [element p]
  (let [m (types/ve-props element)]
    (if (contains? m p)
      (get m p)
      (get m (name p)))))

(defn update-property
  "Returns an element, which differs from `element` in that it has
  value `(apply f v args)` for property `p`, where `v` is the
  respective property value of `element` or `nil`."
  [element p f & args]
  (types/set-ve-props element
                      (let [curr (get-property element p)
                            upd (apply f curr args)]
                        (-> (types/ve-props element)
                            (dissoc p (name p))
                            (assoc p upd)))))

(defn assoc-property
  "Returns an element, which differs from `element` in that it has value `v` for property `p`."
  [element p v]
  (update-property element p (constantly v)))

(defn- resolve-element [element]
  (cond
    (and (types/velement? element)
         (indirection/indirection-type? (types/ve-type element)))
    (recur (indirection/expand-indirection (types/ve-type element) (types/ve-props element)))

    (types/with-context-update? element)
    (types/with-context-update-content element)

    :else
    element))

(defn- children [element]
  (let [e (resolve-element element)]
    (cond
      (and (types/velement? e)
           (element/element-type? (types/ve-type e)))
      (get-property e :childNodes)

      :else
      [])))

(defn- string-classes [s]
  (set (remove empty? (map string/trim (string/split s " ")))))

(defn- attribute [element k]
  (get (get-property element :attributes) k))

(defn- classes [element]
  (let [e (resolve-element element)]
    (set/union (get-property e :classList)
               (string-classes (get-property e :className))
               (string-classes (attribute e :class)))))

(defn find-by-class
  "Returns an element with the given class, which is either `element`
  itself or any of its contained elements, or `nil` if none is found."
  [class element]
  (let [e (resolve-element element)]
    (or (when (contains? (classes e) class)
          e)
        (some (partial find-by-class class)
              (children e)))))

(defn contains-element?
  "Returns true, if `element` or any of its contained elements equals
  `search`, and false otherwise."
  [element search]
  (or (= element search)
      (let [e (resolve-element element)]
        (or (= e search)
            (boolean (some #(contains-element? % search)
                           (children e)))))))
