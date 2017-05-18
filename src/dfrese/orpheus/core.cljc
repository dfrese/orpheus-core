(ns dfrese.orpheus.core
  (:require [dfrese.orpheus.types :as types]
            [dfrese.orpheus.types.element :as element]
            [dfrese.orpheus.types.indirection :as indirection]
            [dfrese.clj.functions :as f]))

(defn keyed
  "Adds a key to the given velement, which will guarantee that a node
  created for it, is reused during patching within the same child
  list."
  [velement key]
  (assert (types/velement? velement))
  (types/set-ve-key velement key))

(defn- dispatch [f args msg]
  (apply f msg args))

(defn- update-dispatcher [[f args] options]
  (update options :dispatch!
          (fn [dp]
            (f/comp dp (f/partial dispatch f args)))))

(defn translate
  "Translate all messages sent by `element`, by piping them through
  `f`, which may return nil to skip the message."
  ([element f]
   (types/with-context-update element (f/partial update-dispatcher [f nil])))
  ([element f & args]
   (types/with-context-update element
     (f/partial update (f/partial update-dispatcher [f args])))))

(defn ^:no-doc create-js-event-handler [h dispatch!]
  (comp (if dispatch!
          (fn [e] (when (some? e)
                    (dispatch! e)))
          (constantly nil))
        h))

(def ^{:doc "Conveniently creates a virtual dom element of the given type,
  where `arg0` may be a property map, and all following arguments are
  used as child nodes."
       :arglists '([type]
                   [type arg0 & args])}
  h element/h)

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
     ([name docstring? bindings? & body]
      (let [docstring (and (string? docstring?) docstring?)
            bindings (if docstring bindings? docstring?)
            body (if docstring body (cons bindings? body))]
        `(def ~name
           (cond-> (function-ctor (fn ~name ~bindings ~@body))
             ~docstring (vary-meta assoc :doc ~docstring)))))))

(defn get-property [element p]
  (let [m (types/ve-props element)]
    (if (contains? m p)
      (get m p)
      (get m (name p)))))

(defn update-property [element p f & args]
  (types/set-ve-props element
                      (let [curr (get-property element p)
                            upd (apply f curr args)]
                        (-> (types/ve-props element)
                            (dissoc p (name p))
                            (assoc p upd)))))

(defn assoc-property [element p v]
  (update-property element p (constantly v)))
