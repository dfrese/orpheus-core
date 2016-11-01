(ns calliope-vdom.core
  (:require [calliope-vdom.extension :as ext]))

(defrecord VElement [type props children])

(def ^:no-doc ve-type :type)
(def ^:no-doc ve-props :props)
(def ^:no-doc ve-children :children)

(defn map-properties [vdom f]
  (if (instance? VElement vdom)
    (VElement. type
               (reduce-kv (fn [r n v]
                            (assoc r n (f n v)))
                          {}
                          (ve-props vdom))
               (map #(map-properties % f)
                    (ve-children vdom)))
    vdom))

(defn map-values [vdom f]
  (map-properties vdom
                  (fn [n v]
                    (f v))))


(defn h "TODO"
  [type props & children]
  (assert (map? props))
  ;; type = webcomponent ctor, or element name
  (VElement. type props children))

;;(defn style-map)

(defn- comp-fs [fs]
  ;; Note: last one in list will be applied first
  (apply comp
         (map (fn [[f args]]
                (fn [e]
                  (when (some? e)
                    (apply f e args))))
              fs)))

;; TODO: rename extractor/transformer or alike.
(defrecord EventHandler [fs]
  IFn ;; TODO: only cljs?
  (-invoke [this e]
    ((comp-fs fs) e))
  ext/IConvertible
  (-convert [this options]
    (comp (if-let [dispatch! (:dispatch! options)]
            (fn [e] (when (some? e)
                      (dispatch! e)))
            (constantly nil))
          (comp-fs fs))))

(defn handler
  "Turns `f` into an event handler. The DOM event is passed to `f`,
  and if it returns non-nil, that value will be passed to the
  `:dispatch!` function from the options passed to [[patch-children!]]
  if defined. Another difference between raw functions and handlers
  is, that handlers can be chained in a referentially transparent
  way. See [[comp-handlers]]."
  [f & args]
  (EventHandler. [[f args]]))

(defn comp-handlers
  "Chain several handlers into one. This is referentially transparent,
  meaning that composing the same handlers again is `=`."
  [h0 & hs]
  (EventHandler. (apply mapcat #(.-fs %) h0 hs)))

(defn handlers-> [& hs]
  (comp-handlers (reverse hs)))

;; Some standard handlers (not really cljc :-/)

(defn- snd [_ v] v)

(defn const
  "A handler, that sends the same message for every occurence of the event. Note that `nil` is not a valid message."
  [msg]
  (handler snd msg))

(defn- _tag [v id]
  [id v])
(defn tag [id]
  (handler _tag id))
