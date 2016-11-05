(ns orpheus.core)

(defrecord VElement [type props])

(def ^:no-doc ve-type :type)
(def ^:no-doc ve-props :props)

;; (defn map-properties [vdom f]
;;   (if (instance? VElement vdom)
;;     (VElement. type
;;                (reduce-kv (fn [r n v]
;;                             (assoc r n (f n v)))
;;                           {}
;;                           (ve-props vdom))
;;                (map #(map-properties % f)
;;                     (ve-children vdom)))
;;     vdom))

;; (defn map-values [vdom f]
;;   (map-properties vdom
;;                   (fn [n v]
;;                     (f v))))

(defn velement [type props]
  (assert (map? props))
  (VElement. type props))

(defn velement? [v]
  (instance? VElement v))

(defn memoize-1 [f]
  (let [mem (atom nil)]
    (fn [& args]
      (let [v @mem]
        (if (and v (= (first v) args))
          (second v)
          (let [v (apply f args)]
            (reset! mem [args v])
            v))))))

(defn memoize-n [n f]
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

(defn memoize-2 [f]
  (memoize-n 2 f))


(defn- comp-fs [fs]
  ;; Note: last one in list will be applied first
  (apply comp
         (map (fn [[f args]]
                (fn [e]
                  (when (some? e)
                    (apply f e args))))
              fs)))

(defprotocol ITransformable
  (-update-fs [this f])
  (-get-fs [this]))

(defrecord EventHandler [fs]
  ;; directly execute as a function; useful for testing.
  #?@(:clj [clojure.lang.IFn
            (invoke [this e]
                    ((comp-fs fs) e))])
  #?@(:cljs [IFn
             (-invoke [this e]
                      ((comp-fs fs) e))])
  ITransformable
  (-get-fs [this] fs)
  (-update-fs [this f]
    (update this :fs f)))

(defn event-handler
  ([] (EventHandler. []))
  ([f & args]
   (EventHandler. [[f args]])))

(defn event-handler? [v]
  (instance? EventHandler v))

(defn create-js-event-handler [h dispatch!]
  (assert (event-handler? h))
  (comp (if dispatch!
          (fn [e] (when (some? e)
                    (dispatch! e)))
          (constantly nil))
        (comp-fs (:fs h))))

(comment TODO put somewhere
         "Turns `f` into an event handler. The DOM event is passed to `f`,
  and if it returns non-nil, that value will be passed to the
  `:dispatch!` function from the options passed to [[patch-children!]]
  if defined. Another difference between raw functions and handlers
  is, that handlers can be chained in a referentially transparent
  way. See [[comp-handlers]]."
         )

(defrecord Transformer [fs] ;; an arrow, or sort of..
  #?@(:clj [clojure.lang.IFn
            (invoke [this e]
                    ((comp-fs fs) e))])
  #?@(:cljs [IFn
             (-invoke [this e]
                      ((comp-fs fs) e))])
  ITransformable
  (-get-fs [this] fs)
  (-update-fs [this f]
    (update this :fs f)))

(defn transformer
  [f & args]
  (Transformer. [[f args]]))

(defn trans->
  "Compose several transformers into one, where the first one is
  applied first. Also, the result has the same type as the first. Note
  that this is referentially transparent, meaning that composing the
  same transformers again is `=`."
  [h0 & hs]
  (assert (every? #(satisfies? ITransformable %) (cons h0 hs)))
  (-update-fs h0
              (fn [fs0]
                ;; TODO could allow arbitrary IFns in hs for simple cases?
                (concat (mapcat -get-fs (reverse hs)) fs0))))

(defn comp-transformers
  "Compose several transformers into one, where the last one is
  applied first. Note that this is referentially transparent, meaning
  that composing the same transformers again is `=`."
  [h0 & hs]
  (apply trans-> (reverse (cons h0 hs))))

;; Some standard handlers

(def ^{:doc "Returns transformer, that returns the constant value `v`."
       :arglists '([v])}
  const
  (let [f (fn [_ v] v)]
    (fn [v]
      (transformer f v))))

(def ident (transformer identity))

(defrecord Tagged [value tag])

(defn tagged [tag v]
  (Tagged. v tag))

(defn tagged? [v]
  (instance? Tagged v))

(defn tag
  "Returns a transformer that wraps any value as a [[tagged]] value with the given arbitrary value `t` as the tag."
  [t]
  (transformer ->Tagged t))

(defn untag "Returns a tuple `[tag value]` if `v` is a [[tagged]] value, or `[v nil]` otherwise."
  [v]
  (if (tagged? v)
    [(:tag v) (:value v)]
    [v nil]))
