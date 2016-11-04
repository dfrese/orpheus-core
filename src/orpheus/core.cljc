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

(defprotocol IEventHandler
  (-to-js-event-handler [this options]))

(defrecord Transformer [fs]
  ;; directly execute as a function; useful for testing.
  #?@(:clj [clojure.lang.IFn
            (invoke [this e]
                    ((comp-fs fs) e))])
  #?@(:cljs [IFn
             (-invoke [this e]
                      ((comp-fs fs) e))])
  
  IEventHandler
  (-to-js-event-handler [this options]
    (comp (if-let [dispatch! (:dispatch! options)]
            (fn [e] (when (some? e)
                      (dispatch! e)))
            (constantly nil))
          (comp-fs fs))))

(comment TODO put somewhere
         "Turns `f` into an event handler. The DOM event is passed to `f`,
  and if it returns non-nil, that value will be passed to the
  `:dispatch!` function from the options passed to [[patch-children!]]
  if defined. Another difference between raw functions and handlers
  is, that handlers can be chained in a referentially transparent
  way. See [[comp-handlers]]."
         )

(defn transformer
  [f & args]
  (Transformer. [[f args]]))

(defn comp-transformers
  "Chain several handlers into one. This is referentially transparent,
  meaning that composing the same handlers again is `=`."
  [h0 & hs]
  (Transformer. (apply mapcat #(.-fs %) h0 hs)))

(defn trans-> [& hs]
  (comp-transformers (reverse hs)))

;; Some standard handlers

(def ^{:doc "A transformer returning a constant value."}
  const
  (let [f (fn [_ v] v)]
    (fn [msg]
      (transformer f msg))))

(def ident (transformer identity))

(def ^{:doc "A transformer that wraps any value in a tuple `[id v]`."}
  tag
  (let [f (fn [v id]
            [id v])]
    (fn [id]
      (transformer f id))))
