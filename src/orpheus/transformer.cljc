(ns orpheus.transformer)

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

(defn transformable? [v]
  (satisfies? ITransformable v))

(defn transformed [t]
  (comp-fs (-get-fs t)))

(defn transform [v t]
  ((transformed t) v))

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
  ([] (Transformer. [])) ;; identity
  ([v]
   (if (transformable? v)
     v
     (do
       (assert (ifn? v))
       (Transformer. [[v nil]]))))
  ([f a0 & args]
   (Transformer. [[f (cons a0 args)]])))

(defn- get-fs [v]
  (if (satisfies? ITransformable v)
    (-get-fs v)
    ;; otherwise, also allow functions directly
    (do
      (assert (ifn? v))
      [[v nil]])))

(defn trans->
  "Compose several transformers into one, where the first one is
  applied first. Also, the result has the same type as the first. Note
  that this is referentially transparent, meaning that composing the
  same transformers again is `=`."
  [h0 & hs]
  (-update-fs h0
              (fn [fs0]
                (concat (mapcat get-fs (reverse hs)) fs0))))

(defn comp-transformers
  "Compose several transformers into one, where the last one is
  applied first. Note that this is referentially transparent, meaning
  that composing the same transformers again is `=`."
  [h0 & hs]
  (apply trans-> (reverse (cons h0 hs))))

;; Some standard transformers

(def ^{:doc "Returns transformer, that returns the constant value `v`."
       :arglists '([v])}
  const
  (let [f (fn [_ v] v)]
    (fn [v]
      (transformer f v))))

(def ident (transformer))

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
