(ns dfrese.orpheus.patch.reservoirs
  (:require [dfrese.orpheus.patch.indices :as i]))

(deftype TRes [types v ^:mutable tsize indices])

(defn type-reservoir-init [types]
  (assert (vector? types))
  ;; TODO: optimize with a native array?
  (TRes. types
         (transient (vec (repeat (count types) false)))
         (transient (zipmap types (repeat 0))) ;; TODO: a map that uses identical? on keys?
         (i/indices (count types))))

(defn- type-reservoir-count! [tr type f]
  (assoc! (.-tsize tr) type (f (get (.-tsize tr) type))))

(defn type-reservoir-push! [tr idx]
  (assert (not (get (.-v tr) idx)))
  (type-reservoir-count! tr (get (.-types tr) idx) inc)
  (assoc! (.-v tr) idx true))

(defn type-reservoir-seq [tr]
  (let [sizes (persistent! (.-tsize tr))]
    (if (every? zero? (vals sizes))
      nil
      (filter #(get (.-v tr) %)
              (.-indices tr)))))

(defn- type-reservoir-remove! [tr type idx]
  (assert (get (.-v tr) idx))
  (type-reservoir-count! tr type dec)
  (assoc! (.-v tr) idx false))

(defn type-reservoir-pull! [tr type]
  (if-let [i (and (not (zero? (get (.-tsize tr) type))) ;; worth it to search?
                  (some (fn pull [i]
                          (and (get (.-v tr) i)
                               (identical? ((.-types tr) i) type)
                               i))
                        (.-indices tr)))]
    (do (type-reservoir-remove! tr type i)
        i)
    nil))


(defn key-reservoir-init []
  (transient (array-map)))

(defn key-reservoir-pull! [kr key]
  (if-let [old (get kr key)]
    (do
      (dissoc! kr key)
      old)
    nil))

(defn key-reservoir-push! [kr k v]
  (when (contains? kr k)
    (throw (ex-info (str "Duplicate key: " k ".") {:value k})))
  (assoc! kr k v))

(defn key-reservoir-seq [kr]
  (vals (persistent! kr)))
