(ns orpheus.core)

(defrecord VElement [type props])

(def ^:no-doc ve-type :type)
(def ^:no-doc ve-props :props)

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


