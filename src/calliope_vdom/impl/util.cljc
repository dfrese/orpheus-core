(ns calliope-vdom.impl.util)

(def ^:private undef ::undef)

(defn patch-map [v old new dissoc assoc patch]
  (as-> v $
    (reduce-kv (fn [v name x]
                 (let [n (get new name undef)]
                   (cond
                     (= n undef) (dissoc v name)
                     ;; left up to caller if worth it?: (= n x)
                     :else (patch v name x n))))
               $
               old)
    (reduce-kv (fn [v name x]
                 (if (contains? old name)
                   v ;; handled above
                   (assoc v name x)))
               $
               new)))

(defn patch-map-simple [v old new dissoc assoc]
  (patch-map v old new
             dissoc
             assoc
             (fn [v name o n]
               (if (not= o n)
                 (assoc v name n)
                 v))))

(defn diff-patch [init olds news similar? insert update remove]
  (let [append (fn [v n]
                 (insert v n nil))
        equal? =] ;; maybe identical? is a better compromise?
    (loop [olds olds
           news news
           v init]
      (cond
        (empty? news)
        (reduce remove
                v
                olds)

        (empty? olds)
        (reduce insert
                v
                news)

        (equal? (first olds) (first news))
        (recur (rest olds) (rest news) v)

        (similar? (first olds) (first news))
        (recur (rest olds) (rest news)
               (update v (first olds) (first news)))

        ;; TODO: a little more...
        :else
        (recur olds (rest news)
               (insert v (first news) (first olds)))))))
