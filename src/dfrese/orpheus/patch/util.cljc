(ns dfrese.orpheus.patch.util)

(def ^:private undef ::undef)

(defn patch-map [v old new dissoc assoc patch]
  (if (identical? old new) ;; ..cheap shortcut
    v
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
                 new))))

(defn patch-map-simple [v old new dissoc assoc]
  (if (= old new) ;; these maps are small, so maybe worth it?
    v
    (patch-map v old new
               dissoc
               assoc
               (fn [v name o n]
                 (if (not= o n)
                   (assoc v name n)
                   v)))))

(defn vec?! [v]
  ;; surprisingly, cljs does not do this??!!
  (if (vector? v)
    v
    (vec v)))

(defn fold-diff-patch [init append remove cut patch olds news patchable?]
  (loop [olds olds ;; TODO: apply seq pattern...?
         news news
         res init]
    (cond
      ;; stop if no news left (remove all olds), or olds remain (append all news):
      (or (empty? olds)
          (empty? news))
      (as-> res $
        (cut $ olds)
        (reduce append $ news))

      ;; patch if patchable
      (patchable? (first olds) (first news))
      (recur (rest olds)
             (rest news)
             (patch res (first olds) (first news)))

      :else
      ;; not patchable, remove old (TODO: unless it's 'precious', then insert new)
      (recur (rest olds)
             news
             (remove res (first olds)))
      )))

(defn fold-diff-patch-keyed [init append remove patch olds news patchable? old-key new-key create destroy! resurrect]
  ;; TODO: do we event have to do more for a focused elements? (not touching it at all?)
  (let [reservoir (transient {})
        ;; when removing smth, put it in the reservoir; when appending try to take and patch smth from it.
        res (fold-diff-patch init
                             (fn append' [res new]
                               (let [nkey (new-key new)]
                                 (if-let [old (and nkey (get reservoir nkey))]
                                   (do
                                     (dissoc! reservoir nkey)
                                     ;; could still have same key, but not be patchable.
                                     (if (patchable? old new)
                                       (patch (append res (resurrect old))
                                              old new)
                                       (append res (create new))))
                                   (append res (create new)))))
                             (fn remove' [res old]
                               (if-let [k (old-key old)]
                                 (do
                                   ;; TODO: do smth when key already in reservoir
                                   (assoc! reservoir k old)
                                   (remove res old))
                                 (let [res (remove res old)]
                                   (destroy! old)
                                   res)))
                             (fn cut [res olds]
                               ;; when cutting of remaining olds, we don't need to put it in the reservoir
                               (let [res (reduce remove res (reverse olds))]
                                 (doseq [o olds]
                                   (destroy! o))
                                 res))
                             patch
                             olds
                             news
                             (fn [old new]
                               (and (patchable? old new)
                                    ;; both with no keys is ok too
                                    (= (old-key old) (new-key new)))))]
    ;; what remains, is already removed, by maybe it needs some finalization:
    (doseq [o (vals (persistent! reservoir))]
      (destroy! o))
    res))

