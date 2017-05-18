(ns dfrese.orpheus.patch.util)

(def ^:private undef ::undef)

(defn patch-map [v old new dissoc assoc patch]
  (if (identical? old new) ;; ..cheap shortcut
    v
    (as-> v $
      ;; remove or patch what's in old.
      (reduce-kv (fn patch-map-remove [v name x]
                   (let [n (get new name undef)]
                     (cond
                       (identical? n undef) (dissoc v name)
                       :else (patch v name x n))))
                 $
                 old)
      (reduce-kv (fn patch-map-add [v name x]
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
               (fn patch-map-simple-patch [v name o n]
                 (if (not= o n)
                   (assoc v name n)
                   v)))))

(defn vec!? [v]
  ;; surprisingly, cljs does not do this??!!
  (if (vector? v)
    v
    (vec v)))

;;(def _iterator #?(:cljs iter) #?(:clj iterator))

(defn reduce-i
  "= (reduce f init (range start end step))"
  [f init start end step]
  (if (= start end)
    init
    (recur f (f init start) (+ start step) end step)))

(defn fold-diff-patch-v1 [init append remove insert patch old-cnt news patchable? precious?]
  (loop [old-i 0
         news (seq news)
         res init]
    (if (or (= old-i old-cnt)
            (not news))
      ;; stop if no news left (remove all olds), or olds remain (append all news):
      (as-> res $
        (reduce-i remove $ (dec old-cnt) (dec old-i) -1)
        (reduce append $ news))
      (let [folds old-i
            fnews (first news)]
        (cond
          ;; patch if patchable
          (patchable? folds fnews)
          (recur (inc old-i)
                 (next news)
                 (patch res folds fnews))

          ;; keep old if it's worth it
          (precious? folds)
          (recur old-i
                 (next news)
                 (insert res fnews folds))

          ;; otherwise, remove old.
          :else
          (recur (inc old-i)
                 news
                 (remove res folds)))))
    ))

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


