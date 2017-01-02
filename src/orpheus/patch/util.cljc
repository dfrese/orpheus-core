(ns orpheus.patch.util)

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

(defn diff-patch [init olds news similar? equal? append insert-before update remove]
  (let [olds (vec?! olds)
        news (vec?! news)
        ocnt (count olds)
        ncnt (count news)
        replace (fn replace [v n o]
                  (-> v
                      (remove o)
                      (append n)))]
    (loop [opos 0
           npos 0
           v init]
      (cond
        ;; at and of news, remove all remaining olds
        (= npos ncnt)
        (reduce remove
                v
                (subvec olds opos ocnt))

        ;; at end of olds, append all remaining news
        (= opos ocnt)
        (reduce append
                v
                (subvec news npos ncnt))

        :else
        (let [o (nth olds opos)
              orest (- ocnt opos 1)
              nrest (- ncnt npos 1)
              n (nth news npos)
              are-similar? (similar? o n) ;; quite fast usually, use it
              ]
          (cond
            ;; equals? go on
            (and are-similar? ;; try to avoid to call equal? if we already know something..
                 (equal? o n))
            (recur (inc opos) (inc npos)
                   v)

            ;; if remaining news is longer than olds, prefer an insert over an update
            (> nrest orest)
            (recur opos (inc npos)
                   (insert-before v n o))

            ;; if remaining olds is longer than nows, prefer remove old
            (> orest nrest)
            (recur (inc opos) npos
                   (remove v o))

            ;; equal size and updateable? (dives into subtrees)
            are-similar?
            (recur (inc opos) (inc npos)
                   (update v o n))

            ;; otherwise, insert new, remove old, or both
            :else
            (if (= orest 0)
              ;; last elements of both lists, but not even similar:
              (replace v n o)
              (let [next-o (nth olds (inc opos))]
                (cond
                  ;; new one equal to next old, remove old and skip
                  (equal? next-o n)
                  (recur (+ 2 opos) (inc npos)
                         (remove v o))

                  ;; old one equal to next new, insert new before old, and skip
                  (equal? o (nth news (inc npos)))
                  (recur (inc opos) (+ 2 npos)
                         (insert-before v n o))

                  ;; otherwise just remove old and insert new
                  :else
                  (recur (inc opos) (inc npos)
                         (-> (insert-before v n o)
                             (remove o))))))
            
            ))))))
