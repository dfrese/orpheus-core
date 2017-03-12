(ns dfrese.orpheus.patch.indices)

(def indices
  (let [N 10000
        reservoir (vec (range N))]
    (fn [n]
      (if (<= n N)
        (subvec reservoir 0 n)
        (vec (range n))))))

