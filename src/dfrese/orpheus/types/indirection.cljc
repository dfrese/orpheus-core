(ns dfrese.orpheus.types.indirection)

;; function type components:

(defrecord ^:no-doc IndirectionType [f])

(defn indirection-type [f]
  (IndirectionType. f))

(defn indirection-type?
  "Return if v is an IndirectionType."
  [v]
  (instance? IndirectionType v))

(defn expand-indirection
  "Returns a different velement object, that this type and properties stand for."
  [this props]
  (apply (:f this) props))

