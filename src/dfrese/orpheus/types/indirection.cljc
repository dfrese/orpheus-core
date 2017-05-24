(ns dfrese.orpheus.types.indirection
  "Functions for virtual dom elements of 'function components' (see [[dfrese.orpheus.core/defnc]]).
   The functions in this package are intented for library developers,
  and not needed for basic usage of Orpheus.")

(defrecord ^:no-doc IndirectionType [f])

(defn indirection-type
  "Creates an element type for indirection elements, with a thunk `f`."
  [f]
  (IndirectionType. f))

(defn indirection-type?
  "Return if v is an indirection type."
  [v]
  (instance? IndirectionType v))

(defn expand-indirection
  "Returns a different velement object, that this type and properties stand for."
  [^IndirectionType t props]
  (apply (.-f t) props))
