(ns dfrese.orpheus.types.foreign)

;; A foreign type could be something like a react component, which can
;; be integrated into the dom, but has special rules for construction
;; and may also participate in the patching, by using it as the type
;; of a velement.
(defprotocol IForeignType
  "A protocol for velement types with special node creation and update
  methods. There are no restrictions on the type of properties, which
  are directly taken from the argument to [[velement]]."
  (foreign-type-create [this document props options] "Create a dom node for this type and props, and return [initial-state node]")
  (foreign-type-patch! [this state node old-props new-props options] "Update the dom node for new props of the same type, and return an updated state.")
  (foreign-type-destroy! [this state node props options] "Clean up the dom node."))

(defn foreign-type?
  "Returns if v is a IForeignType."
  [v]
  (satisfies? IForeignType v))

