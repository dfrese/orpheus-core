(ns calliope-vdom.extension)

(defprotocol IConvertible
  (-convert [this options]))

#_(defprotocol IPatchable
  (-patch [this old-v dom-value])
  )
