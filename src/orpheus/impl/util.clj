(ns orpheus.impl.util)

(defmacro deftag [name ns]
  (let [doc (str "Returns a virtual `" (str name) "` element for an optional property map and children.")
        m {:doc doc
           :arglists ''([props & children] [& children])}]
    `(def 
       ~(vary-meta name merge m)
       (partial orpheus.core/h (orpheus.core/element-type ~ns ~(str name) nil)))))
