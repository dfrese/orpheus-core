(ns dfrese.orpheus.types.element
  (:require [dfrese.orpheus.types :as core]
            [dfrese.edomus.core :as dom]
            [clojure.string :as string]))

(defn- is-props? [arg0]
  (and (or (nil? arg0) (map? arg0))
       (not (core/velement? arg0))))

(defn- arg-props [t arg0]
  (if t
    arg0
    {}))

(defn- arg-children [t arg0 args]
  (if t
    args
    (cons arg0 args)))

(defn ^:no-doc h
  ([type] (core/velement type {}))
  ([type arg0 & args]
   (let [t (is-props? arg0)
         props (arg-props t arg0)
         children (arg-children t arg0 args)]
     (assert (map? props) (str "Props must be a map, not: " (pr-str props)))
     (if (or (contains? props "childNodes")
             (contains? props :childNodes))
       (do
         (assert (empty? children)
                 "Specify the child nodes either as a property, or as the argument list, but not both.")
         (core/velement type props))
       (core/velement type (cond-> props
                             ;; childNodes as a vector greatly helps patching.
                             (not-empty children) (assoc "childNodes" (vec children))))))))

(defrecord ^:no-doc ElementType
  [ns name options]
  #?@(:cljs [IFn
             (-invoke [this & args] (apply h this args))])
  #?@(:clj [clojure.lang.IFn
            (invoke [this] (h this))
            (invoke [this a1] (h this a1))
            (invoke [this a1 a2] (h this a1 a2))
            (invoke [this a1 a2 a3] (h this a1 a2 a3))
            (invoke [this a1 a2 a3 a4] (h this a1 a2 a3 a4))
            (invoke [this a1 a2 a3 a4 a5] (h this a1 a2 a3 a4 a5))
            (invoke [this a1 a2 a3 a4 a5 a6] (h this a1 a2 a3 a4 a5 a6))
            (invoke [this a1 a2 a3 a4 a5 a6 a7] (h this a1 a2 a3 a4 a5 a6 a7))
            (invoke [this a1 a2 a3 a4 a5 a6 a7 a8] (h this a1 a2 a3 a4 a5 a6 a7 a8))
            (invoke [this a1 a2 a3 a4 a5 a6 a7 a8 a9] (h this a1 a2 a3 a4 a5 a6 a7 a8 a9))
            (invoke [this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10] (h this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10))
            (invoke [this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11] (h this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11))
            (invoke [this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12] (h this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12))
            (invoke [this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13] (h this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13))
            (invoke [this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14] (h this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14))
            (invoke [this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15] (h this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15))
            (invoke [this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16] (h this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16))
            (invoke [this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17] (h this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17))
            (invoke [this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18] (h this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18))
            (invoke [this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19] (h this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19))
            (invoke [this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20] (h this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20))
            (invoke [this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21] (h this a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21))
            (applyTo [this args] (apply h this args))]))

(defn element-type?
  "Return if v is an ElementType."
  [v]
  (instance? ElementType v))

(defn ^:no-doc create-element-node [document type]
  (dom/create-element-ns document (:ns type) (:name type) (:options type)))

(defn element-type
  "Returns a velement type for dom elements, given a node type string,
  and optionally a namespace and options."
  ;; Note: lowercase is at least the standard for html..
  ([ns name]
   (element-type ns name nil))
  ([ns name options]
   (ElementType. ns (string/lower-case name) options)))

#?(:clj
   (defmacro deftag [name ns]
     (let [doc (str "Returns a virtual `" (str name) "` element for an optional property map and children.")
           m {:doc doc
              :arglists ''([props & children] [& children])}]
       `(def 
          ~(vary-meta name merge m)
          (dfrese.orpheus.types.element/element-type ~ns ~(str name) nil)))))
