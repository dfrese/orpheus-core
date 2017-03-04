(ns dfrese.orpheus.test-utils
  (:require [dfrese.orpheus.lift :as lift]
            #?@(:cljs [[dfrese.edomus.browser :as dom-impl]])
            #?@(:clj [[dfrese.edomus.virtual :as dom-impl]])
            [dfrese.edomus.core :as dom]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.rose-tree :as rose]
            [clojure.test.check.random :as random]
            [dfrese.orpheus.html :as html]))

(defn document []
  #?(:clj (dom-impl/new-document))
  #?(:cljs (let [b (.-body js/document)]
             (doseq [c (vec (array-seq (.-childNodes b)))]
               (.removeChild b c))
             (doseq [a (vec (array-seq (.-attributes b)))]
               (.removeAttribute b a))
             dom-impl/document)))

(defn prepare! []
  (let [doc (document)
        e (dom/create-element doc "div")]
    [e (lift/lift-properties e)]))

;; generators for velements:

(def color (gen/elements ["blue" "red" "grey"]))

(def style
  (gen/bind (gen/tuple (gen/elements [:padding-top :line-height :margin-right nil])
                       
                       (gen/elements [:color :background-color :text-color nil])
                       color)
            (fn [[nst n cst c]]
              (gen/bind (if nst
                          (gen/bind (gen/choose 1 200)
                                    (fn [n]
                                      (gen/return {nst n})))
                          (gen/return {}))
                        (fn [m1]
                          (if cst
                            (gen/bind color
                                      (fn [c]
                                        (gen/return (assoc m1 cst c))))))))))

(def attributes
  (gen/bind (gen/elements [nil "data-test" "width"])
            (fn [n]
              (if n
                (gen/bind gen/string
                          (fn [v]
                            (gen/return {n v})))
                (gen/return {})))))

(defn _props [element]
  (gen/bind (gen/tuple style attributes (gen/vector element))
            (fn [[style attrs children]]
              (gen/return {:childNodes children
                           :style style
                           :attributes attrs}))))

(def element
  (gen/recursive-gen (fn [element]
                       (gen/bind (gen/tuple (gen/elements [html/div html/span html/p])
                                            (_props element))
                                 (fn [[ctor props]]
                                   (gen/return (ctor props)))))
                     gen/string-ascii))

(def props (_props element))

(defn generate [generator size seed]
  (let [rng (random/make-random seed)]
    (rose/root (gen/call-gen generator rng size))))
