(ns dfrese.orpheus.test-utils
  (:require [dfrese.orpheus.lift :as lift]
            #?@(:cljs [[dfrese.edomus.browser :as dom-impl]])
            #?@(:clj [[dfrese.edomus.virtual :as dom-impl]])
            [dfrese.edomus.core :as dom]))

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
