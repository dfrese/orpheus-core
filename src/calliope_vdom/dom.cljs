(ns calliope-vdom.dom
  "Functions to apply a virtual dom to a real dom."
  (:require [calliope-vdom.core :as core]
            [calliope-dom.core :as dom]
            [calliope-dom.batch :as dom-batch]
            [calliope-dom.properties.children :as cs]
            [calliope-vdom.extension :as ext]
            [calliope-vdom.impl.util :as util]))

(defn flush! []
  (dom-batch/flush!))

;; TODO: generalize this over:
;; create-element!/createTextNode
;; alter-element!, insert/remove child etc.

(declare patch-children)
(declare patch-properties)

(defn ^:no-doc patch! [node old-props new-props old-children new-children options]
  (dom/alter-element! ;; batch
   node
   (fn [element-map]
     (-> element-map
         (patch-properties old-props new-props options)
         (patch-children old-children new-children options)))))


(defn patch-children!
  "TODO This is the main public fn..."
  ([element old-vdoms new-vdoms]
   (patch-children! element old-vdoms new-vdoms nil))
  ([element old-vdoms new-vdoms options]
   (dom/alter-element! element ;; batch
    patch-children
    old-vdoms new-vdoms options)))

(defn- to-text [v] (str v)) ;; for everything but the VElements in children

;; properties

(defn ^:no-doc set-simple-property [element-map name value options]
  (assoc element-map name
         (if (satisfies? ext/IConvertible value)
           (ext/-convert value options)
           value)))

(defn ^:no-doc patch-style [style old-v new-v]
  (util/patch-map-simple style
                         old-v
                         new-v
                         dissoc
                         assoc))

(defn ^:no-doc patch-property [element-map name old-v new-v options]
  (if (= old-v new-v)
    element-map
    (case name
      "style" (update element-map "style" patch-style old-v new-v)
      ;;"attributes" (patch-dom-attributes element-map old-v new-v)
      ;;"classList" ...
      (set-simple-property element-map name new-v options))))

(defn ^:no-doc set-property [element-map name value options]
  (case name
    "style" (update element-map "style" patch-style {} value)
    ;;"attributes" (patch-dom-attributes element-map {} value)
    ;;"classList" ...
    (set-simple-property element-map name value options)))

(defn ^:no-doc patch-properties [element-map old-props new-props options]
  (util/patch-map element-map
                  old-props
                  new-props
                  dissoc
                  #(set-property %1 %2 %3 options)
                  #(patch-property %1 %2 %3 %4 options)))

;; children

(defn create-child [document vdom options]
  (if (instance? core/VElement vdom)
    (doto (dom/create-element document (core/ve-type vdom)) ;; TODO: allow +ns
      ;; not batched here!
      (dom/alter-element! (fn [element-map]
                            (as-> element-map $
                              (reduce-kv #(set-property %1 %2 %3 options)
                                         $
                                         (core/ve-props vdom))
                              (update $ "childNodes"
                                      (fn [child-vec]
                                        (reduce (fn [child-vec cvdom]
                                                  (cs/append-child child-vec
                                                                   (create-child document cvdom options)))
                                                child-vec
                                                (core/ve-children vdom))))))))
    (.createTextNode document (to-text vdom))))

;; ----

(defn ^:no-doc alter-child! [options children [node old-vdom] new-vdom]
  (if (instance? core/VElement old-vdom)
    ;; update an element of same type.
    (do
      (assert (instance? js/Element node)) ;; TODO: helpful message
      (assert (= (core/ve-type old-vdom) (core/ve-type new-vdom)))
      (patch! node
              (core/ve-props old-vdom) (core/ve-props new-vdom)
              (core/ve-children old-vdom) (core/ve-children new-vdom)
              options))
    ;; update text of a textnode to new-vdom (a string or anything else)
    (do
      (assert (instance? js/Text node)) ;; TODO: helpful message
      (set! (.-nodeValue node) (to-text new-vdom))))
  children)

(defn ^:no-doc remove-child [children [node _]]
  (cs/remove-child children node))

(defn ^:no-doc insert-child [doc options children new-vdom & [[ref-node _]]]
  ;; ref may be nil, which will append n
  (let [node (create-child doc new-vdom options)]
    (cs/insert-before children node ref-node)))

(defn- similar-velement?
  [e1 e2]
  (= (core/ve-type e1) (core/ve-type e2)))

(defn- similar-vdom?
  "Aka 'updateable' element"
  [vdom1 vdom2]
  (if (instance? core/VElement vdom1)
    (and (instance? core/VElement vdom2)
         (similar-velement? vdom1 vdom2))
    (not (instance? core/VElement vdom2))))

(defn patch-children [element-map old-vdoms new-vdoms options]
  (update element-map "childNodes"
          (fn [child-vec]
            (let [doc (get element-map "ownerDocument")]
              (util/diff-patch child-vec
                               ;; we need the node and the previous vnode for updates:
                               (map vector
                                    child-vec
                                    old-vdoms)
                               new-vdoms
                   
                               #(similar-vdom? (second %1) %2) ;; if true, alter-child! is used

                               (partial insert-child doc options)
                               (partial alter-child! options)
                               remove-child)))))
