(ns dfrese.orpheus.patch.lifting
  "Read a real dom structure into a virtual dom tree."
  (:require [dfrese.orpheus.core :as core]
            [dfrese.edomus.core :as dom]))

(declare lift-properties)

(defn lift-node [parent node]
  (cond
    (dom/element? node)
    ;; TODO pick up 'is' property into options if it's there.
    (let [[state props] (lift-properties node)]
      [state (core/velement (core/element-type (dom/get-property node "namespaceURI") (dom/get-property node "tagName"))
                            props)])
    
    (dom/text-node? node)
    [nil (dom/text-node-value node)]

    :else
    (do
      (dom/remove-child! parent node)
      nil)))

(defn lift-children [parent nodes]
  ;; Note: must be non-lazy
  (doall (remove nil?
                 (map (partial lift-node parent) nodes))))

(defn lift-properties "TODO" [element]
  ;; TODO: try to lift more? for now the basic structure of nodes is helpful already
  (let [lifted (lift-children element (dom/child-nodes element))]
    ;; Note: lift-children may modify the dom, removing some children.
    (assert (= (count lifted) (count (dom/child-nodes element))))
    [(vec (map first lifted))
     {"childNodes" (vec (map second lifted))}]))
