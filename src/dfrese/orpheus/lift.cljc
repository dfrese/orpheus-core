(ns dfrese.orpheus.lift
  "Read a real dom structure into a virtual dom tree."
  (:require [dfrese.orpheus.core :as core]
            [dfrese.edomus.core :as dom]))

(declare lift-properties)

(defn- lift-node [parent node]
  (cond
    (dom/element? node)
    ;; TODO pick up 'is' property into options if it's there.
    (core/velement (core/element-type (dom/get-property node "namespaceURI") (dom/get-property node "tagName"))
                   (lift-properties node))
    
    (dom/text-node? node)
    (dom/text-node-value node)

    :else
    (do
      (dom/remove-child! parent node)
      nil)))

(defn- lift-children [parent nodes]
  ;; Note: must be non-lazy
  (doall (remove nil?
                 (map (partial lift-node parent) nodes))))

(defn lift-properties "TODO" [element]
  ;; TODO: try to lift more? for now the basic structure of nodes is helpful already
  (let [lifted (lift-children element (dom/child-nodes element))]
    ;; Note: lift-children may modify the dom, removing some children.
    (assert (= (count lifted) (count (dom/child-nodes element))))
    {"childNodes" (vec lifted)}))
