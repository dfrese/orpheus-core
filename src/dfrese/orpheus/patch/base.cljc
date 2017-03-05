(ns dfrese.orpheus.patch.base
  "Functions to apply a virtual dom to a real dom."
  (:require [dfrese.orpheus.core :as core]
            [dfrese.orpheus.patch.util :as util]
            [dfrese.edomus.core :as dom]
            [dfrese.edomus.event :as dom-event]
            [clojure.set :as set]
            [clojure.string :as str]))

(declare patch-children!)
(declare patch-properties!)
(declare init-children!)

;; properties

(def ^:no-doc event-type-re #"(?i)on(.*)")
(def ^:no-doc event-type-capture-re #"(?i)on(.*)capture")

(defn ^:no-doc event-type-name? [s]
  (if-let [[_ name] (re-matches event-type-re s)]
    name
    nil))

(defn ^:no-doc event-type? [s]
  (if (and (> (count s) 2) ;; optimze a little with a quick preliminary test.
           (= "on" (subs s 0 2)))
    (if-let [[_ name] (re-matches event-type-capture-re s)]
      (dom-event/event-type name true)
      (if-let [name (event-type-name? s)]
        (dom-event/event-type name false)
        nil))
    nil))

(defn ^:no-doc set-simple-property! [element name value options]
  (if-let [etype (and (or (nil? value) ;; nil for removal :-/ TODO: look at old-v if it's an handler instead?
                          (ifn? value))
                      (event-type? name))]
    (do
      ;; set event handlers as a side effect, unfortunately; but the
      ;; properties 'onclick' etc., cannot be called propertly, for
      ;; custom event triggering (at least I did not find out). Onle
      ;; the 'addEventListener' handlers can be called properly via
      ;; dispatchEvent - so we map them here.
      (if (nil? value)
        (dom-event/unset-event-handler! element etype)
        (let [f (core/create-js-event-handler value (:dispatch! options))]
          (dom-event/set-event-handler! element etype f)))
      nil)
    ;; any other prop
    (dom/set-property! element name value)))

(defn ^:no-doc patch-style! [element old-v new-v]
  (util/patch-map-simple nil old-v new-v
                         #(dom/remove-style! element %2)
                         #(dom/set-style! element %2 %3)))

(defn ^:no-doc init-style! [element v]
  (reduce-kv #(dom/set-style! element %2 %3)
             nil
             v))

(defn ^:no-doc patch-classes! [element old-v new-v]
  (let [o (set old-v) ;; should be sets already for optimal performance
        n (set new-v)]
    (doseq [c (set/difference o n)]
      (dom/remove-class! element c))
    (doseq [c (set/difference n o)]
      (dom/add-class! element c))))

(defn ^:no-doc init-classes! [element v]
  (dom/set-classes! element v))

(defn ^:no-doc patch-attributes! [element old-v new-v]
  (util/patch-map-simple nil old-v new-v
                         #(dom/remove-attribute! element %2)
                         #(dom/set-attribute! element %2 %3)))

(defn ^:no-doc init-attributes! [element v]
  (reduce-kv #(dom/set-attribute! element %2 %3)
             nil
             v))

(defn ^:no-doc patch-property! [element name old-v new-v document options]
  (if (identical? old-v new-v) ;; doing a = is probably not worth it
    nil
    (case name
      "childNodes" (patch-children! element old-v new-v document options)
      "style" (patch-style! element old-v new-v)
      "attributes" (patch-attributes! element old-v new-v)
      "classList" (patch-classes! element old-v new-v)
      (if (not= old-v new-v) ;; need to because of IConvertible
        (set-simple-property! element name new-v options)
        nil))))

(defn ^:no-doc init-property! [element name value document options]
  ;; Note: value can always be nil; mening to remove the property 'as much as possible'.
  (case name
    "childNodes" (init-children! element value document options)
    "style" (init-style! element value)
    "attributes" (init-attributes! element value)
    "classList" (init-classes! element (set value))
    (set-simple-property! element name value options)))

(defn ^:no-doc remove-property! [element name]
  ;; to actively remove a property that was set before (in constrast to leaving it totally unchanged):
  (let [options nil ;; setting to nil should go without options
        document nil] 
    (case name
      "childNodes" (do ;; we actually need to do both - TODO explain why
                     ;; TODO: add a clear-children to dom?
                     (doseq [c (dom/child-nodes element)]
                       (dom/remove-child! element c))
                     (init-property! element "childNodes" [] document options))
      (init-property! element name nil document options))))

(defn ^:no-doc patch-properties! [element old-props new-props document options]
  (util/patch-map nil
                  old-props
                  new-props
                  #(remove-property! element %2)
                  #(init-property! element %2 %3 document options)
                  #(patch-property! element %2 %3 %4 document options)))

;; children

(defn ^:no-doc set-props! ;; need not be async..
  [element props document options]
  (reduce-kv (fn [_ k v]
               (init-property! element k v document options))
             nil
             props))

(defn ^:no-doc create-child [document vdom options]
  (loop [vdom vdom
         options options]
    (cond
      (core/velement? vdom)
      (let [type (core/ve-type vdom)
            props (core/ve-props vdom)]
        (cond
          (core/element-type? type)
          (let [e (core/create-element-node document type)]
            (set-props! e props document options)
            ;; Note: and/or wait for the async success? ("mounted")
            e)

          (core/indirection-type? type)
          (recur (core/expand-indirection type (core/ve-props vdom))
                 options)

          (core/leaf-type? type)
          (core/leaf-type-create type props options)

          :else
          (throw (ex-info (str "Unsupport velement type: " (pr-str type) ".") {}))))
      (core/with-context-update? vdom)
      (recur (:content vdom) ((:update-options vdom) options))

      (string? vdom) ;; calling str would hide a lot of errors; and the user should explicitly do it.
      (dom/create-text-node document vdom)
      
      :else
      (throw (ex-info (str "Unsupported vdom element: " (pr-str vdom) ".") {:value vdom})))))

;; ----

(defn ^:no-doc alter-child! [document options node old-vdom new-vdom]
  (loop [old-vdom old-vdom
         new-vdom new-vdom
         options options]
    (cond
      (core/velement? old-vdom)
      ;; update an element of same type.
      (do
        (assert (core/velement? new-vdom))
        (let [type (core/ve-type old-vdom)
              old-props (core/ve-props old-vdom)
              new-props (core/ve-props new-vdom)]
          (when-not (dom/element? node)
            (throw (ex-info (str "Actual node is not an element, where the previous vdom is: " (pr-str old-vdom) ", " node ".") {})))
          (assert (= (core/ve-type old-vdom) (core/ve-type new-vdom))) ;; impl error
          (cond
            (core/element-type? type)
            (let [old-props (core/ve-props old-vdom)
                  new-props (core/ve-props new-vdom)]
              (patch-properties! node
                                 old-props new-props
                                 document
                                 options)
              nil)
        
            (core/indirection-type? type)
            (recur (core/expand-indirection type old-props) (core/expand-indirection type new-props)
                   options)

            (core/leaf-type? type)
            (do (core/leaf-type-patch! type node old-props new-props options)
                nil)
          
            :else
            (throw (ex-info (str "Unsupport velement type: " (pr-str type) ".") {})))))

      (core/with-context-update? old-vdom)
      (do
        ;; TODO: keep abstraction...
        (assert (core/with-context-update? new-vdom))
        (assert (= (:update-options old-vdom) (:update-options new-vdom)))
        (recur (:content old-vdom) (:content new-vdom)
               ((:update-options new-vdom) options)))
      
      (string? old-vdom)
      ;; update text of a textnode to new-vdom (a string or anything else)
      (do
        (assert (string? new-vdom))
        (when-not (dom/text-node? node)
          (throw (ex-info (str "Actual node is not a text node, where the previous vdom is: " (pr-str old-vdom) ", " node ".") {})))
        (dom/set-text-node-value! node new-vdom)
        nil)
      
      :else
      (throw (ex-info (str "Unsupported vdom element: " (pr-str old-vdom) ".") {:value old-vdom})))))

(defn ^:no-doc destroy-node! [options node vdom]
  (when (and (core/velement? vdom)
             (core/leaf-type? (core/ve-type vdom)))
    (core/leaf-type-destroy! (core/ve-type vdom) node (core/ve-props vdom) options)))

(defn ^:no-doc remove-child! [options element node vdom]
  (destroy-node! options node vdom)
  (dom/remove-child! element node))

(defn ^:no-doc insert-child! [document options element vdom ref-node]
  (let [node (create-child document vdom options)]
    (dom/insert-before! element node ref-node)))

(defn ^:no-doc append-child! [document options element vdom]
  (let [node (create-child document vdom options)]
    (dom/append-child! element node)))

(defn ^:no-doc init-children! [element vdoms document options]
  (doseq [c vdoms]
    (dom/append-child! element (create-child document c options))))

(defn ^:no-doc similar-vdom?
  "Aka 'updateable' element"
  [vdom1 vdom2]
  (cond
    (identical? vdom1 vdom2) true
    
    (core/velement? vdom1)
    (and (core/velement? vdom2)
         (= (core/ve-type vdom1) (core/ve-type vdom2)))

    (core/with-context-update? vdom1)
    (and (core/with-context-update? vdom2)
         ;; if the context-change changes, we recreate the tree (it might be bound in event-handlers)
         (= (:update-options vdom1) (:update-options vdom2)))

    (string? vdom1) (string? vdom2)
    
    :else
    (throw (ex-info (str "Unsupported vdom element: " (pr-str vdom1) ".") {:value vdom1}))))

(def indices
  (let [N 1000
        reservoir (vec (range N))]
    (fn [n]
      (if (<= n N)
        (subvec reservoir 0 n)
        (vec (range n))))))

(defn- node-name [node]
  ;; TODO -> edomus?
  #?(:clj node)
  #?(:cljs (.-nodeName node)))

(defn ^:no-doc patch-children! [element old-vdoms new-vdoms document options]
  (if (identical? old-vdoms new-vdoms) ;; ..cheap shortcut
    nil
    (let [nodes (dom/child-nodes element)
          olds (zipmap nodes old-vdoms)]
      
      (when (not= (count nodes) (count old-vdoms))
        (throw (ex-info (str "Actual dom child nodes do not match the number of vdom elements: " (pr-str old-vdoms) " /= "
                             (pr-str (map node-name nodes)) ".") {})))

      (util/fold-diff-patch-keyed element
                                  (fn append [element node]
                                    (dom/append-child! element node)
                                    element)
                                  (fn remove [element node]
                                    (dom/remove-child! element node)
                                    element)
                                  (fn patch [element node new-vdom]
                                    (let [old-vdom (get olds node)]
                                      (when (or (not (identical? old-vdom new-vdom))
                                                (not= old-vdom new-vdom))
                                        (alter-child! document options node old-vdom new-vdom)))
                                    element)
                                  nodes
                                  new-vdoms
                                  (fn patchable? [node new-vdom]
                                    (similar-vdom? (get olds node) new-vdom))
                                  (fn old-key [node]
                                    (let [vdom (get olds node)]
                                      (when (core/velement? vdom)
                                        (core/ve-key vdom))))
                                  (fn new-key [vdom]
                                    (when (core/velement? vdom)
                                      (core/ve-key vdom)))
                                  (fn create [vdom]
                                    (create-child document vdom options))
                                  (fn destroy! [node]
                                    (destroy-node! options node (get olds node))))
      (assert (= (count (dom/child-nodes element)) (count new-vdoms)) (str "Internal error; new vdoms:" (pr-str new-vdoms)))
      nil)))
