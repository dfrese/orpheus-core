(ns dfrese.orpheus.patch.base
  "Functions to apply a virtual dom to a real dom."
  (:require [dfrese.orpheus.core :as core]
            [dfrese.orpheus.patch.util :as util]
            [dfrese.edomus.core :as dom]
            [dfrese.edomus.event :as dom-event]
            [clojure.set :as set]))

(declare patch-children!)
(declare patch-properties!)

;; properties

(def ^:no-doc event-type-re #"(?i)on(.*)")
(def ^:no-doc event-type-capture-re #"(?i)on(.*)capture")

(defn ^:no-doc event-type-name? [s]
  (if-let [[_ name] (re-matches event-type-re s)]
    name
    nil))

(defn ^:no-doc event-type? [s]
  ;; TODO: optimize; probably quite slow.
  (if-let [[_ name] (re-matches event-type-capture-re s)]
    (dom-event/event-type name true)
    (if-let [name (event-type-name? s)]
      (dom-event/event-type name false)
      nil)))

(defn ^:no-doc set-simple-property! [element name value options]
  (if-let [etype (and (or (nil? value) ;; nil for removal :-/ TODO: look at old-v if it's an handler instead
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

(defn ^:no-doc patch-classes! [element old-v new-v]
  (let [o (set old-v) ;; should be sets already for optimal performance
        n (set new-v)]
    (doseq [c (set/difference o n)]
      (dom/remove-class! element c))
    (doseq [c (set/difference n o)]
      (dom/add-class! element c))))

(defn ^:no-doc patch-attributes! [element old-v new-v]
  (util/patch-map-simple nil old-v new-v
                         #(dom/remove-attribute! element %2)
                         #(dom/set-attribute! element %2 %3)))

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
  ;; TODO: optimize (we can act like the property has never been set before)
  (case name
    "childNodes" (patch-children! element [] value document options)
    "style" (patch-style! element {} value)
    "attributes" (patch-attributes! element {} value)
    "classList" (patch-classes! element #{} (set value))
    (set-simple-property! element name value options)))

(defn ^:no-doc remove-property! [element name]
  ;; to actively remove a property that was set before (in constrast to leaving it totally unchanged):
  (let [options nil ;; setting to nil should go without options
        document nil] 
    (case name
      "childNodes" (do ;; we actually need to do both - TODO explain why
                     ;; TODO: add a clear-children to dom
                     (doseq [c (dom/child-nodes element)]
                       (dom/remove-child! element c))
                     (init-property! element "childNodes" [] document options))
      ;; FIXME: also remove all present styles, attributes, classList ?
      (init-property! element name nil document options))))

(defn patch-properties! "TODO" [element old-props new-props document options]
  (util/patch-map nil
                  old-props
                  new-props
                  #(remove-property! element %2)
                  #(init-property! element %2 %3 document options)
                  #(patch-property! element %2 %3 %4 document options)))

;; children

(defn- to-text [v] (str v)) ;; for everything but the VElements in children

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
          (satisfies? core/IElementType type)
          (let [e (core/create-element-node type document)]
            (set-props! e props document options)
            (core/element-node-was-created! type e)
            ;; Note: and/or wait for the async success? ("mounted")
            e)

          (satisfies? core/IIndirectionType type)
          (recur (core/expand-indirection type (core/ve-props vdom))
                 options)

          (satisfies? core/IForeignType type)
          (core/foreign-type-create type props options)

          :else
          (throw (ex-info (str "Unsupport velement type: " (pr-str type) ".") {}))))
      (core/with-context-update? vdom)
      (recur (:content vdom) ((:update-options vdom) options))
      :else
      (dom/create-text-node document (to-text vdom)))))

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
            (satisfies? core/IElementType type)
            (let [old-props (core/ve-props old-vdom)
                  new-props (core/ve-props new-vdom)]
              (core/element-node-will-be-updated! type node old-props new-props)
              (patch-properties! node
                                 old-props new-props
                                 document
                                 options)
              (core/element-node-was-updated! type node new-props)
              nil)
        
            (satisfies? core/IIndirectionType type)
            (recur (core/expand-indirection type old-props) (core/expand-indirection type new-props)
                   options)

            (satisfies? core/IForeignType type)
            (do (core/foreign-type-patch! type node old-props new-props options)
                nil)
          
            :else
            (throw (ex-info (str "Unsupport velement type: " (pr-str type) ".") {})))))

      (core/with-context-update? old-vdom)
      (do
        (assert (core/with-context-update? new-vdom))
        (assert (= (:update-options old-vdom) (:update-options new-vdom)))
        (recur (:content old-vdom) (:content new-vdom)
               ((:update-options new-vdom) options)))
      
      :else
      ;; update text of a textnode to new-vdom (a string or anything else)
      (do
        (when-not (dom/text-node? node)
          (throw (ex-info (str "Actual node is not a text node, where the previous vdom is: " (pr-str old-vdom) ", " node ".") {})))
        (dom/set-text-node-value! node (to-text new-vdom))
        nil))))

(defn ^:no-doc remove-child! [options element node vdom]
  (when-let [type (and (core/velement? vdom)
                       (core/ve-type vdom))]
    (cond
      (satisfies? core/IElementType type)
      (core/element-node-will-be-removed! type node)
      (satisfies? core/IForeignType type)
      (core/foreign-type-destroy! type node (core/ve-props vdom) options)))
  (dom/remove-child! element node))

(defn ^:no-doc insert-child! [document options element vdom ref-node]
  (let [node (create-child document vdom options)]
    (dom/insert-before! element node ref-node)))

(defn ^:no-doc append-child! [document options element vdom]
  (let [node (create-child document vdom options)]
    (dom/append-child! element node)))

(defn ^:no-doc similar-velement?
  [e1 e2]
  (= (core/ve-type e1) (core/ve-type e2)))

(defn ^:no-doc similar-vdom?
  "Aka 'updateable' element"
  [vdom1 vdom2]
  (cond
    (core/velement? vdom1)
    (and (core/velement? vdom2)
         (similar-velement? vdom1 vdom2))
    (core/with-context-update? vdom1)
    (and (core/with-context-update? vdom2)
         (= (:update-options vdom1) (:update-options vdom2)))
    :else
    (not (core/velement? vdom2))))

(def indices
  (let [N 1000
        reservoir (vec (range N))]
    (fn [n]
      (if (<= n N)
        (subvec reservoir 0 n)
        (vec (range n))))))

(defn ^:no-doc patch-children! [element old-vdoms new-vdoms document options]
  (if (identical? old-vdoms new-vdoms) ;; ..cheap shortcut
    nil
    (let [nodes (dom/child-nodes element)
          olds (util/vec?! old-vdoms)
          news (util/vec?! new-vdoms)]
      (when (not= (count nodes) (count olds))
        (throw (ex-info (str "Actual dom child nodes do not match the number of vdom elements: " (pr-str olds) ", " nodes ".") {})))
      (util/diff-patch nil
                       (indices (count olds))
                       (indices (count news))

                       ;; if true, alter-child! is used
                       (fn similar-child? [old-i new-i] (similar-vdom? (nth olds old-i) (nth news new-i)))
                       ;; if true, nothing is done
                       (fn equal-child? [old-i new-i] (= (nth olds old-i) (nth news new-i)))

                       (fn [_ new-i] (append-child! document options element (nth news new-i)))
                       (fn [_ new-i old-i] (insert-child! document options element (nth news new-i) (nth nodes old-i)))
                       (fn [_ old-i new-i] (alter-child! document options (nth nodes old-i) (nth olds old-i) (nth news new-i)))
                       (fn [_ old-i] (remove-child! options element (nth nodes old-i) (nth olds old-i))))
      (assert (= (count (dom/child-nodes element)) (count news)) (str "Internal error; new vdoms:" (pr-str news)))
      nil)))
