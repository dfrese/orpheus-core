(ns dfrese.orpheus.patch.base
  "Functions to apply a virtual dom to a real dom."
  (:require [dfrese.orpheus.core :as core]
            [dfrese.orpheus.patch.util :as util]
            [dfrese.orpheus.patch.reservoirs :as r]
            [dfrese.orpheus.patch.indices :as i]
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

(defn ^:no-doc set-simple-property! [state element name value options]
  (if-let [etype (and (or (nil? value) ;; nil for removal :-/ TODO: look at old-v if it's an handler instead?
                          (ifn? value))
                      (event-type? name))]
    (do
      ;; set event handlers as a side effect, unfortunately; but the
      ;; properties 'onclick' etc., cannot be called properly, for
      ;; custom event triggering (at least I did not find out). Only
      ;; the 'addEventListener' handlers can be called properly via
      ;; dispatchEvent - so we map them here.
      (if (nil? value)
        (dom-event/unset-event-handler! element etype)
        (let [f (core/create-js-event-handler value (:dispatch! options))]
          (dom-event/set-event-handler! element etype f)))
      nil)
    ;; any other prop
    (dom/set-property! element name value))
  state)

(defn ^:no-doc patch-style! [state element old-v new-v]
  (util/patch-map-simple nil old-v new-v
                         #(dom/remove-style! element %2)
                         #(dom/set-style! element %2 %3))
  state)

(defn ^:no-doc init-style! [state element v]
  (reduce-kv #(dom/set-style! element %2 %3)
             nil
             v)
  state)

(defn- set!? [v]
  (if (set? v) v (set v)))

(defn ^:no-doc patch-classes! [state element old-v new-v]
  (let [o (set!? old-v) ;; should be sets already for optimal performance
        n (set!? new-v)]
    (doseq [c (set/difference o n)]
      (dom/remove-class! element c))
    (doseq [c (set/difference n o)]
      (dom/add-class! element c)))
  state)

(defn ^:no-doc init-classes! [state element v]
  (dom/set-classes! element v)
  state)

(defn ^:no-doc patch-attributes! [state element old-v new-v]
  (util/patch-map-simple nil old-v new-v
                         #(dom/remove-attribute! element %2)
                         #(dom/set-attribute! element %2 %3))
  state)

(defn ^:no-doc init-attributes! [state element v]
  (reduce-kv #(dom/set-attribute! element %2 %3)
             nil
             v)
  state)

(defn ^:no-doc patch-property! [state element n old-v new-v document options]
  (if (identical? old-v new-v) ;; doing a = is probably not worth it
    nil
    (let [n (name n)]
      ;; TODO: make this extensible; e.g. :points of svg polyline elements. (so per element type?)
      (case n
        "childNodes" (patch-children! state element old-v new-v document options)
        "style" (patch-style! state element old-v new-v)
        "attributes" (patch-attributes! state element old-v new-v)
        "classList" (patch-classes! state element old-v new-v)
        (if (not= old-v new-v) ;; need to because of IConvertible
          (set-simple-property! state element n new-v options)
          state)))))

(defn ^:no-doc init-property! [state element n value document options]
  ;; Note: value can always be nil; meaning to remove the property 'as much as possible'.
  (let [n (name n)]
    (case n
      "childNodes" (init-children! state element value document options)
      "style" (init-style! state element value)
      "attributes" (init-attributes! state element value)
      "classList" (init-classes! state element (set value))
      (set-simple-property! state element n value options))))

(defn ^:no-doc remove-property! [state element n]
  ;; to actively remove a property that was set before (in constrast to leaving it totally unchanged):
  (let [options nil ;; setting to nil should go without options
        document nil
        n (name n)]
    (case n
      "childNodes" (do ;; we actually need to do both - TODO explain why
                     (dom/clear-child-nodes! element)
                     (init-property! state element "childNodes" [] document options))
      (init-property! state element n nil document options))))

(defn ^:no-doc patch-properties! [state element old-props new-props document options]
  (util/patch-map state
                  old-props
                  new-props
                  #(remove-property! %1 element %2)
                  #(init-property! %1 element %2 %3 document options)
                  #(patch-property! %1 element %2 %3 %4 document options)))

;; children

(defn ^:no-doc set-props! ;; need not be async..
  [state element props document options]
  (reduce-kv (fn [state k v]
               (init-property! state element k v document options))
             state
             props))

(defrecord IndirectionState [expanded sub-state])
;; other state types:
;; element: vector of children states
;; foreign-type: it's own state
;; with-context-update: none (state of content)
;; strings: nil

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
            [(set-props! [] e props document options) e])

          (core/indirection-type? type)
          (let [expanded (core/expand-indirection type (core/ve-props vdom))
                [sub-state node] (create-child document expanded options)]
            [(IndirectionState. expanded sub-state) node])

          (core/foreign-type? type)
          (let [res (core/foreign-type-create type document props options)]
            (assert (sequential? res))
            (assert (= 2 (count res)))
            (assert (or (dom/text-node? (second res))
                        (dom/element? (second res))))
            res)

          :else
          (throw (ex-info (str "Unsupport velement type: " (pr-str type) ".") {:type type}))))
      (core/with-context-update? vdom)
      (recur (:content vdom) ((:update-options vdom) options))

      (string? vdom) ;; calling str would hide a lot of errors; and the user should explicitly do it.
      [nil (dom/create-text-node document vdom)]
      
      :else
      (throw (ex-info (str "Unsupported vdom element: " (pr-str vdom) ".") {:value vdom})))))

;; ----

(defn ^:no-doc alter-child! [state document options node old-vdom new-vdom]
  (when-not (identical? old-vdom new-vdom)
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
                (patch-properties! state node
                                   old-props new-props
                                   document
                                   options))
        
              (core/indirection-type? type)
              (do
                (assert (instance? IndirectionState state))
                (assert (= (.-expanded state) (core/expand-indirection type old-props)))
                (let [new-vdom (core/expand-indirection type new-props)]
                  (IndirectionState. new-vdom
                                     (alter-child! (.-sub-state state) document options node (.-expanded state) new-vdom))))

              (core/foreign-type? type)
              (core/foreign-type-patch! type state node old-props new-props options)
          
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
          state)
      
        :else
        (throw (ex-info (str "Unsupported vdom element: " (pr-str old-vdom) ".") {:value old-vdom}))))))

(defn ^:no-doc destroy-node! [state options node vdom]
  (cond
    (core/velement? vdom)
    (let [type (core/ve-type vdom)
          props (core/ve-props vdom)]
      (cond
        (core/element-type? type)
        (let [vdoms (or (get props "childNodes")
                        (get props :childNodes))]
          (assert (vector? state)) ;; one state for each child.
          (reduce (fn [i vdom]
                    (destroy-node! (get state i) options (dom/get-child node i) vdom)
                    (inc i))
                  0
                  vdoms))
        (core/indirection-type? type)
        (do
          (assert (instance? IndirectionState state))
          (assert (= (.-expanded state) (core/expand-indirection type props)))
          (destroy-node! (.-sub-state state) options node (.-expanded state)))
        
        (core/foreign-type? type)
        (core/foreign-type-destroy! type state node props options)))
    (core/with-context-update? vdom) ;; FIXME: update options.
    (destroy-node! state options node (:content vdom))))

(defn ^:no-doc init-children! [state element vdoms document options]
  (let [res (map #(create-child document % options)
                 vdoms)]
    (doseq [c (map second res)]
      (dom/append-child! element c))
    ;; total state is the vector of the child states:
    (mapv first res)))

(defn ^:no-doc similar-vdom?
  "Aka 'updateable' element"
  [vdom1 vdom2]
  (cond
    (identical? vdom1 vdom2) true
    
    (core/velement? vdom1)
    (and (core/velement? vdom2)
         (identical? (core/ve-type vdom1) (core/ve-type vdom2)))

    (core/with-context-update? vdom1)
    (and (core/with-context-update? vdom2)
         ;; if the context-change changes, we recreate the tree (it might be bound in event-handlers)
         (= (:update-options vdom1) (:update-options vdom2)))

    (string? vdom1) (string? vdom2)
    
    :else
    (throw (ex-info (str "Unsupported vdom element: " (pr-str vdom1) ".") {:value vdom1}))))

(defn- node-name [node]
  ;; TODO -> edomus?
  #?(:clj node)
  #?(:cljs (.-nodeName node)))

#_(defn ^:no-doc remove-child [element node]
  (dom/remove-child! element node)
  element)

#_(defn ^:no-doc append-child [element node]
  (dom/append-child! element node)
  element)

#_(defn ^:no-doc insert-child [element node before]
  (dom/insert-before! element node before)
  element)

(defn- ^:no-doc vdom-key [vdom]
  (when (core/velement? vdom)
    (core/ve-key vdom)))

(defn- vdom-type [vdom]
  (cond
    (core/velement? vdom) (core/ve-type vdom)
    (core/with-context-update? vdom) (:update-options vdom)
    :else ::text))

#_(defn ^:no-doc patch-children-v3 [element old-vdoms new-vdoms document options]
  (let [nodes (util/vec!? (dom/child-nodes element))
        olds (util/vec!? old-vdoms)
        type-reservoir (r/type-reservoir-init (mapv vdom-type olds))]
    (util/fold-diff-patch-keyed element
                                append-child
                                (fn remove-old [element nodei]
                                  (remove-child element (nodes nodei)))
                                (fn patch [element nodei new-vdom]
                                  (let [old-vdom (olds nodei)]
                                    (alter-child! document options (nodes nodei) old-vdom new-vdom))
                                  element)
                                (i/indices (count nodes))
                                new-vdoms
                                (fn patchable? [nodei new-vdom]
                                  (similar-vdom? (olds nodei) new-vdom))
                                (fn old-key [nodei]
                                  (let [vdom (olds nodei)]
                                    (vdom-key vdom)))
                                vdom-key
                                (fn create [vdom]
                                  ;; Note: might be worth it to extend this on a 'most suitable' node?
                                  (if-let [nodei (r/type-reservoir-pull! type-reservoir (vdom-type vdom))]
                                    (let [node (nodes nodei)]
                                      (alter-child! document options node (olds nodei) vdom)
                                      node)
                                    (create-child document vdom options)))
                                (fn destroy! [nodei resurrectable?]
                                  (if resurrectable?
                                    (r/type-reservoir-push! type-reservoir nodei)
                                    (destroy-node! options (nodes nodei) (olds nodei))))
                                (fn resurrect [nodei]
                                  (nodes nodei)))
    (doseq [nodei (r/type-reservoir-seq type-reservoir)]
      (destroy-node! options (nodes nodei) (olds nodei)))))

(defn- child-nodes-array [element]
  #?(:clj (to-array (dom/child-nodes element)))
  #?(:cljs (let [n (dom/child-nodes-count element)
                 r (make-array n)]
             (dotimes [i n]
               (aset r i (dom/get-child element i)))
             r)))

(defn ^:no-doc patch-children-v1 [state element old-vdoms new-vdoms document options]
  (assert (vector? state))
  (let [olds (to-array old-vdoms)
        ;; Note: this nodes list must not change during the patch
        nodes (child-nodes-array element)
        old-states state]
    ;; Note: there's a guarantee that all final nodes are either
    ;; appended, inserted or patched in-order; so we can sequentially
    ;; build the new state up.
    (util/fold-diff-patch-v1 []
                             (fn append [state new]
                               (let [[st n] (create-child document new options)]
                                 (dom/append-child! element n)
                                 (conj state st)))
                             (fn remove [state idx]
                               (let [node (aget nodes idx)
                                     old (aget olds idx)]
                                 (dom/remove-child! element node)
                                 (destroy-node! (get old-states idx) options node old)
                                 state))
                             (fn insert [state new before-idx]
                               (let [before-node (aget nodes before-idx)
                                     [st n] (create-child document new options)]
                                 (dom/insert-before! element n before-node)
                                 (conj state st)))
                             (fn patch [state idx new]
                               (let [node (aget nodes idx)
                                     old (aget olds idx)]
                                 (conj state
                                       (alter-child! (get old-states idx) document options node old new))))
                             (count olds)
                             new-vdoms
                             (fn patchable? [idx new]
                               ;; Only patch, if keys are equal (or both have none), and types are patchable.
                               ;; TODO: integrate key-check with similar-vdom? for optimization.
                               (let [old (aget olds idx)]
                                 (and (= (vdom-key old) (vdom-key new))
                                      (similar-vdom? old new))))
                             (fn precious? [idx]
                               ;; try not to remove, if keyed (TODO or focused!?)
                               (let [old (aget olds idx)]
                                 (some? (vdom-key old)))))))


(defn ^:no-doc patch-children! [state element old-vdoms new-vdoms document options]
  (if (identical? old-vdoms new-vdoms) ;; ..cheap shortcut
    state
    (do
      (assert (= (dom/child-nodes-count element) (count old-vdoms))
              (str "Actual dom child nodes do not match the number of vdom elements: " (pr-str old-vdoms) " /= "
                   (pr-str (map node-name (dom/child-nodes element))) "."))
      (let [state (patch-children-v1 state element old-vdoms new-vdoms document options)]
        (assert (= (dom/child-nodes-count element) (count state)))
        (assert (= (dom/child-nodes-count element) (count new-vdoms)) (str "Internal error; new vdoms:" (pr-str new-vdoms)))
        state))))
