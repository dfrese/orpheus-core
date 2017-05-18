(ns dfrese.orpheus.patch.base
  "Functions to apply a virtual dom to a real dom."
  (:require [dfrese.orpheus.core :as core]
            [dfrese.orpheus.types.element :as element]
            dfrese.orpheus.html dfrese.orpheus.math dfrese.orpheus.svg ;; make sure multi-methods are loaded
            [dfrese.orpheus.types.indirection :as indirection]
            [dfrese.orpheus.types.foreign :as foreign]
            [dfrese.orpheus.types :as types]
            [dfrese.orpheus.patch.util :as util]
            [dfrese.orpheus.patch.indices :as i]
            [dfrese.edomus.core :as dom]
            [clojure.string :as str]))

(declare patch-children!)
(declare patch-properties!)
(declare init-children!)

;; properties

(defn ^:no-doc patch-property! [state element n old-v new-v document options]
  (if (identical? old-v new-v) ;; doing a = is probably not worth it
    state
    (let [n (name n)]
      (case n
        "childNodes" (patch-children! state element old-v new-v document options)
        (do (when (not= old-v new-v)
              (element/patch-property! element n old-v new-v options))
            state)))))

(defn ^:no-doc init-property! [state element n value document options]
  ;; Note: value can always be nil; meaning to remove the property 'as much as possible'.
  (let [n (name n)]
    (case n
      "childNodes" (init-children! state element value document options)
      (do (element/init-property! element n value options)
          state))))

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
      (types/velement? vdom)
      (let [type (types/ve-type vdom)
            props (types/ve-props vdom)]
        (cond
          (element/element-type? type)
          (let [e (element/create-element-node document type)]
            [(set-props! [] e props document options) e])

          (indirection/indirection-type? type)
          (let [expanded (indirection/expand-indirection type props)
                [sub-state node] (create-child document expanded options)]
            [(IndirectionState. expanded sub-state) node])

          (foreign/foreign-type? type)
          (let [res (foreign/foreign-type-create type document props options)]
            (assert (sequential? res))
            (assert (= 2 (count res)))
            (assert (or (dom/text-node? (second res))
                        (dom/element? (second res))))
            res)

          :else
          (throw (ex-info (str "Unsupport velement type: " (pr-str type) ".") {:type type}))))
      (types/with-context-update? vdom)
      (recur (types/with-context-update-content vdom) ((types/with-context-update-f vdom) options))

      (string? vdom) ;; calling str would hide a lot of errors; and the user should explicitly do it.
      [nil (dom/create-text-node document vdom)]
      
      :else
      (throw (ex-info (str "Unsupported vdom element: " (pr-str vdom) ".") {:value vdom})))))

;; ----

(defn ^:no-doc alter-child! [state document options node old-vdom new-vdom]
  (if (identical? old-vdom new-vdom)
    state
    (loop [old-vdom old-vdom
           new-vdom new-vdom
           options options]
      (cond
        (types/velement? old-vdom)
        ;; update an element of same type.
        (do
          (assert (types/velement? new-vdom))
          (let [type (types/ve-type old-vdom)
                old-props (types/ve-props old-vdom)
                new-props (types/ve-props new-vdom)]
            (when-not (dom/element? node)
              (throw (ex-info (str "Actual node is not an element, where the previous vdom is: " (pr-str old-vdom) ", " node ".") {})))
            (assert (= type (types/ve-type new-vdom))) ;; impl error
            (cond
              (element/element-type? type)
              (patch-properties! state node
                                 old-props new-props
                                 document
                                 options)
        
              (indirection/indirection-type? type)
              (let [state ^IndirectionState state]
                (assert (instance? IndirectionState state))
                (assert (= (.-expanded state) (indirection/expand-indirection type old-props)))
                (let [new-vdom (indirection/expand-indirection type new-props)]
                  (IndirectionState. new-vdom
                                     (alter-child! (.-sub-state state) document options node (.-expanded state) new-vdom))))

              (foreign/foreign-type? type)
              (foreign/foreign-type-patch! type state node old-props new-props options)
          
              :else
              (throw (ex-info (str "Unsupport velement type: " (pr-str type) ".") {})))))

        (types/with-context-update? old-vdom)
        (do
          (assert (types/with-context-update? new-vdom))
          (assert (= (types/with-context-update-f old-vdom) (types/with-context-update-f new-vdom))) ;; impl error
          (recur (types/with-context-update-content old-vdom) (types/with-context-update-content new-vdom)
                 ((types/with-context-update-f new-vdom) options)))
      
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
    (types/velement? vdom)
    (let [type (types/ve-type vdom)
          props (types/ve-props vdom)]
      (cond
        (element/element-type? type)
        (let [vdoms (or (get props "childNodes")
                        (get props :childNodes))]
          (assert (vector? state)) ;; one state for each child.
          (reduce (fn [i vdom]
                    (destroy-node! (get state i) options (dom/get-child node i) vdom)
                    (inc i))
                  0
                  vdoms))
        
        (indirection/indirection-type? type)
        (let [state ^IndirectionState state]
          (assert (instance? IndirectionState state))
          (assert (= (.-expanded state) (indirection/expand-indirection type props)))
          (destroy-node! (.-sub-state state) options node (.-expanded state)))
        
        (foreign/foreign-type? type)
        (foreign/foreign-type-destroy! type state node props options)))
    (types/with-context-update? vdom)
    (recur state ((types/with-context-update-f vdom) options) node (types/with-context-update-content vdom))))

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
    
    (types/velement? vdom1)
    (and (types/velement? vdom2)
         (identical? (types/ve-type vdom1) (types/ve-type vdom2)))

    (types/with-context-update? vdom1)
    (and (types/with-context-update? vdom2)
         ;; if the function changes, we recreate the tree (it might be bound in event-handlers)
         (= (types/with-context-update-f vdom1) (types/with-context-update-f vdom2)))

    (string? vdom1) (string? vdom2)
    
    :else
    (throw (ex-info (str "Unsupported vdom element: " (pr-str vdom1) ".") {:value vdom1}))))

(defn- node-name [node]
  ;; TODO -> edomus?
  #?(:clj node)
  #?(:cljs (.-nodeName node)))

(defn- child-nodes-array [element]
  #?(:clj (to-array (dom/child-nodes element)))
  #?(:cljs (let [n (dom/child-nodes-count element)
                 r (make-array n)]
             (dotimes [i n]
               (aset r i (dom/get-child element i)))
             r)))

(defn- ^:no-doc vdom-key [vdom]
  (when (types/velement? vdom)
    (types/ve-key vdom)))

(defn ^:no-doc patch-children-v1 [state element old-vdoms new-vdoms document options]
  (assert (vector? state))
  (let [olds (to-array old-vdoms)
        ;; Note: this nodes list must not change during the patch
        ^objects nodes (child-nodes-array element)
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
