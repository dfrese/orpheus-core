(ns dfrese.orpheus.patch-bench
  (:require [dfrese.orpheus.butil :refer-macros [defbench] :refer [bench]]
            [cljs.test :refer-macros [deftest is testing]]
            [dfrese.orpheus.core :as core :include-macros true]
            [dfrese.orpheus.patch :as patch]
            [dfrese.orpheus.html :as html]
            [dfrese.orpheus.test-utils :as t]))

(defn random-props [n]
  (t/generate t/props n 92739872))

(defn render-seq [state node s]
  (reduce (fn [st vdom]
            (patch/patch-properties! node st vdom))
          state
          s))

(defn bench-seq [s]
  (let [[node state] (t/prepare!)]
    (render-seq state node s)))

(defbench patch-random-bench
  (let [N 1000

        e1 (random-props 30)
        e2 (random-props 20)]
    ;; a3be45c90e43680246622ab7a62707418901315a: 2267.5000132   ~2ms per patch
    ;; no lifecycle: 2149.7340216 ms  -5%
    ;; new diff-path: ~1100 ms
    (bench-seq (interleave (repeat N e1) (repeat N e2)))))

(defbench create-rows-bench
  (let [N 50 ;; repetitions
        M 1000 ;; size
        long {"childNodes" (vec (repeat M (html/div)))}
        empty {"childNodes" []}]
    (bench-seq (interleave (repeat N long) (repeat N empty)))))

(defbench replace-rows-bench
  (let [N 20 ;; repetitions
        M 1000 ;; size
        divs {"childNodes" (repeat M (html/div))}
        spans {"childNodes" (repeat M (html/span))}]
    (bench-seq (interleave (repeat N divs) (repeat N spans)))))

(defbench partial-update-bench
  (let [N 20    ;; repetitions
        M 1000 ;; size
        l1 {"childNodes" (repeat M (html/div "A"))}
        l2 {"childNodes" (apply concat (repeat (/ M 10) (concat (repeat 9 (html/div "A"))
                                                                [(html/div "B")])))}]
    (bench-seq (interleave (repeat N l1) (repeat N l2)))))

(defbench select-row-bench
  (let [N 25 ;; repetitions
        M 1000 ;; size

        unselected (html/div {:style {"background-color" "transparent"}})
        selected (html/div {:style {"background-color" "grey"}})
        
        l1 {"childNodes" (repeat M unselected)}
        l2 {"childNodes" (concat (repeat (dec (/ M 2)) unselected)
                                 [selected]
                                 (repeat (/ M 2) unselected))}]
    (bench-seq (interleave (repeat N l1) (repeat N l2)))))

;; swap-rows
;; remove-row
;; create-many-rows
;; append-rows-to-large-table
;; clear-rows

#_(defbench move-from-start-to-end
  )
