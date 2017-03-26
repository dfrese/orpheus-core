(ns dfrese.orpheus.patch-bench
  (:require [dfrese.orpheus.butil :refer-macros [defbench] :refer [bench]]
            [cljs.test :refer-macros [deftest is testing]]
            [dfrese.orpheus.core :as core :include-macros true]
            [dfrese.orpheus.patch :as patch]
            [dfrese.orpheus.html :as html]
            [dfrese.orpheus.test-utils :as t]))

;; Note: run benchmarks in browser:
;; doo.runner.run_BANG_()

(defn random-props [n]
  (t/generate t/props n 92739872))

(defn render-seq [state node s]
  (reduce (fn [st vdom]
            (patch/patch! st node vdom))
          state
          s))

(defn bench-seq [s]
  (let [[node state] (t/prepare!)]
    (render-seq state node s)))

(def rand-60 (random-props 60))
(def rand-30 (random-props 30))
(def rand-20 (random-props 20))

(defbench no-change-identical-bench
  (let [N 100000]
    (bench-seq (repeat N rand-60))))

(def no-change-seq
  (let [N 3000
        e #(random-props 30)]
    (mapv e (range 0 N))))
(defbench no-change-equal-bench
  (bench-seq no-change-seq))

(defbench patch-random-bench
  (let [N 1000]
    (bench-seq (interleave (repeat N rand-30) (repeat N rand-20)))))

(def long-l {"childNodes" (vec (repeat 100 (html/div)))})
(def empty-l {"childNodes" []})

(defbench create-rows-bench
  (let [N 2000]
    (bench-seq (interleave (repeat N long-l) (repeat N empty-l)))))

(def divs {"childNodes" (repeat 1000 (html/div))})
(def spans {"childNodes" (repeat 1000 (html/span))})

(defbench replace-rows-bench
  (let [N 30]
    (bench-seq (interleave (repeat N divs) (repeat N spans)))))

(def part-1 {"childNodes" (vec (repeat 1000 (html/div "A")))})
(def part-2 {"childNodes" (vec (apply concat (repeat 100 (concat (repeat 9 (html/div "A"))
                                                                 [(html/div "B")]))))})

(defbench partial-update-bench
  (let [N 20]
    (bench-seq (interleave (repeat N part-1) (repeat N part-2)))))

(def unselected (html/div {:style {"background-color" "transparent"}}))
(def selected (html/div {:style {"background-color" "grey"}}))
(def sel-1 {"childNodes" (vec (repeat 1000 unselected))})
(def sel-2 {"childNodes" (vec (concat (repeat (dec 499) unselected)
                                      [selected]
                                      (repeat 500 unselected)))})

(defbench select-row-bench
  (let [N 25]
    (bench-seq (interleave (repeat N sel-1) (repeat N sel-2)))))

;; swap-rows
;; remove-row
;; create-many-rows
;; append-rows-to-large-table
;; clear-rows

#_(defbench move-from-start-to-end
  )
