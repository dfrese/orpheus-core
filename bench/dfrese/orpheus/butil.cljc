(ns dfrese.orpheus.butil
  #?(:cljs (:require [cljs.test :refer-macros [deftest is testing]])))

#_(:clj [[clojure.test :refer [deftest is testing]]])

(defn abs [v]
  (if (< v 0) (- v) v))

(defn average [l]
  (/ (reduce + l) (count l)))

(defn stime [v]
  (str v " ms"))

(defn measure-time [f]
  #?(:cljs (let [st (.now js/performance)]
             (doall (f))
             (let [en (.now js/performance)]
               (- en st)))))

(defn bench [n f]
  (f) ;; warm up
  (let [R 3
        times (mapv #(measure-time f)
                    (range 0 R))
        avg (average times)
        dig (average (map #(* 100 (/ (abs (- avg %))
                                     avg))
                          times))]
    (println (str (name n) ":")
             (stime avg) (str "(+/- " (int dig) "%)"))))

#?(:clj
  (defmacro defbench [name & body]
    `(cljs.test/deftest ~name
       (bench '~name (fn [] ~@body))
       (cljs.test/is true))))
