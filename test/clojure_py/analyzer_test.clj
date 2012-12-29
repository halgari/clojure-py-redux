(ns clojure-py.analyzer-test
  (:use clojure.test
        clojure-py.alyzer))


(def forms
  '[(+ 1 2)])

(defn debug [x]
  (println (pr-str x))
  x)
(defn make-test [form]
  (debug `(testing ~(str "can analyze " form)
           (is (analyze (list 'quote ~form))))))

(defmacro emit-forms []
  (list* 'do
         (for [form forms]
           (make-test form))))

(deftest analyze-exprs
  (emit-forms))