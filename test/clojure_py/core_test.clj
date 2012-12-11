(ns clojure-py.core-test
  (:use clojure.test)
  (:require [clojure-py.core :as cpy]))

(deftest can-analyze-an-expr
  (is (cpy/analyze '(+ 1 2))))

(deftest can-compile-an-expr
  (is (cpy/compile '(+ 1 2))))

(deftest can-eval-an-expr
  (is (= (cpy/run '(+ 1 2))
         3)))

