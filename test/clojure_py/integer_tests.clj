(ns clojure-py.integer-tests
  (:use clojure.test
        clojure-py.nexpr-compiler
        clojure.pprint)
  (:require [clojure-py.llvmc :as llvmc]
            [clojure-py.system.integer :as i]
            [clojure-py.constructors :refer :all]
            [clojure.pprint :refer [pprint]]))

(defn dbg [p]
  (pprint p)
  p)

(def main-fn-t (c-fn-t [:int :i8**] :int))

(deftest basic-integers
  (is (= (-> (c-module '[clojure-py.system.integer
                         clojure-py.object]
              (c-fn "main" main-fn-t [argc argv]
                    (c-call i/unwrap-int (c-call i/from-int (const-int 42)))))
             (dbg)
             (llvmc/compile-as-exe)
             (llvmc/run-exe)
             :exit)
         42)))




