(ns clojure-py.pointer-dict-tests
  (:use clojure.test
        clojure-py.nexpr-compiler
        clojure.pprint)
  (:require [clojure-py.llvmc :as llvmc]
            [clojure-py.system.integer :as i]
            [clojure-py.system.pointer-dict :as p]
            [clojure-py.constructors :refer :all]
            [clojure.pprint :refer [pprint]]))

(defn dbg [p]
  (pprint p)
  p)

(def main-fn-t (c-fn-t [:int :i8**] :int))

(deftest can-create-and-add
  (is (= (-> (c-module '[clojure-py.system.pointer-dict
                         clojure-py.object]
                       (c-fn "main" main-fn-t [argc argv]
                             (-> (p/new)
                                 (p/add-item (c-bitcast 42 :i8*)
                                             (c-bitcast 42 :i8*))
                                 (p/-count))
                             
                    ))
             #_(dbg)
             (llvmc/compile-as-exe)
             (llvmc/run-exe)
             :exit)
         42)))
